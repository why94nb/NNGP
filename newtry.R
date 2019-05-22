library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)


set.seed(1)

rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

#exponential
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
sigma.sq <- 5
tau.sq <- 1
phi <- 10
D <- as.matrix(dist(coords))
R <- exp(-phi*D)
w <- rmvn(1, rep(0,n), sigma.sq*R)
y <- rnorm(n, w, sqrt(tau.sq))
ho <- sample(1:n, n/5)
y.ho <- y[ho]
w.ho <- w[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
w <- w[-ho,,drop=FALSE]
coords <- coords[-ho,]
y <- exp(y) / 10
y.ho <- exp(y.ho) / 10


#matern(works)
set.seed(1)
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
sigma.sq <- 4
phi <- 0.2
nu <- 0.5
tau.sq <- 0
y <- fast_Gp_sim(c(4,0.2,0.5,0), "matern_isotropic", coords, 30 )
ho <- sample(1:n, n/5)
y.ho <- y[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
coords <- coords[-ho,]
X <- as.matrix( rep(1,length(y)) )
y <- exp(y) / 10
y.ho <- exp(y.ho) / 10





#gpgp, used to make kriging predictions as input to neural nets 
X <- as.matrix( rep(1,length(y)))
fit <- fit_model(y, coords, X, "matern_isotropic")
X_pred <- as.matrix(rep(1,length(y.ho)))
y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                      coords.ho, X, X_pred, fit$beta, reorder = TRUE)
yinput <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                      coords, X, X, fit$beta, reorder = TRUE)
mse1 <- mean((y.ho - y_pred)^2) #this should be close to NNGP

##nngp
nngp <- function(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  #return prediction mse, predictions on both training dataset and testing dataset, and No. of neighbors
  starting <- list("phi"=phi, "sigma.sq"=sigma.sq, "tau.sq"=tau.sq)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(1, 5), "tau.sq.IG"=c(2, 1))
  cov.model <- "exponential"
  n.report <- n.samples
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = F)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1) , coords.0 = coords, n.omp.threads=2, verbose = F)
  list("mse" = mse, "prediction" = p.r, "m" = m, "train"=pp)
}

nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 2000,m=10)

pp <- nn$train
p.r <- nn$prediction
yinput <- apply(pp$p.y.0, 1, mean)
y_pred <- apply(p.r$p.y.0, 1, mean)
#kriging + coords
ymean <- mean(y)
ysd <- sd(y)
y_norm <- (y - ymean) / ysd
yho_norm <- (y.ho - ymean) / ysd


inputmean <- mean(cbind(yinput,coords))
inputsd <- sd(cbind(yinput,coords))
input <- (cbind(yinput,coords) - inputmean) / inputsd
input_pre <- (cbind(y_pred,coords.ho) - inputmean) / inputsd


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 200, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.1) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'relu') %>%
  # layer_dropout(rate = 0.5) %>%
  layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('mse')
)

# Training & Evaluation ----------------------------------------------------
callbacks <- list(callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 5,
                                    verbose = 0, mode = "min", epsilon = 1e-04, cooldown = 0,
                                    min_lr = 0),
                  callback_early_stopping(monitor = "val_loss", min_delta = 0.0005, patience = 10,
                                verbose = 0, mode = "min"))
# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 32,
  epochs = 100,
  verbose = 0,
  validation_data = list(input_pre, yho_norm),
  callbacks = callbacks
)

plot(history)

score <- model %>% evaluate(
  input_pre, yho_norm,
  verbose = 0
)

y_pr <- model %>% predict(input_pre)
y_pr <- y_pr * ysd + ymean
mse2 <- mean((y.ho - y_pr)^2)
print(list("quadN" = mse2, "nngp" = nn$mse, "gpgp" = mse1))