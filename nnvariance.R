library(keras)
library(spNNGP)

rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}
##Make some data
set.seed(1)
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
x <- cbind(1, rnorm(n))
B <- as.matrix(c(1,5))
sigma.sq <- 5
tau.sq <- 1
phi <- 3/0.5
D <- as.matrix(dist(coords))
R <- exp(-phi*D)
w <- rmvn(1, rep(0,n), sigma.sq*R)
y <- rnorm(n, x%*%B + w, sqrt(tau.sq))
ho <- sample(1:n, n/5)
y.ho <- y[ho]
x.ho <- x[ho,,drop=FALSE]
w.ho <- w[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
x <- x[-ho,,drop=FALSE]
w <- w[-ho,,drop=FALSE]
coords <- coords[-ho,]


n.samples <- 10000
starting <- list("phi"=phi, "sigma.sq"=3, "tau.sq"=0.5)
tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(2, 5), "tau.sq.IG"=c(2, 1))
cov.model <- "exponential"
n.report <- 10000
m <- 5
##Predict for holdout set using both models

m.r <- spNNGP(y~x-1, coords=coords, starting=starting, method="response", n.neighbors=m,
              tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
              n.samples=n.samples, n.omp.threads=2, n.report=n.report)
##Prediction for holdout data

p.r <- spPredict(m.r, X.0 = x.ho, coords.0 = coords.ho, n.omp.threads=2)
plot(apply(p.r$p.y.0, 1, mean), y.ho)
points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")

mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
covhat <- colMeans(m.r$p.theta.samples[1000:10000,])
cl <- apply(p.r$p.y.0, 1, function(x) quantile(x,0.025))
cu <- apply(p.r$p.y.0, 1, function(x) quantile(x,0.975))

ypred_train <- model %>% predict(input)
r <- y - ypred_train
logr_train <- log(r^2)
ypred <- model %>% predict(input_pre)
r_pred <- y.ho - ypred
logr_pred <- log(r_pred^2)

K <- backend()
deviation_loss <- function(y_true,y_pred){
    return (K$mean(y_pred + (y_true - y_pred)*K$exp(-y_pred)))  
}

model_residual <- keras_model_sequential()
model_residual %>% 
  layer_dense(units = 400, activation = 'relu', input_shape = c(1)) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 200, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)

summary(model_residual)

model_residual %>% compile(
  loss = deviation_loss,
  optimizer = optimizer_rmsprop(),
  metrics = c('mse','deviation_loss' = deviation_loss)
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model_residual %>% fit(
  input, logr_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  validation_split = 0.2
)

logrr1 <- model_residual %>% predict(input)
logrr2 <- model_residual %>% predict(input_pre)
sigma2_train <- exp(logrr1)
sigma2_pred <- exp(logrr2)

