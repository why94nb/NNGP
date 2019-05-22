library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)


n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = 0.1,
                        smooth = 1))
y <- frech2gev(w,10,1,0.5)
quilt.plot(coords[,1], coords[,2], y)
ho <- sample(1:n, n/5)
y.ho <- y[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
coords <- coords[-ho,]

#gpgp, used to make kriging predictions as input to neural nets 
X <- as.matrix( rep(1,length(y)))
fit <- fit_model(y, coords, X, "matern_isotropic")
X_pred <- as.matrix(rep(1,length(y.ho)))
y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                      coords.ho, X, X_pred, fit$beta, reorder = TRUE)
y_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                       coords, X, X, fit$beta, reorder = TRUE)
mse1 <- mean((y.ho - y_pred)^2) #this should be close to NNGP

nngp <- function(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  #return prediction mse, predictions on both training dataset and testing dataset, and No. of neighbors
  starting <- list("phi"=10, "sigma.sq"=5, "tau.sq"=1)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(1, 5), "tau.sq.IG"=c(2, 1))
  cov.model <- "exponential"
  n.report <- n.samples * 0.8
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = F)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- NA
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1) , coords.0 = coords, n.omp.threads=2, verbose = F)
  list("mse" = mse, "prediction" = p.r, "m" = m, "train"=pp, "neighbor" = m.r)
}

nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 2000,m=10)
m = 10
pp <- nn$train
p.r <- nn$prediction
#neighbor info
neibinfo <- nn$neighbor
ord <- neibinfo$ord
y.ord <- neibinfo$y.ord
coord.ord <- neibinfo$coords.ord
ind1 <- neibinfo$n.indx

knnx <- get.knnx(coords,coords.ho, k = m)
ind2 <- knnx$nn.index



yinput <- matrix(0,dim(coords)[1]-m,m)
coordinput <- matrix(0,dim(coords)[1]-m,2*m)
for (i in 1:(dim(coords)[1]-m)){
  yinput[i,] <- y.ord[ind1[[i+m]]]
  coordinput[i,] <- as.vector(t(coord.ord[ind1[[i+m]],]))
}

yinput_pre <- matrix(0,dim(coords.ho)[1],m)
coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
for (i in 1:dim(coords.ho)[1]){
  yinput_pre[i,] <- y[ind2[i,]]
  coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
}

input <- array(0,dim = c(length(y) - m, m + 1, 3))
input_pred <- array(0, dim = c(length(y_pred), m+1 , 3))

for (i in 1: dim(input)[1]){
  #input[i,11,1] <- y.ord[(m+1):dim(coords)[1]][i]
  input[i,m+1,2:3] <- coord.ord[i,]
  input[i,m:1,2:3] <- coord.ord[ind1[[i+m]],]
  input[i,m:1,1] <- yinput[i,]
}

for (i in 1: dim(input_pred)[1]){
  input_pred[i,m+1,2:3] <- coords.ho[i,]
  input_pred[i,m:1,2:3] <- coords[ind2[i,],]
  input[i,m:1,1] <- yinput_pre[i,]
}

model <- keras_model_sequential()
model %>%
  layer_lstm(units = 10,input_shape=c(m+1, 3), return_sequences = T) %>% 
  layer_lstm(units = 80) %>%
  layer_dense(units = 1) 

# Try using different optimizers and different optimizer configs
model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('mse')
)

# callbacks <- list(callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 5,
#                                                 verbose = 0, mode = "min", epsilon = 1e-04, cooldown = 0,
#                                                 min_lr = 0),
# callback_early_stopping(monitor = "val_loss", min_delta = 0.0005, patience = 10,
#                         verbose = 0, mode = "min"))

history <- model %>% fit(
  input, y.ord[(m+1):dim(coords)[1]],
  batch_size = 16,
  epochs = 100,
  verbose = 0,
  validation_data = list(input_pred, y.ho)
  #callbacks = callbacks
)

plot(history)

scores <- model %>% evaluate(
  input_pred, y.ho,
  verbose = 0
)
