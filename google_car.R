library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)



vars <- c("Y_block_med", "Longitude", "Latitude")
car1_10min <- df[,names(df) %in% c("Longitude", "Latitude", "Y", 
                                   "PC1", "PC2","PC3","PC4", "PC5", "PC6", "PC7")]
quilt.plot(car1_10min$Longitude, car1_10min$Latitude, car1_10min$Y)

ind <- sample(1:dim(car1_10min)[1],1000)
ind.ho <- seq(1,dim(car1_10min)[1], length.out = 1000)
y <- car1_10min$Y[ind]
y.ho <- car1_10min$Y[ind.ho]
coords <- as.matrix(car1_10min[ind,1:2])
coords.ho <- as.matrix(car1_10min[ind.ho,1:2])
X <- as.matrix(car1_10min[ind,4:10])
X.ho <- as.matrix(car1_10min[ind.ho, 4:10])

nngp <- function(y,y.ho,coords,coords.ho,X,X.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  #return prediction mse, predictions on both training dataset and testing dataset, and No. of neighbors
  starting <- list("phi"=10, "sigma.sq"=5, "tau.sq"=1)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(1, 5), "tau.sq.IG"=c(2, 1))
  cov.model <- "exponential"
  n.report <- n.samples * 0.8
  
  m.r <- spNNGP(y~X-1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = T)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = X.ho, coords.0 = coords.ho, n.omp.threads=2,verbose = T)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- NA
  pp <- spPredict(m.r, X.0 = X , coords.0 = coords, n.omp.threads=2, verbose = T)
  list("mse" = mse, "prediction" = p.r, "m" = m, "train"=pp, "neighbor" = m.r)
}




n <- dim(car1_10min)[1]
ss <- sample(1:5, n, replace = T)
mse <- matrix(0,5,2)
for (ho in 1:5){
y.ho <- car1_10min$Y[ss == ho]
coords.ho <- as.matrix(car1_10min[ss == ho,c(1,2)])
X.ho <- as.matrix(car1_10min[ss == ho, -which(names(car1_10min) %in% c("Y"))])
y <- car1_10min$Y[ss != ho]
coords <- as.matrix(car1_10min[ss != ho,c(1,2)])
X <- as.matrix(car1_10min[ss != ho, -which(names(car1_10min) %in% c("Y"))])
#gpgp, used to make kriging predictions as input to neural nets 
nn <- nngp(y,y.ho,coords,coords.ho,X,X.ho, phi,sigma.sq,tau.sq,n.samples = 1000,m=10)
yinput <- rowMeans(nn$train$p.y.0)
y_pred <- rowMeans(nn$prediction$p.y.0)

feature <- feature_extraction(X,y,X.ho,y.ho,coords,coords.ho, method = "nonparametric")
input <- feature$X
input_pre <- feature$X_pred
y_norm <- feature$y_train

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.1) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.0005),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y,
  batch_size = 32,
  epochs = 50,
  verbose = 0,
  validation_data = list(input_pre, y.ho)
)

plot(history)

score <- model %>% evaluate(
  input_pre, y.ho,
  verbose = 0
)
mse[ho,2] <- score$mean_squared_error
}


