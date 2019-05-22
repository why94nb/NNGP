library(potts)
library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)

K = 5
mse <- matrix(0,K,2)
for (k in 1:K){
  set.seed(k^2)
  n <- 50000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = 0.5,
                          smooth = 1))
  y <- frech2gev(w,1,2,0.5)
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]*10
  
  coords.ho <- coords[ho,]
  y <- y[-ho]*10
  
  coords <- coords[-ho,]
  X <- matrix(1,nrow = length(y), ncol = 1)
  X.ho <- matrix(1, nrow = length(y.ho), ncol = 1)
  

feature <- feature_extraction(X,y,X.ho, y.ho, 
                              coords, coords.ho, 
                              method = "kriging np", n.samples = 1000, m=10)

input <- feature$X
input_pre <- feature$X_pred
y_norm <- feature$y_train



model <- keras_model_sequential()
model %>% 
  layer_dense(units = 10000, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  
  layer_dense(units = 1)



model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.0005),
  metrics = c('mse')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 32,
  epochs = 100,
  verbose = 0,
  validation_data = list(input_pre, y.ho)
)


mse[k,1] <- feature$nngp
mse[k,2] <- min(history$metrics$val_mean_squared_error)
}
