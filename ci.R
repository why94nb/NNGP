library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)
library(GpGp)

K <- backend()



KK <- 10
quant1 <- 0.025
quant2 <- 0.975

tilted_loss <- function(q, y, f) {
  e <- y - f
  K$mean(K$maximum(q * e, (q - 1) * e))
}
ci <- rep(0,KK)
#####GP data and tranformed GP ######
for (i in 1:KK){
if (i %% 10 == 0){print (i)}
set.seed(i^2)
#matern
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
y <- fast_Gp_sim(c(4,0.2,0.5,0), "matern_isotropic", coords, 30 )
ho <- sample(1:n, n/5)
y.ho <- y[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
coords <- coords[-ho,]
X <- as.matrix( rep(1,length(y)) )
X.ho <- as.matrix( rep(1,length(y.ho)) )

y <- y^3 / 100 + exp(y/5)/10
y.ho <- y.ho^3 / 100 + exp(y.ho/5)/10

feature <- feature_extraction(X,y,X.ho, y.ho, 
                              coords, coords.ho, 
                              method = "kriging np", n.samples = 1000, m=30)

input <- feature$X
input_pre <- feature$X_pred
y_norm <- feature$y_train


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = function(y_true, y_pred) tilted_loss(quant1, y_true, y_pred),
  optimizer = optimizer_adam(lr = 0.0003),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 8,
  epochs = 50,
  verbose = 0,
  #validation_split = 0.2
  validation_data = list(input_pre, y.ho)
)

nn_lower <- model %>% predict(input_pre)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = function(y_true, y_pred)
    tilted_loss(quant2, y_true, y_pred),
  optimizer = optimizer_adam(lr = 0.0001),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 8,
  epochs = 50,
  verbose = 0,
  #validation_split = 0.2
  validation_data = list(input_pre, y.ho)
)


nn_upper <- model %>% predict(input_pre)

ci[i] <- mean(nn_upper >= y.ho & nn_lower <= y.ho)
}


########## potts data ########
KK <- 10
quant1 <- 0.025
quant2 <- 0.975
tilted_loss <- function(q, y, f) {
  e <- y - f
  K$mean(K$maximum(q * e, (q - 1) * e))
}
ci <- rep(0,KK)
#####GP data and tranformed GP ######
for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
  #matern
  ncolor <- as.integer(8)
  beta <- 2
  theta <- c(rep(8, ncolor), beta)
  nrow <- 30
  ncol <- 30
  x <- matrix(sample(1:ncolor,nrow*ncol, replace = T), nrow = nrow, ncol = ncol)
  foo <- packPotts(x, ncolor)
  out <- potts(foo, theta, nbatch = 10)
  final <- unpackPotts(out$final)
  final1 <- as.vector(final)
  y <- rep(0,nrow*ncol)
  
  for (j in 1: ncolor){
    y[final == j] <- rnorm(sum(final == j), j^2+5*j, sqrt(j)) / 10
  }
  
  grid    <- expand.grid(1:nrow, 1:ncol)
  coords <- as.matrix(grid)
  #map.heatmap(coords[,1],coords[,2],y)
  quilt.plot(coords[,1], coords[,2], y)
  
  n <- nrow * ncol
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  coords.ho <- coords[ho,]
  y <- y[-ho]
  coords <- coords[-ho,]
  X <- as.matrix( rep(1,length(y)) )
  X.ho <- as.matrix( rep(1,length(y.ho)) )
  
  feature <- feature_extraction(X,y,X.ho, y.ho, 
                                coords, coords.ho, 
                                method = "kriging np", n.samples = 1000, m=30)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 200, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    # layer_dense(units = 20, activation = 'tanh') %>%
    # layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(lr = 0.001),
    metrics = c('mse')
  )
  
  # Training & Evaluation ----------------------------------------------------
  
  # Fit model to data
  history <- model %>% fit(
    input, y_norm,
    batch_size = 32,
    epochs = 100,
    verbose = 0,
    #validation_split = 0.2
    validation_data = list(input_pre, y.ho)
  )
  
  nn_lower <- model %>% predict(input_pre)
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 200, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 10, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    # layer_dense(units = 20, activation = 'tanh') %>%
    # layer_dropout(rate = 0.5) %>%
    layer_dense(units = 1)
  
  
  
  
  model %>% compile(
    loss = function(y_true, y_pred)
      tilted_loss(quant2, y_true, y_pred),
    optimizer = optimizer_adam(lr = 0.001),
    metrics = c('mse')
  )
  
  # Training & Evaluation ----------------------------------------------------
  
  # Fit model to data
  history <- model %>% fit(
    input, y_norm,
    batch_size = 32,
    epochs = 100,
    verbose = 0,
    validation_split = 0.2
  )
  
  nn_upper <- model %>% predict(input_pre)
  
  ci[i] <- mean(nn_upper >= y.ho & nn_lower <= y.ho)
}



####### max stable data######
KK <- 10
quant1 <- 0.025
quant2 <- 0.975
tilted_loss <- function(q, y, f) {
  e <- y - f
  K$mean(K$maximum(q * e, (q - 1) * e))
}
ci <- rep(0,KK)
#####GP data and tranformed GP ######
for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = 0.5,
                        smooth = 1))
y <- frech2gev(w,1,2,0.3)
ho <- sample(1:n, n/5)
y.ho <- y[ho]*10

coords.ho <- coords[ho,]
y <- y[-ho]*10

coords <- coords[-ho,]
X <- matrix(1,nrow = length(y), ncol = 1)
X.ho <- matrix(1, nrow = length(y.ho), ncol = 1)

feature <- feature_extraction(X,y,X.ho, y.ho, 
                              coords, coords.ho, 
                              method = "kriging np", n.samples = 1000, m=30)

input <- feature$X
input_pre <- feature$X_pred
y_norm <- feature$y_train


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = function(y_true, y_pred) tilted_loss(quant1, y_true, y_pred),
  optimizer = optimizer_adam(lr = 0.0003),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 8,
  epochs = 50,
  verbose = 0,
  #validation_split = 0.2
  validation_data = list(input_pre, y.ho)
)

nn_lower <- model %>% predict(input_pre)

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'relu') %>%
  #layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = function(y_true, y_pred)
    tilted_loss(quant2, y_true, y_pred),
  optimizer = optimizer_adam(lr = 0.0001),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 8,
  epochs = 50,
  verbose = 0,
  #validation_split = 0.2
  validation_data = list(input_pre, y.ho)
)


nn_upper <- model %>% predict(input_pre)

ci[i] <- mean(nn_upper >= y.ho & nn_lower <= y.ho)
}
