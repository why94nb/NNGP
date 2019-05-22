h <- 60
lag <- 0
m <- 60
n_neighbor <- 70

scale_input <- function(input,input_pred){
  input_mean <- colMeans(input)
  input_sd <- apply(input,2,sd)
  input1 <- (input_pred - input_mean[col(input_pred)]) / input_sd[col(input_pred)]
  return (input1)
}

data_train <- est_data[names(est_data) %in% c("Longitude", "Latitude", "Y_block_med", 
                                        "PC1", "PC2","PC3","PC4", "PC5", "PC6", "PC7", "H1",
                                        "H2", "H3", "H4", "t")]
data_train2 <- est_data2[names(est_data2) %in% c("Longitude", "Latitude", "Y_block_med", 
                                               "PC1", "PC2","PC3","PC4", "PC5", "PC6", "PC7", "H1",
                                               "H2", "H3", "H4", "t")]
data_pred <- datpred[names(datpred) %in% c("Longitude", "Latitude", "Y_block_med", 
                                              "PC1", "PC2","PC3","PC4", "PC5", "PC6", "PC7", "H1",
                                              "H2", "H3", "H4", "t")]
data_pred2 <- datpred2[names(datpred2) %in% c("Longitude", "Latitude", "Y_block_med", 
                                                 "PC1", "PC2","PC3","PC4", "PC5", "PC6", "PC7", "H1",
                                                 "H2", "H3", "H4", "t")]

input <- matrix(0,length(idx),n_neighbor*dim(data_train)[2])
y_train <- y[idx]

# h is lag to first neighbor, m is interval past lag h
for (i in 1:length(idx)){
  ub <- data_train[idx[i],"t"] - (h+lag)/60/24
  lb <- data_train[idx[i],"t"] - (h+lag+m)/60/24
  int <- dplyr::between(data_train[,"t"], lb, ub)
  input[i,] <- as.vector(t(data_train[int,][1:n_neighbor,]))
  input[i,] <- ifelse(is.na(input[i,]), 0, input[i,])
}

#input <- scale_input(input,input)

input_pred <- matrix(0,length(id_pred),n_neighbor*dim(data_train)[2])
for (i in 1:length(id_pred)){
  ub <- data_pred[id_pred[i],"t"] - h/60/24
  lb <- data_pred[id_pred[i],"t"] - (h+m)/60/24
  int <- dplyr::between(data_pred[,"t"], lb, ub)
  input_pred[i,] <- as.vector(t(data_pred[int,][1:n_neighbor,]))
  input_pred[i,] <- ifelse(is.na(input_pred[i,]), 0, input_pred[i,])
}

#input_pred <- scale_input(input,input_pred)
y.ho <- dat$resid_xb


set.seed(0420)
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_train,
  batch_size = 16,
  epochs = 20,
  verbose = 0,
  validation_data = list(input_pred, y.ho)
)

plot(history)

score <- model %>% evaluate(
  input_pred, y.ho,
  verbose = 0
)

################################################################################
###############   RNN   ########################################################
################################################################################
library(keras)

input <- array(0,c(length(idx),n_neighbor,dim(data_train)[2]))
y_train <- y[idx]

# h is lag to first neighbor, m is interval past lag h
for (i in 1:length(idx)){
  ub <- data_train[idx[i],"t"] - (h+lag)/60/24
  lb <- data_train[idx[i],"t"] - (h+lag+m)/60/24
  int <- dplyr::between(data_train[,"t"], lb, ub)
  input[i,1:n_neighbor,1:dim(data_train)[2]] <- as.matrix(data_train[int,][1:n_neighbor,])
  input[i,1:n_neighbor,1:dim(data_train)[2]] <- ifelse(is.na(input[i,1:n_neighbor
        ,1:dim(data_train)[2]]), 0, input[i,1:n_neighbor,1:dim(data_train)[2]])
}

#input <- scale_input(input,input)

input_pred <- array(0,c(length(id_pred),n_neighbor,dim(data_train)[2]))
for (i in 1:length(id_pred)){
  ub <- data_pred[id_pred[i],"t"] - h/60/24
  lb <- data_pred[id_pred[i],"t"] - (h+m)/60/24
  int <- dplyr::between(data_pred[,"t"], lb, ub)
  input_pred[i,,] <- as.matrix(data_pred[int,][1:n_neighbor,])
  input_pred[i,,] <- ifelse(is.na(input_pred[i,1:n_neighbor
        ,1:dim(data_pred)[2]]), 0, input_pred[i,1:n_neighbor,1:dim(data_pred)[2]])
}

#input_pred <- scale_input(input,input_pred)
y.ho <- dat$resid_xb

model <- keras_model_sequential()
model %>%
  layer_lstm(units = 100,input_shape=c(n_neighbor,dim(data_train)[2] )) %>% 
  #layer_lstm(units = 80) %>%
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
  input, y_train,
  batch_size = 32,
  epochs = 25,
  verbose = 0,
  validation_data = list(input_pred, y.ho)
  #callbacks = callbacks
)

plot(history)

scores <- model %>% evaluate(
  input_pred, y.ho,
  verbose = 0
)


