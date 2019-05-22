library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)
library(geoR)
library(rgdal)
library(ggmap)
library(plotly)
library(ggplot2)
library(NeuralNetTools)

set.seed(0420)
KK <- 10
imp_gp <- imp_gp_nonp <- list()
#####GP data and tranformed GP ######
for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
  #matern
  n <- 10000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  y <- fast_Gp_sim(c(4,0.2,0.5,0), "matern_isotropic", coords, 30 )
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  coords.ho <- coords[ho,]
  y <- y[-ho]
  coords <- coords[-ho,]
  X <- as.matrix( rep(1,length(y)) )
  X.ho <- as.matrix( rep(1,length(y.ho)) )
  
  # y <- y^3 / 100 + exp(y/5)/10
  # y.ho <- y.ho^3 / 100 + exp(y.ho/5)/10
  
  feature <- feature_extraction(X,y,X.ho, y.ho, 
                                coords, coords.ho, 
                                method = "kriging np", n.samples = 1000, m=10)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_gp[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
  
  
  ###### nonparametric #######
  input <- feature$X[,-31]
  input_pre <- feature$X_pred[,-31]
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_gp_nonp[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
  
}

#importance_gp <- Reduce('+', qq) / KK
  
KK <- 10
imp_trans <- imp_trans_nonp <- list()
#####GP data and tranformed GP ######
for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
  #matern
  n <- 10000
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
                                method = "kriging np", n.samples = 1000, m=10)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_trans[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
  
  ##### nonparametric #######
  input <- feature$X[,-31]
  input_pre <- feature$X_pred[,-31]
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_trans_nonp[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
}

KK <- 10
imp_potts <- imp_potts_nonp <- list()
##### Potts model ######
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
                                method = "kriging np", n.samples = 1000, m=10)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_potts[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
  
  
  input <- feature$X[,-31]
  input_pre <- feature$X_pred[,-31]
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_potts_nonp[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
}


KK <- 10
imp_max <- imp_max_nonp <- list()
##### Max stable model ######
for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
  #matern
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
                                method = "kriging np", n.samples = 1000, m=10)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_max[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
  
  ##### nonparametrics ##########
  input <- feature$X[,-31]
  input_pre <- feature$X_pred[,-31]
  y_norm <- feature$y_train
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = "mse",
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
  
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  
  imp_max_nonp[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
}

imp <- Reduce('+', imp_max_nonp) / KK
importance_gp_nonp <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                   imp[2] + (imp[13] + imp[14]) +imp[34],
                   imp[3] + (imp[15] + imp[16]) +imp[35],
                   imp[4] + (imp[17] + imp[18] ) +imp[36],
                   imp[5] + (imp[19] + imp[20] ) +imp[37],
                   imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                   imp[7] + (imp[23] + imp[24] ) +imp[39],
                   imp[8] + (imp[25] + imp[26] ) +imp[40],
                   imp[9] + (imp[27] + imp[28] ) +imp[41],
                   imp[10] + (imp[29] + imp[30] ) +imp[42])

importance_trans_nonp <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                      imp[2] + (imp[13] + imp[14]) +imp[34],
                      imp[3] + (imp[15] + imp[16]) +imp[35],
                      imp[4] + (imp[17] + imp[18] ) +imp[36],
                      imp[5] + (imp[19] + imp[20] ) +imp[37],
                      imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                      imp[7] + (imp[23] + imp[24] ) +imp[39],
                      imp[8] + (imp[25] + imp[26] ) +imp[40],
                      imp[9] + (imp[27] + imp[28] ) +imp[41],
                      imp[10] + (imp[29] + imp[30] ) +imp[42])


importance_max_nonp <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                           imp[2] + (imp[13] + imp[14]) +imp[34],
                           imp[3] + (imp[15] + imp[16]) +imp[35],
                           imp[4] + (imp[17] + imp[18] ) +imp[36],
                           imp[5] + (imp[19] + imp[20] ) +imp[37],
                           imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                           imp[7] + (imp[23] + imp[24] ) +imp[39],
                           imp[8] + (imp[25] + imp[26] ) +imp[40],
                           imp[9] + (imp[27] + imp[28] ) +imp[41],
                           imp[10] + (imp[29] + imp[30] ) +imp[42])

importance_potts_nonp <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                           imp[2] + (imp[13] + imp[14]) +imp[34],
                           imp[3] + (imp[15] + imp[16]) +imp[35],
                           imp[4] + (imp[17] + imp[18] ) +imp[36],
                           imp[5] + (imp[19] + imp[20] ) +imp[37],
                           imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                           imp[7] + (imp[23] + imp[24] ) +imp[39],
                           imp[8] + (imp[25] + imp[26] ) +imp[40],
                           imp[9] + (imp[27] + imp[28] ) +imp[41],
                           imp[10] + (imp[29] + imp[30] ) +imp[42])


nn_neibor <- as.data.frame(cbind(as.factor(1:10),
                           
                                 importance_gp_nonp[order(-importance_gp_nonp)],
                                 importance_trans_nonp[order(-importance_trans_nonp)],
                                 importance_max_nonp[order(-importance_max_nonp)], 
                                 importance_potts_nonp[order(-importance_potts_nonp)]
                                 ))
nn_loc <- as.data.frame(cbind("s",c(1 - sum(importance_gp_nonp),1- sum(importance_trans_nonp),
                                    1-sum(importance_max_nonp), 1-sum(importance_potts_nonp)),
                              c("GP","transf", "max", "potts")))

names(nn_neibor) <- c("Neighbors","GP","transf", "max", "potts")
names(nn_loc) <- c("Neighbors", "value","variable")
nn_neibor$Neighbors <- as.factor(nn_neibor$Neighbors)
nn_loc$Neighbors <- as.factor(nn_loc$Neighbors)
nn_loc$value <- as.numeric(levels(nn_loc$value))[nn_loc$value]


library(reshape2)
d <- melt(nn_neibor, id.vars="Neighbors")

ggplot(d, aes(Neighbors,value, col=variable, group = variable)) + geom_point(size=3) +
  labs(y = "Importance")+  
  geom_point(data = nn_loc, aes(y=value),size=4) + 
  geom_line(size = 1) + 
  scale_color_discrete(name="Models",
                       breaks=c("GP","transf", "max", "potts"), labels=c("GP", "Transformed GP",
                                                                         "Max stable", "Potts")) +
  scale_x_discrete(limits=c("s",1:10)) + 
  theme(legend.position = c(0.9, 0.8))


imp <- Reduce('+', imp_gp) / KK

importance_gp <-  c(imp[31],imp[1] + (imp[11] + imp[12]) +imp[34],
  imp[2] + (imp[13] + imp[14]) +imp[35],
  imp[3] + (imp[15] + imp[16]) +imp[36],
  imp[4] + (imp[17] + imp[18] ) +imp[37],
  imp[5] + (imp[19] + imp[20] ) +imp[38],
  imp[6] + (imp[21] + imp[22] ) +imp[39] ,
  imp[7] + (imp[23] + imp[24] ) +imp[40],
  imp[8] + (imp[25] + imp[26] ) +imp[41],
  imp[9] + (imp[27] + imp[28] ) +imp[42],
  imp[10] + (imp[29] + imp[30] ) +imp[43])

importance_trans <-  c(imp[31],imp[1] + (imp[11] + imp[12]) +imp[34],
                    imp[2] + (imp[13] + imp[14]) +imp[35],
                    imp[3] + (imp[15] + imp[16]) +imp[36],
                    imp[4] + (imp[17] + imp[18] ) +imp[37],
                    imp[5] + (imp[19] + imp[20] ) +imp[38],
                    imp[6] + (imp[21] + imp[22] ) +imp[39] ,
                    imp[7] + (imp[23] + imp[24] ) +imp[40],
                    imp[8] + (imp[25] + imp[26] ) +imp[41],
                    imp[9] + (imp[27] + imp[28] ) +imp[42],
                    imp[10] + (imp[29] + imp[30] ) +imp[43])

importance_max <-  c(imp[31],imp[1] + (imp[11] + imp[12]) +imp[34],
                    imp[2] + (imp[13] + imp[14]) +imp[35],
                    imp[3] + (imp[15] + imp[16]) +imp[36],
                    imp[4] + (imp[17] + imp[18] ) +imp[37],
                    imp[5] + (imp[19] + imp[20] ) +imp[38],
                    imp[6] + (imp[21] + imp[22] ) +imp[39] ,
                    imp[7] + (imp[23] + imp[24] ) +imp[40],
                    imp[8] + (imp[25] + imp[26] ) +imp[41],
                    imp[9] + (imp[27] + imp[28] ) +imp[42],
                    imp[10] + (imp[29] + imp[30] ) +imp[43])

importance_potts <-  c(imp[31],imp[1] + (imp[11] + imp[12]) +imp[34],
                    imp[2] + (imp[13] + imp[14]) +imp[35],
                    imp[3] + (imp[15] + imp[16]) +imp[36],
                    imp[4] + (imp[17] + imp[18] ) +imp[37],
                    imp[5] + (imp[19] + imp[20] ) +imp[38],
                    imp[6] + (imp[21] + imp[22] ) +imp[39] ,
                    imp[7] + (imp[23] + imp[24] ) +imp[40],
                    imp[8] + (imp[25] + imp[26] ) +imp[41],
                    imp[9] + (imp[27] + imp[28] ) +imp[42],
                    imp[10] + (imp[29] + imp[30] ) +imp[43])

nn_neibor <- as.data.frame(cbind(as.factor(1:10),importance_gp[order(-importance_gp)][2:11],
                                 importance_trans[order(-importance_trans)][2:11],
                                 importance_max[order(-importance_max)][2:11], 
                                 importance_potts[order(-importance_potts)][2:11]
))
nn_kriging <- as.data.frame(cbind("Kriging",c(importance_gp[1], importance_trans[1],
                                              importance_max[1], importance_potts[1]),
                                  c("GP","transf", "max", "potts")))
nn_loc <- as.data.frame(cbind("s",c(1 - sum(importance_gp),1- sum(importance_trans),
                                    1-sum(importance_max), 1-sum(importance_potts)),
                                  c("GP","transf", "max", "potts")))
names(nn_kriging) <- c("Neighbors", "value","variable")
names(nn_loc) <- c("Neighbors", "value","variable")
names(nn_neibor) <-  c("Neighbors","GP","transf", "max", "potts")
nn_neibor$Neighbors <- as.factor(nn_neibor$Neighbors)
nn_kriging$Neighbors <- as.factor(nn_kriging$Neighbors)
nn_kriging$value <- as.numeric(levels(nn_kriging$value))[nn_kriging$value]
nn_loc$Neighbors <- as.factor(nn_loc$Neighbors)
nn_loc$value <- as.numeric(levels(nn_loc$value))[nn_loc$value]


library(reshape2)
d <- melt(nn_neibor, id.vars="Neighbors")

ggplot(d, aes(Neighbors,value, col=variable, group = variable)) + geom_point(size=3) +
  labs(y = "Importance")+  
  geom_point(data = nn_kriging, aes(y=value),size=4) + 
  geom_point(data = nn_loc, aes(y=value),size=4) + 
  geom_line(size = 1) + 
  scale_color_discrete(name="Models",
                       breaks=c("GP","transf", "max", "potts"), labels=c("GP", "Transformed GP",
                                                                              "Max stable", "Potts")) +
  scale_x_discrete(limits=c("Kriging","s",1:10)) + 
  theme(legend.position = c(0.9, 0.8))