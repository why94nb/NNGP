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

K <- backend()
set.seed(0420)
data(CHM)
chm_loc <- as.matrix(CHM[,1:2])
sputm <- SpatialPoints(chm_loc, 
                       proj4string=CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
latlong <- coordinates(spgeo)
CHM <- cbind(latlong,CHM[,3])
names(CHM) <- c("lon", "lat", "height")

quilt.plot(CHM[,1], CHM[,2], CHM[,3])

ind.train <- sample(1:dim(CHM)[1], 10000)
ind.test <- sample(1:dim(CHM)[1],1000)
quilt.plot(CHM[ind.train,1], CHM[ind.train,2], CHM[ind.train,3]-21.5)
quilt.plot(CHM[ind.test,1], CHM[ind.test,2], CHM[ind.test,3]-21.5)

y <- CHM[ind.train,3]
y.ho <- CHM[ind.test,3]
coords <- cbind(CHM[ind.train,1], CHM[ind.train,2])
coords.ho <- cbind(CHM[ind.test,1], CHM[ind.test,2])

X <- matrix(1,nrow = length(ind.train), ncol = 1)
X.ho <- matrix(1, nrow = length(ind.test), ncol = 1)


feature <- feature_extraction(X,y,X.ho, y.ho, 
                              coords, coords.ho, 
                              method = "kriging np", n.samples = 1000, m=10)



input <- feature$X
input_pre <- feature$X_pred
y_norm <- feature$y_train




model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  #layer_dropout(rate = 0.5) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 10, activation = 'relu') %>%
  # layer_dropout(rate = 0.1) %>%
  #layer_dense(units = 12, activation = 'tanh') %>%
  # #layer_dense(units = 6, activation = 'tanh') %>%
  # #layer_dropout(rate = 0.1) %>%
  # #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('mse')
)

# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 2,
  epochs = 50,
  verbose = 0,
  #validation_split = 0.2
  validation_data = list(input_pre, y.ho)
)

plot(history)

nn_train <- model %>% predict(input)
nn_pred <- model %>% predict(input_pre)
quilt.plot(CHM[ind.test,1], CHM[ind.test,2],nn_pred - y.ho)
quilt.plot(CHM[ind.test,1], CHM[ind.test,2], CHM[ind.test,3])
plot( nn_pred,CHM[ind.test,3])
abline(a=0,b=1, col = "red")
score <- model %>% evaluate(
  input_pre, CHM[ind.test,3],
  verbose = 0
)


ind_bad = which(abs(nn_pred - y.ho) > 5)
ind_good = which(abs(nn_pred - y.ho) < 1)

quilt.plot(coords.ho[ind_bad,1],coords.ho[ind_bad,2], y.ho[ind_bad])
quilt.plot(coords.ho[ind_good,1],coords.ho[ind_good,2], y.ho[ind_good])

out = 0
inn = 0
for (i in 1:length(ind_bad)){
  if (y.ho[ind_bad[i]] < min(yinput_pre[ind_bad[1],]) | y.ho[ind_bad[i]] > 
                                    max(yinput_pre[ind_bad[1],])){
    out = out + 1
  }
  else{inn = inn + 1}
}


###### CI ######
KK <- 5
quant1 <- 0.025
quant2 <- 0.975
tilted_loss <- function(q, y, f) {
  e <- y - f
  K$mean(K$maximum(q * e, (q - 1) * e))
}
ci <- rep(0,KK)

for (i in 1:KK){
  if (i %% 10 == 0){print (i)}
  set.seed(i^2)
  ind.train <- sample(1:dim(CHM)[1], 2000)
  ind.test <- sample(1:dim(CHM)[1],1000)
  quilt.plot(CHM[ind.train,1], CHM[ind.train,2], CHM[ind.train,3]-21.5)
  quilt.plot(CHM[ind.test,1], CHM[ind.test,2], CHM[ind.test,3]-21.5)
  
  y <- CHM[ind.train,3]
  y.ho <- CHM[ind.test,3]
  coords <- cbind(CHM[ind.train,1], CHM[ind.train,2])
  coords.ho <- cbind(CHM[ind.test,1], CHM[ind.test,2])
  
  X <- matrix(1,nrow = length(ind.train), ncol = 1)
  X.ho <- matrix(1, nrow = length(ind.test), ncol = 1)
  
  feature <- feature_extraction(X,CHM[ind.train,3],X.ho, CHM[ind.test,3], 
                                CHM[ind.train,1:2], CHM[ind.test,1:2], 
                                method = "nonparametric", n.samples = 1000, m=30)
  
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

###### garson ######
wts <- get_weights(model)
weights <- rbind(wts[[2]], wts[[1]])
weights <- as.vector(weights)
w2 <- c(wts[[4]], wts[[3]])
weights <- c(weights,w2)
struct <- c(dim(input_pre)[2],500,1)
garson(weights, struct = struct)

cols <- heat.colors(10)
imp <- as.matrix(garson(weights, struct = struct, bar_plot = F))
importance_10k <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
imp[2] + (imp[13] + imp[14]) +imp[34],
imp[3] + (imp[15] + imp[16]) +imp[35],
imp[4] + (imp[17] + imp[18] ) +imp[36],
imp[5] + (imp[19] + imp[20] ) +imp[37],
imp[6] + (imp[21] + imp[22] ) +imp[38] - 0.004,
imp[7] + (imp[23] + imp[24] ) +imp[39],
imp[8] + (imp[25] + imp[26] ) +imp[40],
imp[9] + (imp[27] + imp[28] ) +imp[41],
imp[10] + (imp[29] + imp[30] ) +imp[42]-0.004)

importance_95 <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                   imp[2] + (imp[13] + imp[14]) +imp[34],
                   imp[3] + (imp[15] + imp[16]) +imp[35],
                   imp[4] + (imp[17] + imp[18] ) +imp[36],
                   imp[5] + (imp[19] + imp[20] ) +imp[37],
                   imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                   imp[7] + (imp[23] + imp[24] ) +imp[39],
                   imp[8] + (imp[25] + imp[26] ) +imp[40],
                   imp[9] + (imp[27] + imp[28] ) +imp[41],
                   imp[10] + (imp[29] + imp[30] ) +imp[42])

importance_75 <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                   imp[2] + (imp[13] + imp[14]) +imp[34],
                   imp[3] + (imp[15] + imp[16]) +imp[35],
                   imp[4] + (imp[17] + imp[18] ) +imp[36],
                   imp[5] + (imp[19] + imp[20] ) +imp[37],
                   imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                   imp[7] + (imp[23] + imp[24] ) +imp[39],
                   imp[8] + (imp[25] + imp[26] ) +imp[40],
                   imp[9] + (imp[27] + imp[28] ) +imp[41],
                   imp[10] + (imp[29] + imp[30] ) +imp[42])

importance_5 <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                   imp[2] + (imp[13] + imp[14]) +imp[34],
                   imp[3] + (imp[15] + imp[16]) +imp[35],
                   imp[4] + (imp[17] + imp[18] ) +imp[36],
                   imp[5] + (imp[19] + imp[20] ) +imp[37],
                   imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                   imp[7] + (imp[23] + imp[24] ) +imp[39],
                   imp[8] + (imp[25] + imp[26] ) +imp[40],
                   imp[9] + (imp[27] + imp[28] ) +imp[41],
                   imp[10] + (imp[29] + imp[30] ) +imp[42])

importance_25 <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                  imp[2] + (imp[13] + imp[14]) +imp[34],
                  imp[3] + (imp[15] + imp[16]) +imp[35],
                  imp[4] + (imp[17] + imp[18] ) +imp[36],
                  imp[5] + (imp[19] + imp[20] ) +imp[37],
                  imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                  imp[7] + (imp[23] + imp[24] ) +imp[39],
                  imp[8] + (imp[25] + imp[26] ) +imp[40],
                  imp[9] + (imp[27] + imp[28] ) +imp[41],
                  imp[10] + (imp[29] + imp[30] ) +imp[42])




nn_neibor <- as.data.frame(cbind(as.factor(1:10),importance_25[order(-importance_25)],
                                 importance_5[order(-importance_5)],
                                 importance_75[order(-importance_75)], 
                                 importance_95[order(-importance_95)], 
                                 importance_10k[order(-importance_10k)]))
names(nn_neibor) <- c("Neighbors","q25","q50", "q75", "q95", "mean")
nn_neibor$Neighbors <- as.factor(nn_neibor$Neighbors)

library(reshape2)
d <- melt(nn_neibor, id.vars="Neighbors")

ggplot(d, aes(Neighbors,value, col=variable, group = variable)) + geom_point(size=3) +
    geom_line(size = 1) + labs(y = "Importance")+ scale_color_discrete(name="Metrics",
                                                              breaks=c("q25", "q50", "q75", "q95", "mean"),
                                                              labels=c("25% quantile", "50% quantile",
                                                                       "75% quantile", "95% quantile", "mse")) +
  theme(legend.position = c(0.9, 0.8))
