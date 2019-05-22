sigma2 <- fit$covparms[1]
rang <- fit$covparms[2]
smoth <- fit$covparms[3]
nugget <- fit$covparms[4]


K_ss = sigma2 * matern(rdist(coords.ho,coords.ho),rang,smoth )
K_s = sigma2 * matern(rdist(coords,coords.ho),rang,smoth )
K = sigma2 * matern(rdist(coords,coords),rang,smoth )


baoli = t(K_s)%*%solve(K)%*%K_s
s2 = diag(K_ss - baoli)
se = sqrt(s2) / sqrt(dim(coords.ho))

y_qt <- y_pred + 1.96 * sqrt(se)
mae <- mean(pmax(quant * (y.ho - y_qt), (quant - 1) * (y.ho - y_qt)))

library(rgdal)
library(spNNGP)
library(ggmap)
library(plotly)
library(ggplot2)
data("CHM")
chm_loc <- as.matrix(CHM[,1:2])
sputm <- SpatialPoints(chm_loc, 
      proj4string=CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
latlong <- coordinates(spgeo)
chm <- as.data.frame(cbind(latlong,CHM[,3]))
names(chm) <- c("lon", "lat", "height")


df <- data.frame(lon = c(-72.22682,-72.21603,-72.20393,-72.21476), lat = c(42.46365,42.46365,42.48132,42.48132))


require(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
register_google(key = "AIzaSyC6UPDJ_sVVhrAkZjW9NDg8RK05Yw907AI")
al1 = get_map(location = c(lon = -72.21538 , lat = 42.47246),zoom = 14, maptype = 'satellite')
al1MAP = ggmap(al1)
al1MAP + geom_polygon(data = df, aes(x = lon, y = lat), fill = "cadetblue1", alpha = 0.5)


ind <- sample(1:dim(chm)[1], 1000000)
ggplot(chm[ind,]) +  geom_point(aes(x = lon, y = lat, color = height)) + scale_color_gradient(low="blue",
                      high="yellow",
                      limits=range(chm$height))

df <- data.frame(
  x = c(-78.78112,-78.63818,-78.61417,-78.63818,-78.82556,-78.63818,-78.85029,-78.63818,-78.60556,-78.63818,-78.48056,-78.63818,-78.55556),
  y = c(35.79154,35.77959,35.71126,35.77959,35.82348,35.77959,35.73265,35.77959,35.85083,35.77959,35.78765,35.77959,35.69306)
)
illu <- get_googlemap("raleigh", markers = df, path = df, zoom=11, maptype = "roadmap")
illu_map <- ggmap(illu)
illu_map

####make plots#######

###### CI ######
KK <- 5
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
  ind.train <- sample(1:dim(CHM)[1], 2000)
  y <- CHM[ind.train,3]
  y.ho <- CHM[CHM[,1] <= -72.21476 & CHM[,1] >= -72.21477,3]
  coords <- cbind(CHM[ind.train,1], CHM[ind.train,2])
  coords.ho <- as.matrix(CHM[CHM[,1] <= -72.21476 & CHM[,1] >= -72.21477,1:2])
  
  X <- matrix(1,nrow = length(ind.train), ncol = 1)
  X.ho <- matrix(1, nrow = length(y.ho), ncol = 1)
  
  feature <- feature_extraction(X,y,X.ho, y.ho, 
                                coords, coords.ho, 
                                method = "kriging np", n.samples = 1000, m=30)
  
  input <- feature$X
  input_pre <- feature$X_pred
  y_norm <- feature$y_train
  
  nn <- feature$nn
  y_nngp <- rowMeans(nn$prediction$p.y.0)
  y_quant <- apply(nn$prediction$p.y.0,1,quantile,probs = c(0.025,0.975))
  
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
    optimizer = optimizer_adam(lr = 0.0005),
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
    optimizer = optimizer_adam(lr = 0.0005),
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


