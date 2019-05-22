library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)

n <- 50
grid    <- expand.grid(seq(0,1, length = n), 
                       seq(0,1, length = n))
grid <- as.matrix(grid)

map.heatmap <- function (lat, lon, data, 
                         color_low="blue",color_high="yellow",color_na=gray(0.9),zeroiswhite=FALSE,
                         xlim=NULL, ylim=NULL, zlim=NULL,
                         mainTitle="", legendTitle="") {
  
  library(ggplot2)
  
  # Combine the data into a dataframe
  dfMap           <- as.data.frame(cbind(lon, lat, data))
  colnames(dfMap) <- c("lon", "lat", "Value")
  
  # Set limits for x, y, z if not specified as parameters
  if (is.null(xlim)) { xlim <- range( lon,na.rm=TRUE) }
  if (is.null(ylim)) { ylim <- range( lat,na.rm=TRUE) }
  if (is.null(zlim)) { zlim <- range(data,na.rm=TRUE) }
  
  # Create the plot
  p <- ggplot(dfMap, aes(x=lon, y=lat, fill=Value)) + theme_bw()
  p <- p + geom_tile()
  
  
  if(zeroiswhite){
    p <- p + scale_fill_gradient2(low=color_low,
                                  high=color_high,
                                  na.value=color_na,
                                  limits=zlim,
                                  name=legendTitle)
  }
  if(!zeroiswhite){
    p <- p + scale_fill_gradient(low=color_low,
                                 high=color_high,
                                 na.value=color_na,
                                 limits=zlim,
                                 name=legendTitle)
  }
  
  return(p)}

r <- 0.02
w <- as.vector(rmaxstab(1,grid,cov.mod = "powexp", nugget = 0, range = r,
                        smooth = 1))
y <- frech2gev(w,5,5,0.3)

map.heatmap(grid[,1], grid[,2] ,y)


n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = r,
                        smooth = 1))
y <- frech2gev(w,5,5,0.3)
quilt.plot(coords[,1], coords[,2] ,y)

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

nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 2000,m=5)
m = 5
pp <- nn$train
p.r <- nn$prediction
#neighbor info
neibinfo <- nn$neighbor
ord <- neibinfo$ord
y.ord <- neibinfo$y.ord
coord.ord <- neibinfo$coords.ord
ind1 <- neibinfo$n.indx

knnx <- get.knnx(coord.ord,coords.ho, k = m)
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

#nngp kriging
# yinput <- apply(pp$p.y.0, 1, mean)[ord][(m+1):dim(coords)[1]]
# y_pred <- apply(p.r$p.y.0, 1, mean)

#gpgp kriging


y_norm <- y.ord[(m+1):dim(coords)[1]] 


#kriging+nonparametric

input <- cbind(yinput,coordinput,y_train[ord][(m+1):dim(coords)[1]],
                coord.ord[(m+1):dim(coords)[1],])
input_pre <- cbind(yinput_pre,coordinput_pre,y_pred,coords.ho)

#nonparametric only

input <- cbind(yinput,coordinput, coord.ord[(m+1):dim(coords)[1],]) 
input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho) 
#kriging + users
inputmean <- colMeans(cbind(coord.ord[(m+1):dim(coords)[1],],y_train[ord][(m+1):dim(coords)[1]]))
inputsd <- apply(cbind(coord.ord[(m+1):dim(coords)[1],],y_train[ord][(m+1):dim(coords)[1]]), 2, sd)
input <- (cbind(coord.ord[(m+1):dim(coords)[1],],y_train[ord][(m+1):dim(coords)[1]]) - inputmean) / inputsd
input_pre <- (cbind(coords.ho,y_pred) - inputmean) / inputsd


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.1) %>% 
  # #layer_batch_normalization() %>%
  layer_dense(units = 12, activation = 'tanh') %>%
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
                  callback_early_stopping(monitor = "val_loss", min_delta = 0.1, patience = 10,
                                          verbose = 0, mode = "min"))
# Fit model to data
history <- model %>% fit(
  input, y_norm,
  batch_size = 32,
  epochs = 100,
  verbose = 0,
  validation_data = list(input_pre, y.ho),
  callbacks = callbacks
)

plot(history)

score <- model %>% evaluate(
  input_pre, y.ho,
  verbose = 0
)

print(list("quadN" = mse2, "nngp" = nn$mse, "gpgp" = mse1))
