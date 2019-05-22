library(maps)
library(geoR)
library(GpGp)
library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)



temp  <- PRCP[years == 2014,]
locs <- do.call(rbind, replicate(365, lon.lat, simplify=FALSE))
y <- as.vector(t(temp))
locs <- locs[!is.na(y),]
y <- y[!is.na(y)]

n <- length(y)
nsub <- n
ind <- sample(1:n,nsub)
ysub <- y[ind]
locssub <- locs[ind,]
ho <- sample(1:nsub, nsub/5)
ysub.ho <- ysub[ho]

locssub.ho <- locssub[ho,]
ysub <- ysub[-ho]

locssub <- locssub[-ho,]

#gpgp
X <- as.matrix( rep(1,length(ysub)) )
fit <- fit_model(ysub, locssub, X, "matern_isotropic")
X_pred <- as.matrix(rep(1,length(ysub.ho)))
y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", ysub, locssub,
                      locssub.ho, X, X_pred, fit$beta, reorder = TRUE)
mse1 <- mean((ysub.ho - y_pred)^2)

nngp <- function(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  starting <- list("phi"=phi, "sigma.sq"=sigma.sq, "tau.sq"=tau.sq)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(1, 5), "tau.sq.IG"=c(2, 1))
  cov.model <- "exponential"
  n.report <- n.samples
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = T)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1) , coords.0 = coords, n.omp.threads=2, verbose = F)
  list("mse" = mse, "train" = pp, "prediction" = p.r, "m" = m)
}
nn <- nngp(ysub,ysub.ho,locssub,locssub.ho, phi,sigma.sq,tau.sq,n.samples = 1000,m=10)

pp <- nn$train
p.r <- nn$prediction
knn <- get.knn(locssub, k=m)
knnx <- get.knnx(locssub,locssub.ho, k = m)
ind1 <- knn$nn.index
ind2 <- knnx$nn.index
dist1 <- knn$nn.dist
dist2 <- knnx$nn.dist
yinput <- matrix(0,dim(locssub)[1],m)
coordinput <- matrix(0,dim(locssub)[1],2*m)
for (i in 1:dim(locssub)[1]){
  yinput[i,] <- y[ind1[i,]]
  coordinput[i,] <- as.vector(t(locssub[ind1[i,],]))
}
#only yhat
yinput_pre <- matrix(0,dim(locssub.ho)[1],m)
coordinput_pre <- matrix(0,dim(locssub.ho)[1],m*2)
for (i in 1:dim(locssub.ho)[1]){
  yinput_pre[i,] <- ysub[ind2[i,]]
  coordinput_pre[i,] <- as.vector(t(locssub[ind2[i,],]))
}
input <- cbind(yinput,coordinput,locssub)
input_pre <- cbind(yinput_pre,coordinput_pre,locssub.ho)

batch_size <- 64
epochs <- 50

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 1000, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.3) %>% 
  # layer_batch_normalization() %>%
  # layer_dense(units = 12, activation = 'relu') %>%
  # layer_dropout(rate = 0.1) %>%
  #layer_batch_normalization() %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mse')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model %>% fit(
  input, ysub,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 0,
  validation_split = 0.2
)

plot(history)

score <- model %>% evaluate(
  input_pre, ysub.ho,
  verbose = 0
)
