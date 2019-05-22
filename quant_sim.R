library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)
library(GpGp)

K <- backend()

check_loss <- function(q,y,f){
  e <- y - f
  return(mean(pmax(q*e, (q-1)*e)))
}

tilted_loss <- function(q, y, f) {
  e <- y - f
  K$mean(K$maximum(q * e, (q - 1) * e))
}

rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

nngp <- function(y,y.ho,coords,coords.ho,n.samples = n.samples,m=m){
  starting <- list("phi"=10, "sigma.sq"=5, "tau.sq"=1)
  tuning <- list("phi"=0.1, "sigma.sq"=0.1, "tau.sq"=0.1)
  priors <- list("phi.Unif"=c(0.0002, 100), "sigma.sq.IG"=c(2, 2), "tau.sq.IG"=c(2,1))
  cov.model <- "exponential"
  #n.report <- n.samples * 0.8
  
  m.r <- spNNGP(y~X-1 , coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=4,verbose = T)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = X.ho, coords.0 = coords.ho, n.omp.threads=4,verbose = T)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- spPredict(m.r, X.0 = X , coords.0 = coords, n.omp.threads=2, verbose = F)
  list("mse" = mse, "train" = pp, "prediction" = p.r, "m" = m, "neighbor" = m.r)
}

feature_extraction <- function(method = "kriging only"){
  if (method == "kriging only"){
    input <- y_train
    input_pre <- y_pred
    y_norm <- y
  }
  
  if (method == "kriging user"){
    input <- cbind(y_train, coords)
    input_mean <- colMeans(input)
    input_sd <- apply(input,2,sd)
    input <- (input - input_mean[col(input)]) / input_sd[col(input)]
    input_pre <- cbind(y_pred, coords.ho)
    input_pre <- (input_pre - input_mean[col(input_pre)]) / input_sd[col(input_pre)]
    y_norm <- y
  }
  
  
  if (method == "nonparametric"){
    input <- cbind(yinput,coordinput,coord.ord[(m+1):dim(coords)[1],],dist)
    input_mean <- colMeans(input)
    input_sd <- apply(input,2,sd)
    input <- (input - input_mean[col(input)]) / input_sd[col(input)]
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho, dist_pre)
    input_pre <- (input_pre  - input_mean[col(input_pre)]) / input_sd[col(input_pre)]
    y_norm <- y.ord[(m+1):dim(coords)[1]] 
  }
  
  if (method == "kriging np"){
    input <- cbind(yinput,coordinput,y_train[ord][(m+1):dim(coords)[1]],
                   coord.ord[(m+1):dim(coords)[1],], dist)
    input_mean <- colMeans(input)
    input_sd <- apply(input,2,sd)
    input <- (input - input_mean[col(input)]) / input_sd[col(input)]
    input_pre <- cbind(yinput_pre,coordinput_pre,y_pred,coords.ho, dist_pre)
    input_pre <- (input_pre - input_mean[col(input_pre)]) / input_sd[col(input_pre)]
    y_norm <- y.ord[(m+1):dim(coords)[1]] 
  }
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 200, activation = 'relu', input_shape = c(dim(as.matrix(input))[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    # #layer_batch_normalization() %>%
    layer_dense(units = 12, activation = 'relu') %>%
    # layer_dropout(rate = 0.1) %>%
    #layer_dense(units = 12, activation = 'tanh') %>%
    # #layer_dense(units = 6, activation = 'tanh') %>%
    # #layer_dropout(rate = 0.1) %>%
    # #layer_batch_normalization() %>%
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = function(y_true, y_pred)
      tilted_loss(qt, y_true, y_pred),
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
  
  return(min(history$metrics$val_loss))
}

#########simulation GP data#########
m=10
qt <- 0.25
KK=2
results1 <- matrix(0,KK,5)
for (ii in 1:KK){
  set.seed(ii^2)
  print(ii)
  #generate data
  n <- 1000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  sigma.sq <- 5
  tau.sq <- 1
  phi <- 3/0.5
  D <- as.matrix(dist(coords))
  R <- exp(-phi*D)
  w <- rmvn(1, rep(0,n), sigma.sq*R)
  y <- rnorm(n, w, sqrt(tau.sq))
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  w.ho <- w[ho]
  coords.ho <- coords[ho,]
  y <- y[-ho]
  w <- w[-ho,,drop=FALSE]
  coords <- coords[-ho,]
  y <- exp(y/5)*10
  y.ho <- exp(y.ho/5)*10
  X <- matrix(1,nrow = length(y), ncol = 1)
  X.ho <- matrix(1, nrow = length(y.ho), ncol = 1)
  #nngp gpgp and feature engineering
  fit <- fit_model(y, coords, X, "matern_isotropic")
  y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                        coords.ho, X, X.ho, fit$beta, reorder = TRUE)
  y_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                         coords, X, X, fit$beta, reorder = TRUE)
  nn <- nngp(y,y.ho,coords,coords.ho,n.samples = 1000, m=m)
  pp <- nn$train
  p.r <- nn$prediction
  pre1 <- apply(p.r$p.y.0,1,quantile,probs = c(qt))
  results1[ii,1] <- check_loss(qt, y.ho, pre1)
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
  dist <- matrix(0,dim(coords)[1]-m,m)
  for (i in 1:(dim(coords)[1]-m)){
    yinput[i,] <- y.ord[ind1[[i+m]]]
    coordinput[i,] <- as.vector(t(coord.ord[ind1[[i+m]],]))
    temp <- matrix(coordinput[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coord.ord[i+m,], ncol = 2), k = m)
    dist[i,] <- as.vector(temp2$nn.dist)
  }
  
  yinput_pre <- matrix(0,dim(coords.ho)[1],m)
  coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
  dist_pre <- matrix(0,dim(coords.ho)[1],m)
  for (i in 1:dim(coords.ho)[1]){
    yinput_pre[i,] <- y[ind2[i,]]
    coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    temp <- matrix(coordinput_pre[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coords.ho[i,], ncol = 2), k = m)
    dist_pre[i,] <- as.vector(temp2$nn.dist)
  }
  
  results1[ii,2] <- feature_extraction(method = "kriging only")
  results1[ii,3] <- feature_extraction(method = "kriging user")
  results1[ii,4] <- feature_extraction(method = "nonparametric")
  results1[ii,5] <- feature_extraction(method = "kriging np")
}

  
#########simulation Maxstable data#########
m=10
qt <- 0.25
KK=2
results2 <- matrix(0,KK,5)
for (ii in 1:KK){
  set.seed(ii^2)
  print(ii)
  #generate data
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
  #nngp gpgp and feature engineering
  fit <- fit_model(y, coords, X, "matern_isotropic")
  y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                        coords.ho, X, X.ho, fit$beta, reorder = TRUE)
  y_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                         coords, X, X, fit$beta, reorder = TRUE)
  nn <- nngp(y,y.ho,coords,coords.ho,n.samples = 1000, m=m)
  pp <- nn$train
  p.r <- nn$prediction
  pre1 <- apply(p.r$p.y.0,1,quantile,probs = c(qt))
  results2[ii,1] <- check_loss(qt, y.ho, pre1)
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
  dist <- matrix(0,dim(coords)[1]-m,m)
  for (i in 1:(dim(coords)[1]-m)){
    yinput[i,] <- y.ord[ind1[[i+m]]]
    coordinput[i,] <- as.vector(t(coord.ord[ind1[[i+m]],]))
    temp <- matrix(coordinput[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coord.ord[i+m,], ncol = 2), k = m)
    dist[i,] <- as.vector(temp2$nn.dist)
  }
  
  yinput_pre <- matrix(0,dim(coords.ho)[1],m)
  coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
  dist_pre <- matrix(0,dim(coords.ho)[1],m)
  for (i in 1:dim(coords.ho)[1]){
    yinput_pre[i,] <- y[ind2[i,]]
    coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    temp <- matrix(coordinput_pre[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coords.ho[i,], ncol = 2), k = m)
    dist_pre[i,] <- as.vector(temp2$nn.dist)
  }
  
  results2[ii,2] <- feature_extraction(method = "kriging only")
  results2[ii,3] <- feature_extraction(method = "kriging user")
  results2[ii,4] <- feature_extraction(method = "nonparametric")
  results2[ii,5] <- feature_extraction(method = "kriging np")
}


#########simulation Potts data#########
m=10
qt <- 0.25
KK=2
results2 <- matrix(0,KK,5)
for (ii in 1:KK){
  set.seed(ii^2)
  print(ii)
  #generate data
  ncolor <- as.integer(8)
  beta <- 2
  theta <- c(rep(8, ncolor), beta)
  nrow <- 50
  ncol <- 50
  x <- matrix(sample(1:ncolor,nrow*ncol, replace = T), nrow = nrow, ncol = ncol)
  foo <- packPotts(x, ncolor)
  out <- potts(foo, theta, nbatch = 10)
  #out$batch
  image(out$final)
  
  final <- unpackPotts(out$final)
  final1 <- as.vector(final)
  y <- rep(0,nrow*ncol)
  
  for (i in 1: ncolor){
    y[final == i] <- rnorm(sum(final == i), i^2+5*i, sqrt(i)) / 10
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
  X <- matrix(1,nrow = length(y), ncol = 1)
  X.ho <- matrix(1, nrow = length(y.ho), ncol = 1)
  #nngp gpgp and feature engineering
  fit <- fit_model(y, coords, X, "matern_isotropic")
  y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                        coords.ho, X, X.ho, fit$beta, reorder = TRUE)
  y_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                         coords, X, X, fit$beta, reorder = TRUE)
  nn <- nngp(y,y.ho,coords,coords.ho,n.samples = 1000, m=m)
  pp <- nn$train
  p.r <- nn$prediction
  pre1 <- apply(p.r$p.y.0,1,quantile,probs = c(qt))
  results2[ii,1] <- check_loss(qt, y.ho, pre1)
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
  dist <- matrix(0,dim(coords)[1]-m,m)
  for (i in 1:(dim(coords)[1]-m)){
    yinput[i,] <- y.ord[ind1[[i+m]]]
    coordinput[i,] <- as.vector(t(coord.ord[ind1[[i+m]],]))
    temp <- matrix(coordinput[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coord.ord[i+m,], ncol = 2), k = m)
    dist[i,] <- as.vector(temp2$nn.dist)
  }
  
  yinput_pre <- matrix(0,dim(coords.ho)[1],m)
  coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
  dist_pre <- matrix(0,dim(coords.ho)[1],m)
  for (i in 1:dim(coords.ho)[1]){
    yinput_pre[i,] <- y[ind2[i,]]
    coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    temp <- matrix(coordinput_pre[i,], ncol = 2, byrow = T)
    temp2 <- get.knnx(temp, matrix(coords.ho[i,], ncol = 2), k = m)
    dist_pre[i,] <- as.vector(temp2$nn.dist)
  }
  
  results2[ii,2] <- feature_extraction(method = "kriging only")
  results2[ii,3] <- feature_extraction(method = "kriging user")
  results2[ii,4] <- feature_extraction(method = "nonparametric")
  results2[ii,5] <- feature_extraction(method = "kriging np")
}




  