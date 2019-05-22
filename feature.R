

feature_extraction <- function(X,y,X.ho,y.ho,coords,coords.ho,method = "kriging only",
                               m = 10,n.samples = 2000){
  library(keras)
  library(spNNGP)
  library(FNN)
  library(SpatialExtremes)
  library(splines)
  library(fields)
  #gpgp, used to make kriging predictions as input to neural nets 
  fit <- fit_model(y, coords, X, "matern_isotropic")
  y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                        coords.ho, X, X.ho, fit$beta, reorder = TRUE)
  y_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                         coords, X, X, fit$beta, reorder = TRUE)
  mse1 <- mean((y.ho - y_pred)^2) #this should be close to NNGP
  
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
  
  nn <- nngp(y,y.ho,coords,coords.ho,n.samples = n.samples, m=m)
  pp <- nn$train
  p.r <- nn$prediction
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
  
  list("X" = input, "X_pred" = input_pre, "y_train" = y_norm, "gpgp"=mse1, "nngp"=nn$mse,
       "nn" = nn)
}
