library(keras)
library(spNNGP)
library(FNN)
library(SpatialExtremes)
library(splines)
library(fields)

rmvn <- function(n, mu=0, V = matrix(1)){
  p <- length(mu)
  if(any(is.na(match(dim(V),p))))
    stop("Dimension problem!")
  D <- chol(V)
  t(matrix(rnorm(n*p), ncol=p)%*%D + rep(mu,rep(n,p)))
}

sigma.sq <- 5
tau.sq <- 1
phi <- 3/0.5

nngp <- function(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  starting <- list("phi"=phi, "sigma.sq"=sigma.sq, "tau.sq"=tau.sq)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(2, 5), "tau.sq.IG"=c(2, 1))
  cov.model <- "exponential"
  n.report <- n.samples
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = F)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1) , coords.0 = coords, n.omp.threads=2, verbose = F)
  list("mse" = mse, "train" = pp, "prediction" = p.r, "m" = m)
}
#NNGP
nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10)
#compare nngp with quadN
compare <- function(y,y.ho, method){
  m <- nn$m
  mse <- nn$mse
  pp <- nn$train
  p.r <- nn$prediction
  
  if (method == "kriging only"){
    input <- apply(pp$p.y.0, 1, mean)
    input_pre <- apply(p.r$p.y.0, 1, mean)
    pdim <- 1
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 300, activation = 'relu', input_shape = c(pdim)) %>% 
      layer_dropout(rate = 0.4) %>% 
      # layer_dense(units = 12, activation = 'relu') %>%
      # layer_dropout(rate = 0.3) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "kriging user"){
    yinput <- apply(pp$p.y.0, 1, mean)
    input <- cbind(yinput,coords)
    input_pre <- cbind(apply(p.r$p.y.0, 1, mean),coords.ho)
    pdim <- 3
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 300, activation = 'relu', input_shape = c(pdim)) %>% 
      layer_dropout(rate = 0.4) %>% 
      # layer_dense(units = 12, activation = 'relu') %>%
      # layer_dropout(rate = 0.3) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "nonparametric"){
    #preparing inputs
    knn <- get.knn(coords, k=m)
    knnx <- get.knnx(coords,coords.ho, k = m)
    ind1 <- knn$nn.index
    ind2 <- knnx$nn.index
    dist1 <- knn$nn.dist
    dist2 <- knnx$nn.dist
    yinput <- matrix(0,dim(coords)[1],m)
    coordinput <- matrix(0,dim(coords)[1],2*m)
    for (i in 1:dim(coords)[1]){
      yinput[i,] <- y[ind1[i,]]
      coordinput[i,] <- as.vector(t(coords[ind1[i,],]))
    }
    #only yhat
    yinput_pre <- matrix(0,dim(coords.ho)[1],m)
    coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
    for (i in 1:dim(coords.ho)[1]){
      yinput_pre[i,] <- y[ind2[i,]]
      coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    }
    input <- cbind(yinput,coordinput,coords)
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho)
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
      layer_dropout(rate = 0.6) %>% 
      # layer_dense(units = 20, activation = 'relu') %>%
      # layer_dropout(rate = 0.1) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "kriging np"){
    #preparing inputs
    knn <- get.knn(coords, k=m)
    knnx <- get.knnx(coords,coords.ho, k = m)
    ind1 <- knn$nn.index
    ind2 <- knnx$nn.index
    dist1 <- knn$nn.dist
    dist2 <- knnx$nn.dist
    yinput <- matrix(0,dim(coords)[1],m)
    coordinput <- matrix(0,dim(coords)[1],2*m)
    for (i in 1:dim(coords)[1]){
      yinput[i,] <- y[ind1[i,]]
      coordinput[i,] <- as.vector(t(coords[ind1[i,],]))
    }
    #only yhat
    yinput_pre <- matrix(0,dim(coords.ho)[1],m)
    coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
    for (i in 1:dim(coords.ho)[1]){
      yinput_pre[i,] <- y[ind2[i,]]
      coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    }
    input <- cbind(yinput,coordinput,coords,apply(pp$p.y.0, 1, mean))
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho,apply(p.r$p.y.0, 1, mean))
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
      layer_dropout(rate = 0.4) %>% 
      # layer_dense(units = 20, activation = 'relu') %>%
      # layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
}

compare_y3 <- function(y,y.ho, method){
  m <- nn$m
  mse <- nn$mse
  pp <- nn$train
  p.r <- nn$prediction
  
  if (method == "kriging only"){
    input <- apply(pp$p.y.0, 1, mean)
    input_pre <- apply(p.r$p.y.0, 1, mean)
    pdim <- 1
    
    batch_size <- 16
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 100, activation = 'relu', input_shape = c(pdim)) %>% 
      layer_dropout(rate = 0.8) %>% 
      # layer_dense(units = 12, activation = 'relu') %>%
      # layer_dropout(rate = 0.3) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "kriging user"){
    yinput <- apply(pp$p.y.0, 1, mean)
    input <- cbind(yinput,coords)
    input_pre <- cbind(apply(p.r$p.y.0, 1, mean),coords.ho)
    pdim <- 3
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 50, activation = 'relu', input_shape = c(pdim)) %>% 
      layer_dropout(rate = 0.5) %>% 
      layer_dense(units = 20, activation = 'relu') %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 5, activation = 'relu') %>%
      layer_dropout(rate = 0.1) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "nonparametric"){
    #preparing inputs
    knn <- get.knn(coords, k=m)
    knnx <- get.knnx(coords,coords.ho, k = m)
    ind1 <- knn$nn.index
    ind2 <- knnx$nn.index
    dist1 <- knn$nn.dist
    dist2 <- knnx$nn.dist
    yinput <- matrix(0,dim(coords)[1],m)
    coordinput <- matrix(0,dim(coords)[1],2*m)
    for (i in 1:dim(coords)[1]){
      yinput[i,] <- y[ind1[i,]]
      coordinput[i,] <- as.vector(t(coords[ind1[i,],]))
    }
    #only yhat
    yinput_pre <- matrix(0,dim(coords.ho)[1],m)
    coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
    for (i in 1:dim(coords.ho)[1]){
      yinput_pre[i,] <- y[ind2[i,]]
      coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    }
    input <- cbind(yinput,coordinput,coords)
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho)
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
      layer_dropout(rate = 0.4) %>% 
      # layer_dense(units = 20, activation = 'relu') %>%
      # layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
  
  if (method == "kriging np"){
    #preparing inputs
    knn <- get.knn(coords, k=m)
    knnx <- get.knnx(coords,coords.ho, k = m)
    ind1 <- knn$nn.index
    ind2 <- knnx$nn.index
    dist1 <- knn$nn.dist
    dist2 <- knnx$nn.dist
    yinput <- matrix(0,dim(coords)[1],m)
    coordinput <- matrix(0,dim(coords)[1],2*m)
    for (i in 1:dim(coords)[1]){
      yinput[i,] <- y[ind1[i,]]
      coordinput[i,] <- as.vector(t(coords[ind1[i,],]))
    }
    #only yhat
    yinput_pre <- matrix(0,dim(coords.ho)[1],m)
    coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
    for (i in 1:dim(coords.ho)[1]){
      yinput_pre[i,] <- y[ind2[i,]]
      coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
    }
    input <- cbind(yinput,coordinput,coords,apply(pp$p.y.0, 1, mean))
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho,apply(p.r$p.y.0, 1, mean))
    
    batch_size <- 32
    epochs <- 30
    
    model <- keras_model_sequential()
    model %>% 
      layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
      layer_dropout(rate = 0.4) %>% 
      # layer_dense(units = 20, activation = 'relu') %>%
      # layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(),
      metrics = c('mse')
    )
    
    # Training & Evaluation ----------------------------------------------------
    
    # Fit model to data
    history <- model %>% fit(
      input, y,
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.2
    )
    
    plot(history)
    
    score <- model %>% evaluate(
      input_pre, y.ho,
      verbose = 0
    )
    return (score$loss)
  }
}

#compare_np with max stable process data
compare_mstab <- function(y, y.ho, n.samples =1000, m = 10, only = T  ){
  sigma.sq <- 1
  tau.sq <- 0.1
  phi <- 0.1
  starting <- list("phi"=phi, "sigma.sq"=5, "tau.sq"=1)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(0.01,1), "sigma.sq.IG"=c(0.2, 2), "tau.sq.IG"=c(0.02, 0.5))
  cov.model <- "exponential"
  n.report <- n.samples
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = F)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  
  batch_size <- 64
  epochs <- 50
  
  #preparing inputs
  knn <- get.knn(coords, k=m)
  knnx <- get.knnx(coords,coords.ho, k = m)
  ind1 <- knn$nn.index
  ind2 <- knnx$nn.index
  dist1 <- knn$nn.dist
  dist2 <- knnx$nn.dist
  yinput <- matrix(0,dim(coords)[1],m)
  coordinput <- matrix(0,dim(coords)[1],2*m)
  for (i in 1:dim(coords)[1]){
    yinput[i,] <- y[ind1[i,]]
    coordinput[i,] <- as.vector(t(coords[ind1[i,],]))
  }
  #only yhat
  yinput_pre <- matrix(0,dim(coords.ho)[1],m)
  coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
  for (i in 1:dim(coords.ho)[1]){
    yinput_pre[i,] <- y[ind2[i,]]
    coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
  }
  
  #yhat and others including kriging
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1), coords.0 = coords, n.omp.threads=2,verbose = F)
  
  
  
  if (only == T){
    input <- cbind(yinput,coordinput,coords)#,apply(pp$p.y.0, 1, mean))
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho)#,apply(p.r$p.y.0, 1, mean))
  }
  else{
    input <- cbind(yinput,coordinput,coords,apply(pp$p.y.0, 1, mean))
    input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho,apply(p.r$p.y.0, 1, mean))
  }
  
  # mu <- colMeans(input)
  # sigma <- apply(input,2,sd)
  # input <- (input - mu) / sigma
  # input_pre <- (input_pre - mu) / sigma
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 300, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.3) %>% 
    # layer_dense(units = 10, activation = 'relu') %>%
    # layer_dropout(rate = 0.2) %>%
    # layer_dense(units = 10, activation = 'relu') %>%
    # layer_dropout(rate = 0.1) %>%
    # layer_dense(units = 5, activation = 'relu') %>%
    # layer_dropout(rate = 0.1) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(),
    metrics = c('mse')
  )
  
  # Training & Evaluation ----------------------------------------------------
  
  # Fit model to data
  history <- model %>% fit(
    input, y,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 0,
    validation_split = 0.2
  )
  
  plot(history)
  
  score <- model %>% evaluate(
    input_pre, y.ho,
    verbose = 0
  )
  
  return (c(mse,score$loss))
}

#compare nngp krigeonly and krige+other user-constructed feature max stable process
compare_mstab1 <- function(y, y.ho, n.samples =1000, m = 10, only = T  ){
  sigma.sq <- 1
  tau.sq <- 0.1
  phi <- 0.1
  starting <- list("phi"=phi, "sigma.sq"=5, "tau.sq"=1)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
  priors <- list("phi.Unif"=c(0.01,1), "sigma.sq.IG"=c(0.2, 2), "tau.sq.IG"=c(0.02, 0.5))
  cov.model <- "exponential"
  n.report <- n.samples
  
  m.r <- spNNGP(y~1, coords=coords, starting=starting, method="response", n.neighbors=m,
                tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
                n.samples=n.samples, n.omp.threads=2, n.report=n.report,verbose = F)
  ##Prediction for holdout data
  p.r <- spPredict(m.r, X.0 = matrix(1,length(y.ho),1), coords.0 = coords.ho, n.omp.threads=2,verbose = F)
  plot(apply(p.r$p.y.0, 1, mean), y.ho)
  points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")
  
  mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)
  
  batch_size <- 64
  epochs <- 30
  
  #preparing inputs
  pp <- spPredict(m.r, X.0 = matrix(1,length(y),1), coords.0 = coords, n.omp.threads=2,
                  verbose = F)
  yinput <- apply(pp$p.y.0, 1, mean)
  
  if (only == T){
    input <- yinput
    input_pre <- apply(p.r$p.y.0, 1, mean)
    pdim <- 1
  }
  else{
    input <- cbind(yinput,coords)
    input_pre <- cbind(apply(p.r$p.y.0, 1, mean),coords.ho)
    pdim <- 3
  }
  
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 300, activation = 'relu', input_shape = c(pdim)) %>% 
    layer_dropout(rate = 0.4) %>% 
    # layer_dense(units = 12, activation = 'relu') %>%
    # layer_dropout(rate = 0.3) %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mean_squared_error',
    optimizer = optimizer_adam(),
    metrics = c('mse')
  )
  
  # Training & Evaluation ----------------------------------------------------
  
  # Fit model to data
  history <- model %>% fit(
    input, y,
    batch_size = batch_size,
    epochs = epochs,
    verbose = 0,
    validation_split = 0.2
  )
  
  plot(history)
  
  score <- model %>% evaluate(
    input_pre, y.ho,
    verbose = 0
  )
  
  return (c(mse,score$loss))
}


K=10
results1 <- matrix(0,K,5)
for (i in 1:K){
  set.seed(i)
  print(i)
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
  nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 2000,m=10)
  results1[i,1] <- nn$mse
  results1[i,2] <- compare(y,y.ho, method = "kriging only")
  results1[i,3] <- compare(y,y.ho, method = "kriging user")
  results1[i,4] <- compare(y,y.ho, method = "nonparametric")
  results1[i,5] <- compare(y,y.ho, method = "kriging np")
}

results2 <- matrix(0,K,5)
for (i in 1:K){
  set.seed(i)
  print(i)
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
  y <- exp(y)/10
  y.ho <- exp(y.ho)/10
  nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = n,m=10)
  results2[i,1] <- nn$mse
  results2[i,2] <- compare(y,y.ho, method = "kriging only")
  results2[i,3] <- compare(y,y.ho, method = "kriging user")
  results2[i,4] <- compare(y,y.ho, method = "nonparametric")
  results2[i,5] <- compare(y,y.ho, method = "kriging np")
}

results5 <- results6 <- matrix(0,K,2)
for (i in 1:K){
  set.seed(i)
  print(i)
  n <- 1000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  x <- cbind(1, rnorm(n))
  B <- as.matrix(c(1,5))
  sigma.sq <- 5
  tau.sq <- 1
  phi <- 3/0.5
  D <- as.matrix(dist(coords))
  R <- exp(-phi*D)
  w <- rmvn(1, rep(0,n), sigma.sq*R)
  y <- rnorm(n, x%*%B + w, sqrt(tau.sq))
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  x.ho <- x[ho,,drop=FALSE]
  w.ho <- w[ho]
  coords.ho <- coords[ho,]
  y <- y[-ho]
  x <- x[-ho,,drop=FALSE]
  w <- w[-ho,,drop=FALSE]
  coords <- coords[-ho,]
  y <- exp(y/5)
  y.ho <- exp(y.ho/5)
  results5[i,] <- compare_np(y,x,y.ho,x.ho)
  results6[i,] <- compare_np(y,x,y.ho,x.ho,only = F)
}

results7 <- results8 <- matrix(0,K,2)
for (i in 1:K){
  set.seed(2*i)
  print(i)
  n <- 1000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = 0.5,
                          smooth = 1))
  y <- frech2gev(w,1,2,0.3)
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  
  coords.ho <- coords[ho,]
  y <- y[-ho]
  
  coords <- coords[-ho,]
  results7[i,] <- compare_mstab(y,y.ho,n.samples = n)
  results8[i,] <- compare_mstab(y,y.ho,only = F,n.samples = n)
}

results9 <- results10 <- matrix(0,K,2)
for (i in 1:K){
  set.seed(2*i)
  print(i)
  n <- 1000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  w <- as.vector(rmaxstab(1,coords,cov.mod = "powexp", nugget = 0, range = 0.1,
                          smooth = 1))
  y <- frech2gev(w,1,2,0.5)
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  
  coords.ho <- coords[ho,]
  y <- y[-ho]
  
  coords <- coords[-ho,]
  results9[i,] <- compare_mstab1(y,y.ho,n.samples = n)
  results10[i,] <- compare_mstab1(y,y.ho,only = F,n.samples = n)
}

results11 <- matrix(0,K,5)
for (i in 1:K){
  set.seed(i)
  print(i)
  n <- 1000
  coords <- cbind(runif(n,0,1), runif(n,0,1))
  df <- 30
  b1 <- bs(coords[,1],df = df, intercept = T)
  b2 <- bs(coords[,2],df = df, intercept = T)
  basis <- matrix(0,n,df^2)
  for (j in 1:df){
    for (l in 1:df){
     basis[, (j-1)*df + l] <- b1[,j] * b2[,l] 
    }
  }
  a <- rt(dim(basis)[2],10) *5
  y <- as.vector(basis%*%a) + rnorm(n,0,0.1)
  ho <- sample(1:n, n/5)
  y.ho <- y[ho]
  
  coords.ho <- coords[ho,]
  y <- y[-ho]
  
  coords <- coords[-ho,]
  nn <- nngp(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 2000,m=10)
  results11[i,1] <- nn$mse
  results11[i,2] <- compare(y,y.ho, method = "kriging only")
  results11[i,3] <- compare(y,y.ho, method = "kriging user")
  results11[i,4] <- compare(y,y.ho, method = "nonparametric")
  results11[i,5] <- compare(y,y.ho, method = "kriging np")
}

