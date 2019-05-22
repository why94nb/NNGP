library(GpGp)
n <- 10000
locs <- as.matrix( expand.grid( (1:100)/100, (1:100)/100 ) )
y <- fast_Gp_sim(c(4,0.2,0.5,0), "matern_isotropic", locs, 30 )
fields::image.plot( matrix(y,100,100) )

set.seed(1)
n <- 1000
coords <- cbind(runif(n,0,1), runif(n,0,1))
sigma.sq <- 4
phi <- 0.2
nu <- 0.5
tau.sq <- 0
y <- fast_Gp_sim(c(4,0.2,0.5,0), "matern_isotropic", coords, 30 )
ho <- sample(1:n, n/5)
y.ho <- y[ho]
coords.ho <- coords[ho,]
y <- y[-ho]
coords <- coords[-ho,]
X <- as.matrix( rep(1,length(y)) )
y <- exp(y) / 10
y.ho <- exp(y.ho) / 10

#gpgp
fit <- fit_model(y, coords, X, "matern_isotropic")
X_pred <- as.matrix(rep(1,length(y.ho)))
y_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
            coords.ho, X, X_pred, fit$beta, reorder = TRUE)

mse1 <- mean((y.ho - y_pred)^2)

nngp <- function(y,y.ho,coords,coords.ho,phi,sigma.sq,tau.sq,n.samples = 1000,m=10){
  starting <- list("phi"=phi, "sigma.sq"=sigma.sq, "tau.sq"=tau.sq, "nu"=nu)
  tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5,"nu"=0.5)
  priors <- list("phi.Unif"=c(0.1, 1), "sigma.sq.IG"=c(0.05,0.05), "tau.sq.IG"=c(0.05,0.05),"nu.Unif"=c(0.01,1))
  cov.model <- "matern"
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
pp <- nn$train
p.r <- nn$prediction
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
input <- cbind(yinput,coordinput,coords)#,apply(pp$p.y.0, 1, mean))
input_pre <- cbind(yinput_pre,coordinput_pre,coords.ho)


model <- keras_model_sequential()
model %>% 
  layer_dense(units = 100, activation = 'relu', input_shape = c(dim(input)[2]),
              kernel_regularizer = regularizer_l2(l = 0.02)) %>% 
  layer_dropout(rate = 0.1) %>% 
  #layer_batch_normalization() %>%
  layer_dense(units = 120, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  #layer_batch_normalization() %>%
  layer_dense(units = 1)

#summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.005, decay = 0.0005),
  metrics = c('mse')
)

# Training & Evaluation ----------------------------------------------------

# Fit model to data
history <- model %>% fit(
  input, y,
  batch_size = 256,
  epochs = 500,
  verbose = 0,
  validation_split = 0.2
)

plot(history)

score <- model %>% evaluate(
  input_pre, y.ho,
  verbose = 0
)



