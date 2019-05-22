library(devtools)
install_github("brsantos/baquantreg")

library(baquantreg)
#library(GpGp)
library(spNNGP)
library(GpGp)
#library(FNN)
#library(SpatialExtremes)
#library(splines)
library(fields)
library(geoR)

data(CHM)
CHM[,1] <- (CHM[,1] - min(CHM[,1])) / (max(CHM[,1]) - min(CHM[,1]))
CHM[,2] <- (CHM[,2] - min(CHM[,2])) / (max(CHM[,2]) - min(CHM[,2]))
set.seed(0420)
ind.train <- sample(1:dim(CHM)[1], 500)
ind.test <- sample(1:dim(CHM)[1],100)

y <- CHM[ind.train,3]
y.ho <- CHM[ind.test,3]
coords <- cbind(CHM[ind.train,1], CHM[ind.train,2])
coords.ho <- cbind(CHM[ind.test,1], CHM[ind.test,2])

X <- matrix(1,nrow = length(ind.train), ncol = 1)
X.ho <- matrix(1, nrow = length(ind.test), ncol = 1)

data_train <- data.frame(cbind(y, coords, X))
names(data_train) <- c("y", "spCoord1", "spCoord2", "X")

lambdaVec = c(1,2,3)
m <- 100
try <- baquantreg:::sppBQR(y~X-1, tau = 0.95, data_train, 1000, thin = 1, betaValue = 1,
       sigmaValue = 1, spCoord1=2, spCoord2=3, lambdaVec = lambdaVec, lambda = 1, shapeL = 2, rateL = 2, tuneP = 1, m,
       indexes = 1:m, alpha = 0.1,
       tuneA = 1000, priorVar = 100, refresh = 100, quiet = F,
       jitter = 1e-10, includeAlpha = T, tuneV = 0.5, kMT = 5,
       discLambda = F)


beta <- mean(try$chains[[1]][[1]])
sigma2 <- mean(try$chains[[1]][[2]])
xi <- colMeans(try$chains[[1]][[3]])
phi <- mean(try$chains[[1]][[4]])


check_loss <- function(q,y,f){
  e <- y - f
  return(mean(pmax(q*e, (q-1)*e)))
}


  fit <- fit_model(y, coords, X, "matern_isotropic")
  z_pred <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                        coords.ho, X, X.ho, fit$beta, reorder = TRUE)
  z_train <- predictions(fit$covparms, covfun_name = "matern_isotropic", y, coords,
                         coords, X, X, fit$beta, reorder = TRUE)
  epsilon <- sqrt(2*xi*sqrt(sigma2)/p/(1-p))*z_train + (1-2*p)/p/(1-p)*xi
  Z <- (epsilon - (1-2*p)/p/(1-p)*xi) / (sqrt(2*xi*sqrt(sigma2)/p/(1-p)))
  ct <- exp(- phi * rdist(coords,coords.ho))
  c <- exp(-phi * rdist(coords,coords))
  m <- as.vector(t(ct) %*% solve(c) %*% Z)
  qp <- X.ho %*% beta + m * sqrt(pi*sigma2/2/p/(1-p))
  loss <- check_loss(p,y.ho,qp)
  loss

