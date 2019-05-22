library(FNN)

n <- 10000
grid    <- expand.grid(seq(0,1, length = 100), 
                       seq(0,1, length = 100))
grid <- as.matrix(grid)
x <- cbind(1, rnorm(n))
B <- as.matrix(c(1,5))
sigma.sq <- 5
tau.sq <- 1
phi <- 3/0.5
D <- as.matrix(dist(grid))
R <- exp(-phi*D)
w <- rmvn(1, rep(0,n), sigma.sq*R)
y <- x%*%B + w + rnorm(n,0 , sqrt(tau.sq))

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

map.heatmap(grid[,1],grid[,2],y)


n.samples <- 10000
starting <- list("phi"=phi, "sigma.sq"=3, "tau.sq"=0.5)
tuning <- list("phi"=0.5, "sigma.sq"=0.5, "tau.sq"=0.5)
priors <- list("phi.Unif"=c(3/1, 3/0.01), "sigma.sq.IG"=c(2, 5), "tau.sq.IG"=c(2, 1))
cov.model <- "exponential"
n.report <- 10000
m <- 10
##Predict for holdout set using both models

m.r <- spNNGP(y~x-1, coords=coords, starting=starting, method="response", n.neighbors=m,
              tuning=tuning, priors=priors, cov.model=cov.model,return.neighbors = T,
              n.samples=n.samples, n.omp.threads=2, n.report=n.report)
##Prediction for holdout data

p.r <- spPredict(m.r, X.0 = x.ho, coords.0 = coords.ho, n.omp.threads=2)
plot(apply(p.r$p.y.0, 1, mean), y.ho)
points(apply(p.r$p.y.0, 1, mean), y.ho, pch=19, col="blue")

mse <- mean((y.ho - apply(p.r$p.y.0, 1, mean))^2)

# Data Preparation ---------------------------------------------------

batch_size <- 32
epochs <- 40

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
pp <- spPredict(m.r, X.0 = x, coords.0 = coords, n.omp.threads=2)
input <- cbind(yinput,coordinput,x,coords)#,apply(pp$p.y.0, 1, mean))
input_pre <- cbind(yinput_pre,coordinput_pre,x.ho,coords.ho)#,apply(p.r$p.y.0, 1, mean))


# Define Model --------------------------------------------------------------

model <- keras_model_sequential()
model %>% 
  layer_dense(units = 400, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
  layer_dropout(rate = 0.3) %>% 
  # layer_dense(units = 20, activation = 'relu') %>%
  # layer_dropout(rate = 0.3) %>%
  layer_dense(units = 1)

summary(model)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(lr = 0.001),
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

# Output metrics
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')