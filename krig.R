covparms <- fit$covparms
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
krig_train <- rep(0,dim(coords)[1]-m)
krig_train1 <- rep(0,dim(coords)[1]-m)
krig_train2 <- rep(0,dim(coords)[1]-m)
for (i in 1:(dim(coords)[1]-m)){
  yinput[i,] <- y.ord[ind1[[i+m]]]
  coordinput[i,] <- as.vector(t(coord.ord[ind1[[i+m]],]))
  temp <- matrix(coordinput[i,], ncol = 2, byrow = T)
  temp2 <- get.knnx(temp, matrix(coord.ord[i+m,], ncol = 2), k = m)
  temp3 <- rdist(temp)
  dist[i,] <- as.vector(temp2$nn.dist)
  krig_train[i] <- fit$beta[1,1] + matern(dist[i,],phi = covparms[2], kappa = covparms[3])%*%
            solve(matern(temp3, phi = covparms[2], kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput[i,] - fit$beta[1,1])
  krig_train1[i] <- fit$beta[1,1] + matern(dist[i,],phi = covparms[2]/2, kappa = covparms[3])%*%
    solve(matern(temp3, phi = covparms[2]/2, kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput[i,] - fit$beta[1,1])
  krig_train2[i] <- fit$beta[1,1] + matern(dist[i,],phi = covparms[2]*2, kappa = covparms[3])%*%
    solve(matern(temp3, phi = covparms[2]*2, kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput[i,] - fit$beta[1,1])
}

yinput_pre <- matrix(0,dim(coords.ho)[1],m)
coordinput_pre <- matrix(0,dim(coords.ho)[1],m*2)
dist_pre <- matrix(0,dim(coords.ho)[1],m)
krig_pre <- rep(0,dim(coords.ho)[1])
krig_pre1 <- rep(0,dim(coords.ho)[1])
krig_pre2 <- rep(0,dim(coords.ho)[1])
for (i in 1:dim(coords.ho)[1]){
  yinput_pre[i,] <- y[ind2[i,]]
  coordinput_pre[i,] <- as.vector(t(coords[ind2[i,],]))
  temp <- matrix(coordinput_pre[i,], ncol = 2, byrow = T)
  temp2 <- get.knnx(temp, matrix(coords.ho[i,], ncol = 2), k = m)
  temp3 <- rdist(temp)
  dist_pre[i,] <- as.vector(temp2$nn.dist)
  krig_pre[i] <- fit$beta[1,1] + matern(dist_pre[i,],phi = covparms[2], kappa = covparms[3])%*%
    solve(matern(temp3, phi = covparms[2], kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput_pre[i,] - fit$beta[1,1])
  krig_pre1[i] <- fit$beta[1,1] + matern(dist_pre[i,],phi = covparms[2]/2, kappa = covparms[3])%*%
    solve(matern(temp3, phi = covparms[2]/2, kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput_pre[i,] - fit$beta[1,1])
  krig_pre2[i] <- fit$beta[1,1] + matern(dist_pre[i,],phi = covparms[2]*2, kappa = covparms[3])%*%
    solve(matern(temp3, phi = covparms[2]*2, kappa = covparms[3]) + 
            covparms[4]*diag(m))%*%(yinput_pre[i,] - fit$beta[1,1])
}


input <- cbind(krig_train,coord.ord[(m+1):dim(coords)[1],],krig_train1,krig_train2)
input_mean <- colMeans(input)
input_sd <- apply(input,2,sd)
input <- (input - input_mean[col(input)]) / input_sd[col(input)]
input_pre <- cbind(krig_pre,coords.ho, krig_pre1, krig_pre2)
input_pre <- (input_pre  - input_mean[col(input_pre)]) / input_sd[col(input_pre)]
y_norm <- y.ord[(m+1):dim(coords)[1]] 

ll <- lm(y_norm~input-1)
lm_pred <- predict(ll, as.data.frame(input_pre))

