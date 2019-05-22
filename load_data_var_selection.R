# rm(list=ls())
# 
# # LOAD THE AQS, CMAQ AND OTHER AOD/MET/LU DATA
# # remove duplicated locations
# AQS            <- read.csv("Observation 2011/2011 PM25.csv")
# AQS$Sample.Date             <- as.numeric(as.Date(AQS[,2],"%m/%d/%Y")-as.Date("2011-01-01"))+1
# 
# duplicates <- AQS[duplicated(AQS[,c("Latitude","Longitude","Sample.Date")]),]
# whereduplicates = sapply(1:nrow(duplicates),function(x) which(AQS[,"Latitude"]==duplicates[x,"Latitude"]&
#                                     AQS[,"Longitude"]==duplicates[x,"Longitude"]&
#                                     AQS[,"Sample.Date"]==duplicates[x,"Sample.Date"]))
# aves = AQS[c(whereduplicates),]
# aveCon = aggregate(aves$Concentration,by =list(aves$Sample.Date),mean)
# aveAQS = data.frame(Site=aves$Site[1],Sample.Date=aveCon$Group.1,
#                     Concentration = aveCon$x,Latitude = aves$Latitude[1], Longitude = aves$Longitude[1])
# AQS  = rbind(AQS[-c(whereduplicates),],aveAQS) 
# AQS$Site[AQS$Site == aves$Site[2]] <- aves$Site[1]
# allsite        <- unique(AQS[,c("Site","Latitude","Longitude")])
# dim(allsite) # 829 unique sites
# AQS = AQS[order(AQS[,1]),]
# load("National_12km_AOD_2011.RData")
# 
# # EXTRACT IMPORTANT AQS VARIABLES
# 
# station     <- AQS[,1]
# t           <- AQS[,2]
# Y           <- AQS[,3]
# # change to log scale 
# Y[Y==0]     <- 0.05
# Y           <- log(Y)
# qqnorm(Y);qqline(Y)
# S           <- AQS[,5:4]
# 
# rm(AQS)
# gc()
# # x       <- NULL # CONVERT TO NUMERIC MATRIX
# # for(j in 1:ncol(dat)){
# #   x <- cbind(x,as.numeric(dat[,j]))
# # }
# # colnames(x) <- colnames(dat)
# x = dat
# rm(dat)
# gc()
# CMAQ        <- as.matrix(read.csv("CMAQ 2011 PM25.csv"))
# 
# # change to log
# CMAQ = log(CMAQ)
# 
# CMAQ_layout <- read.csv("CMAQ.layout.csv")
# AOD_grid    <- read.csv("CMAQ Grid LatLon.csv")
# 
# nx     <- 53807                # NUMBER OF GRID CELLS
# theseX <- c(5:12,17:33)        # KEEP THESE AS PREDICTORS
# p      <- length(theseX)
# namesX <- colnames(x)[theseX]
# idX    <- x[,16]               # CELL ID NUMBER
# sx     <- x[1:nx,13:14]        # CELL LAT/LONG
# newsx  <- AOD_grid[,c(3,2)]
# x      <- x[,theseX]
# x      <- as.matrix(x)
# 
# # SET UP THE OUTPUT MATRICES AND VARIABLES
# stat <- unique(station)
# n    <- length(stat)
# date <- colnames(CMAQ)
# PM   <- matrix(NA,365,n)
# CM   <- array(NA,c(365,n,4))
# s    <- matrix(NA,n,2)
# X    <- array(NA,c(365,n,p))
# dimnames(X)[[3]]<-namesX
# 
# # LOAD AND COMPUTE THE DATA FOR EACH AQS SITE
# library(fields)
# for(i in 1:n){
# 
#   # GET THE DATA FROM AQS
# 
#   these      <- which(station==stat[i])
#   days       <- t[these]
#   PM[days,i] <- Y[these]
#   s[i,]      <- as.numeric(S[these[1],])
# 
#   # FIND THE CLOSEST CMAQ CELL
#   d          <- which.min(rdist.earth(matrix(s[i,],ncol=2),CMAQ_layout[,3:2]))
#   # d        <- order((s[i,1]-CMAQ_layout[,3])^2+
#   #                   (s[i,2]-CMAQ_layout[,2])^2)[1]
#   CM[,i,1] <- CMAQ[d,] # VALUE AT THE CLOSEST CELL
# 
#   # FIND NEIGHBORING CELLS
# 
#   d        <- order((CMAQ_layout[d,4]-CMAQ_layout[,4])^2+
#                       (CMAQ_layout[d,5]-CMAQ_layout[,5])^2)[1:100]
# 
#   # plot(CMAQ_layout[d,3:2])
#   # points(CMAQ_layout[d[2:5],3:2],pch=19,col=2)
#   # points(CMAQ_layout[d[6:13],3:2],pch=19,col=3)
#   # points(CMAQ_layout[d[14:29],3:2],pch=19,col=4)
#   # points(s[i,1],s[i,2],pch=19)
# 
#   CM[,i,2] <- colMeans(CMAQ[d[2:5],])   # Average value at the neighboring cells
#   CM[,i,3] <- colMeans(CMAQ[d[6:13],])  # Average value at the 2nd-order neighbors
#   CM[,i,4] <- colMeans(CMAQ[d[14:29],]) # Average value at the 3rd-order neighbors
# 
#   # GET THE AOD/MET/LU VARIABLES
# 
#   d       <- which.min(rdist.earth(matrix(s[i,],ncol=2), newsx)) # FIND THE CLOSEST CELL
#   X[,i,]  <- x[idX==idX[d],]
# }
# 
# rm(d,date,days,S,these,i,stat,station,
#    Y,t,CMAQ,CMAQ_layout,AOD_grid,
#    idX,namesX,nx,sx,x,theseX,newsx)
# 
# set.seed(0820)
# fold <- sample(1:5,n,replace=TRUE)
# save(CM,X,fold,n,p,PM,s,"AQS_CMAQ_2011_log.RData")
# 
# library(leaps)
# library(plyr)
# library(fields)
# 
# set.panel(2,2)
# df.X = adply(X,1)
# df.CM = adply(CM,1)
# folds = rep(fold,365)
# stats = rep(stat,365)
# Longt = expand.grid(s[,1],1:365)
# Latt  = expand.grid(s[,2],1:365)
# dat = data.frame(PM=c(t(PM)),Site = stats,Longitude = Longt[,1],Latitude=Latt[,1],t=Latt[,2],df.X[,-1],df.CM[,-1])
# colnames(dat)[(ncol(dat)-3):ncol(dat)] <- paste0("CMAQ",1:4)
# folds = folds[!is.na(dat$PM)]
# dat = dat[!is.na(dat$PM),];
# dat$folds = folds
# 
# save(dat,file="AQS_CMAQ_2011_log_dataframe.RData")
# #  cross validation to choose variables
# predict.regsubsets = function(object ,newdata ,id ,...){
#   form=as.formula(object$call[[2]])
#   mat=model.matrix(form ,newdata)
#   coefi =coef(object ,id=id)
#   xvars =names(coefi)
#   mat[,xvars]%*%coefi
# }
# 
# varnames = c(dimnames(X)[[3]],paste0("CMAQ",1:4))
# nvmax  = 25
# cv.errors =matrix (NA ,5,nvmax, dimnames =list(NULL , paste (1:nvmax)))
# 
# for(j in 1:5){
#   cv.fit = regsubsets(formula(paste0("PM~",paste0(varnames,collapse = "+"))),data=dat[folds!=j,],nvmax = nvmax)
#   for(i in 1:nvmax) {
#     pred=predict(cv.fit, dat[folds==j,],id=i)
#     cv.errors[j,i]=mean((dat$PM[folds==j]-pred)^2)
#   }
# }
# 
# mean.cv.errors =colMeans(cv.errors)
# plot(mean.cv.errors)
# #  pick n = 14, include n = 15 improvement < 0.2%
# P = 14
# which.min(mean.cv.errors)
# image.plot(cor(dat[,colnames(dat)%in%varnames]))
# 
# # refit model
# regfit.full=regsubsets(formula(paste0("PM~",paste0(varnames,collapse = "+"))),dat,nvmax = nvmax)
# sum.full =summary(regfit.full)
# pdf("var_select.pdf")
# plot(regfit.full, scale="adjr2")
# plot(sum.full$adjr2 ,xlab=" Number of Variables ",ylab="adjr2", type="l")
# dev.off()
# 
# dimnames(X)[[3]][dimnames(X)[[3]]%in%names(coef(regfit.full,P))]
# which(dimnames(X)[[3]]%in%names(coef(regfit.full,P)))
# diff(sum.full$adjr2) <=0.001
# names(coef(regfit.full,P))
# namesx = names(coef(regfit.full,P))
# 
# coef(regfit.full,P)
# # (Intercept)           elev   forest_cover nldas_pevapsfc nldas_dlwrfsfc nldas_dswrfsfc 
# # 1.254621e+00   1.422942e-04   9.990001e-02   3.882267e-01   1.744470e-03   2.266649e-04 
# # nldas_cape     nldas_rh2m       narr_dpt      narr_hpbl   narr_vgrd10m narr_tmp1815mb 
# # 9.826514e-05   5.708377e-03  -2.227681e-02  -1.678958e-04   1.089151e-02   1.742218e-02 
# # CMAQ1          CMAQ3          CMAQ4 
# # 3.190448e-01  -5.176752e-01   7.704987e-01 
# 
# # repeat it excluding cmaq variables
# varnames = dimnames(X)[[3]]
# for(j in 1:5){
#   cv.fit = regsubsets(formula(paste0("PM~",paste0(varnames,collapse = "+"))),data=dat[folds!=j,],nvmax = 25)
#   for(i in 1:25) {
#     pred=predict(cv.fit, dat[folds==j,],id=i)
#     cv.errors[j,i]=mean((dat$PM[folds==j]-pred)^2)
#   }
# }
# 
# mean.cv.errors =colMeans(cv.errors)
# plot(mean.cv.errors)
# # pick P = 11, including P = 12 increase performance <0.2 %
# P = 11
# which.min(mean.cv.errors)
# 
# regfit.X=regsubsets(formula(paste0("PM~",paste0(varnames,collapse = "+"))),dat,nvmax=25)
# sum.X = summary(regfit.X)
# pdf("var_select_X.pdf")
# plot(regfit.X, scale="adjr2")
# plot(sum.X$adjr2 ,xlab=" Number of Variables ",ylab="adjr2", type="l")
# abline(v=P)
# dev.off()
# coef(regfit.X,P)
# # (Intercept)             is nldas_pevapsfc nldas_dlwrfsfc     nldas_cape     nldas_rh2m  nldas_vgrd10m 
# # 4.213019e+00   1.294487e-03   6.570199e-01   1.415471e-03   2.036154e-04   8.533672e-03   3.075746e-02 
# # nldas_pressfc       narr_dpt       narr_vis      narr_hpbl narr_tmp1815mb 
# # 7.278646e-06  -4.073579e-02   1.814556e-05  -2.564837e-04   2.594449e-02 
# 
# 
# dimnames(X)[[3]][dimnames(X)[[3]]%in%names(coef(regfit.X,P))]
# namesxidx = which(dimnames(X)[[3]]%in%names(coef(regfit.full,P)))
# diff(sum.X$adjr2) <=0.001
# names(coef(regfit.full,P))
# namesx = dimnames(X)[[3]][dimnames(X)[[3]]%in%names(coef(regfit.X,P))]
# namesx
# # "is"             "nldas_pevapsfc" "nldas_dlwrfsfc" "nldas_cape"     "nldas_rh2m"     "nldas_vgrd10m" 
# # "nldas_pressfc"  "narr_dpt"       "narr_vis"       "narr_hpbl"      "narr_tmp1815mb"
# load(file = "AQS_CMAQ_2011_log.RData")
# save(CM,X,fold,n,p,PM,s,namesx,namesxidx,file="AQS_CMAQ_2011_log.RData")
# load(file="AQS_CMAQ_2011_log_dataframe.RData")
# save(dat,namesx,namesxidx,file="AQS_CMAQ_2011_log_dataframe.RData")


# # random forest -----------------------------------------------------------
# 
# load(file="AQS_CMAQ_2011_log_dataframe.RData")
# folds = dat$folds
# # random forest
# library(randomForest)
# # selected covariates and no cmaq
# varnames = c("Longitude","Latitude","t",namesx)
# # selected covariates
# # varnames = c("Longitude","Latitude","t",namesx,"CMAQ1")
# # all covariates
# # varnames = colnames(dat)[-c(1,2,32:ncol(dat))]
# # ntrees = c(500, 800, 1000) # c(50,100,200,300,400)
# oob.errors = mse.test = mad.test = cor.test = cov.test = time.tab = c()
# 
# # for(j in 1:5){
#   # for(i in 1:length(ntrees)){
#     tick = proc.time()
#     rf.PM=randomForest(formula(paste0("PM~",paste0(varnames,collapse = "+"))),data=dat[folds!=j,],importance=TRUE,ntree=i)
#     tock = proc.time() - tick
#     # plot(rf.PM)
#     oob.errors = rf.PM$mse[i]
#     yhat.rf = predict(rf.PM ,newdata = dat[folds==j,],predict.all = T)
#     expyhat.rf = exp(yhat.rf$individual)
#     ytest   = exp(dat[folds==j,]$PM)
#     LU = apply(expyhat.rf,1,quantile,prob=c(0.025,0.975))
# 
#     time.tab = tock[3]
#     mse.test = mean((rowMeans(expyhat.rf) - ytest)^2)
#     mad.test = mean(abs(rowMeans(expyhat.rf)  - ytest))
#     cor.test = cor(rowMeans(expyhat.rf),ytest)
#     cov.test = mean(ytest<LU[2,] & ytest >LU[1,])
# 
#     output = data.frame(oob = oob.errors,time = time.tab, mse = mse.test, mad = mad.test,cor = cor.test, cov = cov.test)
#     cat("i_",i,"j_",j,"\n")
#     # }
# 	save(output, file = paste0("rf_subx_nocmaq_fold",j,"_ntrees",i,".Rda"))
# 	save(rf.PM, file = paste0("allforest_subx_nocmaq_fold",j,"_ntrees",i,".Rda"))
# # }

#
#
#
# 	# summarize the result ----------------------------------------------------
# 	oob.errors = mse.test = mad.test = cor.test = cov.test = time.tab = matrix(NA,nrow = 5, ncol = 5)
# 	for(j in 1:5){
# 	  for(k in 1:5){
# 	    i =  c( 50, 100, 500, 1000, 1500)[k]
# 	    file = paste0("ReviewPaper_RandomForest/log_RF/rf_subx_nocmaq_fold",j,"_ntrees",i,".Rda")
# 	    if(file.exists(file)){
# 	      load(file = file)
# 	      oob.errors[j,k] <- output$oob
# 	      mse.test[j,k]   <- output$mse
# 	      mad.test[j,k]   <- output$mad
# 	      cor.test[j,k]   <- output$cor
# 	      cov.test[j,k]   <- output$cov
# 	      time.tab[j,k]   <- output$time
# 	      rm(output)
# 	    }
# 	  }
# 	}
# # 
# 	round(apply(time.tab,2,mean,na.rm=T)/3600,3)
# 	round(apply(cor.test,2,mean,na.rm=T),3)
	# # changed to log and with coordinates
	# Time: 0.297  0.694  2.823  6.487 10.939
	# MSE: 12.074 11.881 11.772 11.876 12.467
	# MAD: 2.146  2.122   2.109  2.110  2.134
	# COR: 0.829  0.833   0.835  0.833  0.822
	# COV: 0.934  0.949   0.959  0.960  0.960
	
	#  results for first three folds. 
# 	ntrees: 50     100    500    1000    1500
# 	Time:   0.273  0.564  2.616  5.346  8.134
#   MSE:   14.322 14.141 14.000 13.979 13.978
#   MAD:    2.382  2.361  2.345  2.344  2.344
#   cor:    0.791  0.794  0.797  0.797  0.798
#   cov :   0.929  0.943  0.955  0.957  0.956
# 
	# NNET --------------------------------------------------------------------

	load(file="AQS_CMAQ_2011_log_dataframe.RData")
	library(keras)

	dat_fold     <- dat
	fold = 5
	dat_fold[,1] <- exp(dat_fold[,1])
	dat_fold[,colnames(dat_fold)=="CMAQ1"]   <-  exp(dat_fold[,colnames(dat_fold)=="CMAQ1"])
	varnames = colnames(dat)[-c(1,2,32:ncol(dat))]
	# varnames = c("Longitude","Latitude","t",namesx,"CMAQ1")
	# Scale the variables to be in [0,1]
	maxs    <- apply(dat_fold[,colnames(dat)%in%varnames], 2, max)
	mins    <- apply(dat_fold[,colnames(dat)%in%varnames], 2, min)
	#dat_fold[,colnames(dat)%in%varnames]     <- as.data.frame(scale(dat_fold[,colnames(dat)%in%varnames], center = mins, scale = maxs - mins))
	dat_fold[,colnames(dat)%in%varnames]     <- as.data.frame(scale(dat_fold[,colnames(dat)%in%varnames]))
	# Convert the matrix to a data frame
	dat_fold     <- as.data.frame(dat_fold)

	# Make a formula to go into the neural nets package
	names   <- c("PM", namesx, "Latitude", "Longitude", "CMAQ1")
	names <- c("PM", varnames)
  test_names <- c("Site","t")

	dat_test  <- dat_fold[dat_fold$folds==fold,colnames(dat_fold)%in%names]
	dat_train <- dat_fold[dat_fold$folds!=fold,colnames(dat_fold)%in%names]
	save_preds <- dat[dat$folds==fold,colnames(dat_fold)%in%test_names]
  
  
  #cmaq + covs
  {
    Y_test    <-  (dat_test[,1]) 
    X_test    <- dat_test[,colnames(dat_test)%in%varnames]
    Y_train    <-  (dat_train[,1]) 
    X_train    <- dat_train[,colnames(dat_test)%in%varnames]
    
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 2000,activation="relu",input_shape = c(ncol(X_train))) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 100,activation="relu") %>%
      # layer_dropout(rate = 0.3) %>%
      # layer_dense(units = 50,activation="relu") %>%
      # layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr=0.003, decay = 0.0005),
      metrics = c('mse')
    )
    
    
    # Fit model to data
    batch_size <- 1024
    epochs <- 200
    history <- model %>% fit(
      as.matrix(X_train), c(Y_train),
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.1
    )
    
    plot(history)
    
    pred <- data.frame(y = predict(model, as.matrix(X_test)))
    pred <- pred[[1]] 
    save_preds$pred <- pred
    test <- Y_test 
    # pred <- exp(pred[[1]])
    # test <- exp(Y_test)
    plot(pred,test)
    abline(a=0,b=1)
    mse.test = mean((pred - test)^2)
    mad.test = mean(abs(pred - test))
    cor.test = cor(pred,test)
    output = list(mse.test,mad.test,cor.test)
    print(output)
  }
	
	pred_matrix <- matrix(NA, length(unique(dat$Site)), 365)
  rownames(pred_matrix) <- unique(dat$Site)
  for (i in 1:length(pred)){
    pred_matrix[rownames(pred_matrix)==save_preds$Site[i], save_preds$t[i]] <- save_preds$pred[i]
  }
  
  
  #covs
  {
    Y_test    <-  (dat_test[,1]) 
    X_test    <- dat_test[,colnames(dat_test)%in%varnames]
    Y_train    <-  (dat_train[,1]) 
    X_train    <- dat_train[,colnames(dat_test)%in%varnames]
    
    model <- keras_model_sequential()
    model %>%
      layer_dense(units = 200,activation="relu",input_shape = c(ncol(X_train))) %>%
      layer_dropout(rate = 0.4) %>%
      layer_dense(units = 100,activation="relu") %>%
      layer_dropout(rate = 0.3) %>%
      layer_dense(units = 50,activation="relu") %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 1)
    
    model %>% compile(
      loss = 'mean_squared_error',
      optimizer = optimizer_adam(lr=0.006, decay = 0.0005),
      metrics = c('mse')
    )
    
    
    # Fit model to data
    batch_size <- 1024
    epochs <- 200
    history <- model %>% fit(
      as.matrix(X_train), c(Y_train),
      batch_size = batch_size,
      epochs = epochs,
      verbose = 0,
      validation_split = 0.1
    )

	plot(history)

	pred <- data.frame(y = predict(model, as.matrix(X_test)))
	pred <- pred[[1]] 
	save_preds$pred <- pred
	test <- Y_test 
	# pred <- exp(pred[[1]])
	# test <- exp(Y_test)
	plot(pred,test)
	abline(a=0,b=1)
	mse.test = mean((pred - test)^2)
	mad.test = mean(abs(pred - test))
	cor.test = cor(pred,test)
	output = list(mse.test,mad.test,cor.test)
	print(output)
  }

  for (i in 1:length(pred)){
    pred_matrix[rownames(pred_matrix)==save_preds$Site[i], save_preds$t[i]] <- save_preds$pred[i]
  }
  