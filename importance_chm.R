library(NeuralNetTools)

quant <- c(0.25,0.5,0.75,0.95)
impor <- list()
for (i in 1:4){
  model <- keras_model_sequential()
  model %>% 
    layer_dense(units = 500, activation = 'relu', input_shape = c(dim(input)[2])) %>% 
    layer_dropout(rate = 0.5) %>% 
    # #layer_batch_normalization() %>%
    #layer_dense(units = 12, activation = 'relu') %>%
    # layer_dropout(rate = 0.1) %>%
    #layer_dense(units = 12, activation = 'tanh') %>%
    # #layer_dense(units = 6, activation = 'tanh') %>%
    # #layer_dropout(rate = 0.1) %>%
    # #layer_batch_normalization() %>%
    layer_dense(units = 1)
  
  #summary(model)
  
  model %>% compile(
    loss = function(y_true, y_pred) tilted_loss(quant[i], y_true, y_pred),
    optimizer = optimizer_adam(lr = 0.0005),
    metrics = c('mse')
  )
  
  # Fit model to data
  history <- model %>% fit(
    input, y_norm,
    batch_size = 16,
    epochs = 30,
    verbose = 0,
    #validation_split = 0.2
    validation_data = list(input_pre, y.ho)
  )
  
  ######garson 50k mse ########
  wts <- get_weights(model)
  weights <- rbind(wts[[2]], wts[[1]])
  weights <- as.vector(weights)
  w2 <- c(wts[[4]], wts[[3]])
  weights <- c(weights,w2)
  struct <- c(dim(input_pre)[2],500,1)
  impor[[i]] <- as.matrix(garson(weights, struct = struct, bar_plot = F))
}

## kriging and neighbors
i=4
importance_k_25_non <- c(impor[[i]][1] + (impor[[i]][11] + impor[[i]][12]) +impor[[i]][33],
                     impor[[i]][2] + (impor[[i]][13] + impor[[i]][14]) +impor[[i]][34],
                     impor[[i]][3] + (impor[[i]][15] + impor[[i]][16]) +impor[[i]][35],
                     impor[[i]][4] + (impor[[i]][17] + impor[[i]][18] ) +impor[[i]][36],
                     impor[[i]][5] + (impor[[i]][19] + impor[[i]][20] ) +impor[[i]][37],
                     impor[[i]][6] + (impor[[i]][21] + impor[[i]][22] ) +impor[[i]][38] ,
                     impor[[i]][7] + (impor[[i]][23] + impor[[i]][24] ) +impor[[i]][39],
                     impor[[i]][8] + (impor[[i]][25] + impor[[i]][26] ) +impor[[i]][40],
                     impor[[i]][9] + (impor[[i]][27] + impor[[i]][28] ) +impor[[i]][41],
                     impor[[i]][10] + (impor[[i]][29] + impor[[i]][30] ) +impor[[i]][42])

importance_k_5_non <- c(impor[[i]][1] + (impor[[i]][11] + impor[[i]][12]) +impor[[i]][33],
                    impor[[i]][2] + (impor[[i]][13] + impor[[i]][14]) +impor[[i]][34],
                    impor[[i]][3] + (impor[[i]][15] + impor[[i]][16]) +impor[[i]][35],
                    impor[[i]][4] + (impor[[i]][17] + impor[[i]][18] ) +impor[[i]][36],
                    impor[[i]][5] + (impor[[i]][19] + impor[[i]][20] ) +impor[[i]][37],
                    impor[[i]][6] + (impor[[i]][21] + impor[[i]][22] ) +impor[[i]][38] ,
                    impor[[i]][7] + (impor[[i]][23] + impor[[i]][24] ) +impor[[i]][39],
                    impor[[i]][8] + (impor[[i]][25] + impor[[i]][26] ) +impor[[i]][40],
                    impor[[i]][9] + (impor[[i]][27] + impor[[i]][28] ) +impor[[i]][41],
                    impor[[i]][10] + (impor[[i]][29] + impor[[i]][30] ) +impor[[i]][42])

importance_k_75_non <- c(impor[[i]][1] + (impor[[i]][11] + impor[[i]][12]) +impor[[i]][33],
                     impor[[i]][2] + (impor[[i]][13] + impor[[i]][14]) +impor[[i]][34],
                     impor[[i]][3] + (impor[[i]][15] + impor[[i]][16]) +impor[[i]][35],
                     impor[[i]][4] + (impor[[i]][17] + impor[[i]][18] ) +impor[[i]][36],
                     impor[[i]][5] + (impor[[i]][19] + impor[[i]][20] ) +impor[[i]][37],
                     impor[[i]][6] + (impor[[i]][21] + impor[[i]][22] ) +impor[[i]][38] ,
                     impor[[i]][7] + (impor[[i]][23] + impor[[i]][24] ) +impor[[i]][39],
                     impor[[i]][8] + (impor[[i]][25] + impor[[i]][26] ) +impor[[i]][40],
                     impor[[i]][9] + (impor[[i]][27] + impor[[i]][28] ) +impor[[i]][41],
                     impor[[i]][10] + (impor[[i]][29] + impor[[i]][30] ) +impor[[i]][42])

importance_k_95_non <- c(impor[[i]][1] + (impor[[i]][11] + impor[[i]][12]) +impor[[i]][33],
                     impor[[i]][2] + (impor[[i]][13] + impor[[i]][14]) +impor[[i]][34],
                     impor[[i]][3] + (impor[[i]][15] + impor[[i]][16]) +impor[[i]][35],
                     impor[[i]][4] + (impor[[i]][17] + impor[[i]][18] ) +impor[[i]][36],
                     impor[[i]][5] + (impor[[i]][19] + impor[[i]][20] ) +impor[[i]][37],
                     impor[[i]][6] + (impor[[i]][21] + impor[[i]][22] ) +impor[[i]][38] ,
                     impor[[i]][7] + (impor[[i]][23] + impor[[i]][24] ) +impor[[i]][39],
                     impor[[i]][8] + (impor[[i]][25] + impor[[i]][26] ) +impor[[i]][40],
                     impor[[i]][9] + (impor[[i]][27] + impor[[i]][28] ) +impor[[i]][41],
                     impor[[i]][10] + (impor[[i]][29] + impor[[i]][30] ) +impor[[i]][42])

importance_50k <- c(imp[31],imp[1] + (imp[11] + imp[12]) +imp[34],
                    imp[2] + (imp[13] + imp[14]) +imp[35],
                    imp[3] + (imp[15] + imp[16]) +imp[36],
                    imp[4] + (imp[17] + imp[18] ) +imp[37],
                    imp[5] + (imp[19] + imp[20] ) +imp[38],
                    imp[6] + (imp[21] + imp[22] ) +imp[39] ,
                    imp[7] + (imp[23] + imp[24] ) +imp[40],
                    imp[8] + (imp[25] + imp[26] ) +imp[41],
                    imp[9] + (imp[27] + imp[28] ) +imp[42],
                    imp[10] + (imp[29] + imp[30] ) +imp[43])

importance_50k_non <- c(imp[1] + (imp[11] + imp[12]) +imp[33],
                    imp[2] + (imp[13] + imp[14]) +imp[34],
                    imp[3] + (imp[15] + imp[16]) +imp[35],
                    imp[4] + (imp[17] + imp[18] ) +imp[36],
                    imp[5] + (imp[19] + imp[20] ) +imp[37],
                    imp[6] + (imp[21] + imp[22] ) +imp[38] ,
                    imp[7] + (imp[23] + imp[24] ) +imp[39],
                    imp[8] + (imp[25] + imp[26] ) +imp[40],
                    imp[9] + (imp[27] + imp[28] ) +imp[41],
                    imp[10] + (imp[29] + imp[30] ) +imp[42])

nn_neibor <- as.data.frame(cbind(as.factor(1:10),importance_k_25_non[order(-importance_k_25_non)],
                                 importance_k_5_non[order(-importance_k_5_non)],
                                 importance_k_75_non[order(-importance_k_75_non)], 
                                 importance_k_95_non[order(-importance_k_95_non)],
                           importance_50k_non[order(-importance_50k_non)]))
nn_kriging <- as.data.frame(cbind("Kriging",c(importance_k_25_non[1], importance_k_5_non[1],
                              importance_k_75_non[1], importance_k_95_non[1], importance_50k_non[1]),
                              c("q25","q50", "q75", "q95", "mean")))
nn_loc <- as.data.frame(cbind("s",c(1 - sum(importance_k_25_non),1- sum(importance_k_5_non),
                                    1-sum(importance_k_75_non), 1-sum(importance_k_95_non),
                                    1-sum(importance_50k_non)),
                              c("q25","q50", "q75", "q95", "mean")))
names(nn_kriging) <- c("Neighbors", "value","variable")
names(nn_loc) <- c("Neighbors", "value","variable")
names(nn_neibor) <-  c("Neighbors","q25","q50", "q75", "q95", "mean")
nn_neibor$Neighbors <- as.factor(nn_neibor$Neighbors)
nn_kriging$Neighbors <- as.factor(nn_kriging$Neighbors)
nn_kriging$value <- as.numeric(levels(nn_kriging$value))[nn_kriging$value]
nn_loc$Neighbors <- as.factor(nn_loc$Neighbors)
nn_loc$value <- as.numeric(levels(nn_loc$value))[nn_loc$value]


library(reshape2)
d <- melt(nn_neibor, id.vars="Neighbors")

ggplot(d, aes(Neighbors,value, col=variable, group = variable)) + geom_point(size=3) +
  labs(y = "Importance")+  
  
  #geom_point(data = nn_kriging, aes(y=value),size=4) + 
  geom_point(data = nn_loc, aes(y=value),size=4) + 
  geom_line(size = 1) + 
  scale_color_discrete(name="Models",
                       breaks=c("q25","q50", "q75", "q95", "mean"), labels=c("25% quantile", "50% quantile",
                                                "75% quantile", "95% quantile", "mse")) +
  scale_x_discrete(limits=c("s",1:10)) + 
  theme(legend.position = c(0.9, 0.8))