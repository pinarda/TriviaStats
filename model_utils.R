library( neuralnet)
library(keras)
library(tictoc)
library(fastDummies)
source('~/git/Trivia/source/content/trivia_stats_2.R') 

rdf = create_rounds_data_frame(normalize=TRUE)
rdf = rdf[which(!is.na(rdf$Alex)),]


####
# Control model (mean only)
####

set.seed(1)
mae_means = rep(NA, 50)
for(i in 1:50){
  sample_ind <- sample.int(n=nrow(rdf), size = floor(.8*nrow(rdf)), replace = F)
  
  train <- rdf[sample_ind,]
  test <- rdf[-sample_ind,]
  
  pred = mean(train$Alex, na.rm=TRUE)
  error = pred - test$Alex
  mae_means[i] = mean(abs(error), na.rm=TRUE)
}
mean(mae_means)

#1.891868

# Takes a while, has to load the bias table
source('~/git/Trivia/source/content/trivia_stats.R')
covTable <- get_player_covariance_scores()

mdf = create_rounds_data_frame(normalize=TRUE)
#rdf = rdf[which(!is.na(rdf$Alex)),]
scores = mdf[,10:18]
covTable = cov(scores, use="pairwise.complete.obs")
####
#### MVN Model (uses other player's scores)
####

set.seed(1)
mae_mvns = rep(NA, 50)
for(j in 1:50) {
  sample_ind <- sample.int(n=nrow(rdf), size = floor(.8*nrow(rdf)), replace = F)
  
  train <- rdf[sample_ind,]
  test <- rdf[-sample_ind,]
  
  Sigma_XY <- covTable[c(1:7, 9), 8]
  Sigma_YX <- covTable[c(1:7, 9), 8]
  Sigma_X <- covTable[c(1:7, 9), c(1:7, 9)]
  Sigma_Y <- covTable[8,8]
  
  
  meanPlayerScores <- c(mean(train$Zach, na.rm=TRUE), 
                        mean(train$Megan, na.rm=TRUE),
                        mean(train$Ichigo, na.rm=TRUE),
                        mean(train$Jenny, na.rm=TRUE),
                        mean(train$Mom, na.rm=TRUE),
                        mean(train$Dad, na.rm=TRUE),
                        mean(train$Chris, na.rm=TRUE),
                        mean(train$Drew, na.rm=TRUE))
  
  meanAlex = mean(train$Alex, na.rm=TRUE)
  #meanAlex = mean(train$Alex[which(train$Creator==creator)], na.rm=TRUE)
  errors = rep(NA, nrow(test))
  for(i in 1:nrow(test)){
    sampleScores = c(test[i,"Zach"], test[i,"Megan"], test[i,"Ichigo"], test[i,"Jenny"], test[i,"Mom"], test[i,"Dad"], test[i,"Chris"], test[i,"Drew"])
    sampleScores[is.na(sampleScores)] <- median(sampleScores, na.rm=TRUE)
    #roboAlexMean <-  meanAlex + Sigma_YX[-8] %*% solve(Sigma_X[-8,-8]) %*% (sampleScores - meanPlayerScores)
    roboAlexMean <-  meanAlex + Sigma_YX %*% solve(Sigma_X) %*% (sampleScores - meanPlayerScores)
    
    errors[i] = roboAlexMean - test[i, "Alex"]
  }
  mae_mvns[j] = mean(abs(errors), na.rm=TRUE)
}
mean(mae_mvns)
#1.198945

####
# NN Model
####

set.seed(1)
mae_nn = rep(NA, 10)
for(j in 1:10) {
  sample_ind <- sample.int(n=nrow(rdf), size = floor(.8*nrow(rdf)), replace = F)
  
  #dcols <- dummy_cols(rdf[,c("Major.Category", "Creator", "Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Alex", "Chris", "Drew")])[,-c(1,2)]
  dcols <- rdf[,c("Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Alex", "Chris", "Drew")]
  #dcols <- dummy_cols(rdf[,c("Major.Category", "Creator", "Alex")])[,-c(1,2)]
  train <- dcols[sample_ind,]
  test <- dcols[-sample_ind,]
  nn_train = train[which(!is.na(train$Alex)),]
  nn_test = test[which(!is.na(test$Alex)),]
  
  x_train_cols = nn_train[,-7]
  y_train = nn_train[,"Alex"]
  x_test_cols = nn_test[,-7]
  y_test = nn_test[,"Alex"]
  
  x_train = as.matrix(x_train_cols)
  
  for(i in 1:nrow(x_train)){
    x_train[i,is.na(x_train[i,])] = median(x_train[i,1:8], na.rm=TRUE)
  }
  
  x_test = as.matrix(x_test_cols)
  
  for(i in 1:nrow(x_test)){
    x_test[i,is.na(x_test[i,])] = median(x_test[i,1:8], na.rm=TRUE)
  }
  
  model <- keras_model_sequential() 
  model %>% 
    #layer_dense(units = 256, activation = "relu", input_shape = c(34)) %>% 
    layer_dense(units = 256, activation = "relu", input_shape = c(8)) %>% 
    layer_dropout(rate = 0.1) %>%
    #layer_dense(units = 256, activation = "relu", input_shape = c(26)) %>% 
    layer_dense(units = 1, activation="linear")
  
  model %>% compile(
    loss = "mean_absolute_error",
    optimizer = optimizer_rmsprop(lr=0.0001),
    metrics = c("mean_squared_error")
  )
  
  history <- model %>% fit(
    x_train, y_train, 
    epochs = 75, batch_size = 50, 
    validation_split = 0.2
  )
  
  preds = model %>% predict(x_test)
  errors = preds - y_test
  mean(abs(errors))
  mae_nn[j] = mean(abs(errors))
}
mean(mae_nn)
# For just players, no rounds: 1.220265 (sample size=10)
# For players and categories and creators:  1.207861 (batch size 50), sample size=10]


####
#### MVN Model (uses other player's scores)
####

set.seed(1)
mae_mvns = rep(NA, 50)
for(j in 1:50) {
  sample_ind <- sample.int(n=nrow(rdf), size = floor(.8*nrow(rdf)), replace = F)
  
  train <- rdf[sample_ind,]
  test <- rdf[-sample_ind,]
  
  Sigma_XY <- covTable[c(1:7, 9), 8]
  Sigma_YX <- covTable[c(1:7, 9), 8]
  Sigma_X <- covTable[c(1:7, 9), c(1:7, 9)]
  Sigma_Y <- covTable[8,8]
  
  
  meanPlayerScores <- c(mean(train$Zach, na.rm=TRUE), 
                        mean(train$Megan, na.rm=TRUE),
                        mean(train$Ichigo, na.rm=TRUE),
                        mean(train$Jenny, na.rm=TRUE),
                        mean(train$Mom, na.rm=TRUE),
                        mean(train$Dad, na.rm=TRUE),
                        mean(train$Chris, na.rm=TRUE),
                        mean(train$Drew, na.rm=TRUE))
  
  #meanAlex = mean(train$Alex, na.rm=TRUE)
  m = mean(train$Alex, na.rm=TRUE)
  adf = alex_round_means() 
  cdf = alex_creator_means()
  #bias = alex_round_bias()
  errors = rep(NA, nrow(test))
  for(i in 1:nrow(test)){
    
    sampleScores = c(test[i,"Zach"], test[i,"Megan"], test[i,"Ichigo"], test[i,"Jenny"], test[i,"Mom"], test[i,"Dad"], test[i,"Chris"], test[i,"Drew"])
    category = tolower(test[i, ]$Major.Category)
    meanAlex = adf$medians[which(adf$categories == category)]
    
    # USELESS??
    creator = tolower(test[i, ]$Creator)
    creator_adjust = cdf$scores[which(cdf$creator == creator)]
    #meanAlex = creator_adjust
 
    #meanAlex = m + bias$scores[which(bias$categories == category)] + (0.25*creator_adjust)
    
    
    sampleScores[is.na(sampleScores)] <- median(sampleScores, na.rm=TRUE)
    
    roboAlexMean <-  meanAlex + Sigma_YX %*% solve(Sigma_X) %*% (sampleScores - meanPlayerScores)
    
    #if(creator=="ichigo"){
      c_adjust = cdf$scores-mean(cdf$scores) 
      k_adjust = adf$scores - mean(adf$scores)
      roboAlexMean = roboAlexMean +c_adjust[which(cdf$creators == creator)] #+ k_adjust[which(adf$categories == category)]
    #}
    
    errors[i] = roboAlexMean - test[i, "Alex"]
  }
  mae_mvns[j] = mean(abs(errors), na.rm=TRUE)
}
mean(mae_mvns)
# 1.156495

# ONLY USING CATEGORY MEANS, NO ACCOUNTING FOR OTHER SCORES
# 1.781642
