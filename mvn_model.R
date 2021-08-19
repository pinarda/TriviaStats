source('~/git/Trivia/source/content/trivia_stats_2.R') 

rdf = create_rounds_data_frame(normalize=TRUE)
#rdf = rdf[which(!is.na(rdf$Alex)),]
scores = rdf[,10:18]
default_c = cov(scores, use="pairwise.complete.obs")

# per theme covariancess
#catlist = categories[-c(4, 7, 12)]
#catlist=c("food")
catlist=c("entertainment")
theme_c = array(NA, dim=c(length(players), length(players), length(catlist)))
i=1
for(category in catlist){
  cat_cov = scores[which(tolower(rdf$Major.Category)==category),]
  theme_c[,,i] = cov(cat_cov, use="pairwise.complete.obs")
  i = i+1
}
# name third dimension of array
dimnames(theme_c)[[3]] = catlist
dimnames(theme_c)[[1]] = players
dimnames(theme_c)[[2]] =  players
theme_c

createlist=players
theme_c = array(NA, dim=c(length(players), length(players), length(players)))
i=1
for(creator in createlist){
  create_cov = scores[which(rdf$Creator==creator),]
  theme_c[,,i] = cov(create_cov, use="pairwise.complete.obs")
  i = i+1
}
# name third dimension of array
dimnames(theme_c)[[3]] = createlist
dimnames(theme_c)[[1]] = players
dimnames(theme_c)[[2]] =  players
theme_c

# TRYING TO NORMALIZE THE SCORES TO ADJUST FOR ROUND DIFFICULTY BEFORE COMPUTING COVARIANCE - DOES NOT SEEM TO WORK
# also try median?
#for(player in players){
#  normalized_scores[,player] = scores[,player] - mean(scores[,player], na.rm=TRUE)
#}

#normalized_rdf = rdf
#normalized_rdf[,10:18] = normalized_scores

####
#### MVN Model (uses other player's scores)
####

set.seed(1)
mae_mvns = rep(NA, 50)
bias_mvns = rep(NA, 50)
for(j in 1:50) {
  sample_ind <- sample.int(n=nrow(normalized_rdf), size = floor(.8*nrow(normalized_rdf)), replace = F)
  
  train <- rdf[sample_ind,]
  test <- rdf[-sample_ind,]
  
  test = test[which(!is.na(test$Alex)),]

  meanPlayerScores <- c(mean(train$Zach, na.rm=TRUE), 
                        mean(train$Megan, na.rm=TRUE),
                        mean(train$Ichigo, na.rm=TRUE),
                        mean(train$Jenny, na.rm=TRUE),
                        mean(train$Mom, na.rm=TRUE),
                        mean(train$Dad, na.rm=TRUE),
                        mean(train$Chris, na.rm=TRUE),
                        mean(train$Drew, na.rm=TRUE))
  names(meanPlayerScores) <- c("Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Chris", "Drew")
  
  meanAlex = mean(train$Alex, na.rm=TRUE)
  errors = rep(NA, nrow(test))
  for(i in 1:nrow(test)){
    
    
    
    creator = test[i,]$Creator
    meanAlex = mean(train$Alex[which(train$Creator==creator)], na.rm=TRUE)
    
    if(any(names(meanPlayerScores) == creator)){
      myMeanPlayerScores = meanPlayerScores[-which(names(meanPlayerScores) == creator)]
    } else {
      myMeanPlayerScores = meanPlayerScores
    }
    
    if(creator %in% createlist){
      covTable = theme_c[,,test[i,]$Creator]
      covTable = covTable[-which(dimnames(covTable)[[1]] == creator), -which(dimnames(covTable)[[1]] == creator)]
    } else{
      covTable = default_c
    }
    
    Sigma_XY <- covTable[-which(dimnames(covTable)[[1]] == "Alex"), which(dimnames(covTable)[[1]] == "Alex")]
    Sigma_YX <- covTable[-which(dimnames(covTable)[[1]] == "Alex"), which(dimnames(covTable)[[1]] == "Alex")]
    Sigma_X <- covTable[-which(dimnames(covTable)[[1]] == "Alex"), -which(dimnames(covTable)[[1]] == "Alex")]
    Sigma_Y <- covTable[which(dimnames(covTable)[[1]] == "Alex"), which(dimnames(covTable)[[1]] == "Alex")]
    
    sampleScores = c(test[i,"Zach"], test[i,"Megan"], test[i,"Ichigo"], test[i,"Jenny"], test[i,"Mom"], test[i,"Dad"], test[i,"Chris"], test[i,"Drew"])
    names(sampleScores) = c("Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Chris", "Drew")
    
    if(any(names(sampleScores) == creator)){
      sampleScores = sampleScores[-which(names(sampleScores) == creator)]
    }

    sampleScores[is.na(sampleScores)] <- median(sampleScores, na.rm=TRUE)

    roboAlexMean <-  meanAlex + Sigma_YX %*% solve(Sigma_X) %*% (sampleScores - myMeanPlayerScores)

    #if(roboAlexMean[1][1] > 10){
    #  roboAlexMean[1][1] = 10
    #}
    
    errors[i] = roboAlexMean - test[i, "Alex"]
  }
  mae_mvns[j] = mean(abs(errors), na.rm=TRUE)
  bias_mvns[j] = mean(errors, na.rm=TRUE)
}
mean(mae_mvns)
mean(bias_mvns)
