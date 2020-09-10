library(plyr)
library(rlang)
library(plotly)
library(ggplot2)
library(RColorBrewer)
library(heatmaply)
library(forcats)
library(dplyr)
library(ggridges)
library(gplots) # not to be confused with `ggplot2`, which is a very different package



# should match excel sheet names
dates=c("March18", "March25", "April1", "April8", "April15", "April22", "April29", "May6", "May13", "May20", "May27", "June3", "June10", "June24", "July2", "July9", "July15", "July22", "July29", "August5", "August12", "August19", "September3", "September9")

# to get id:
# right click on file in drive -> get shareable link -> copy id substring
google_ids=c("1B8jDE1_m9ilwz2c3L__vRBVNlFRkYFY-", "1oYpePFo3TofyYfukndqi7cNR7d19NH9Y", "1qZAzHPoLqWtY-MwiiEzC2cwd2f57uCiP", "1SXQ46m6kqOu3N7uzghNcscKAe171TDz5", "1BSo_bW-Armwnn6qy3fPTimGLontMTTVa", "1mrl_UgL-PGat2d586NfNlXT6OfXkXDtZ", "11RXp1dqKN8DDiFfq0lKrACmbeb_yb-W2", "1KuFoEDJFprCrkGUzE7Bp_9V5Q_nDuA4T", "11RkmBiMfCrOeV1dY2C3l_KUTqAXT8s8R", "1-N2R3ZJp-ahf7WRJa9ODjVP7QaTNHShD", "1J0Kx3Kf-4kTb-tjvxbDMsz0gtU_CoFMM", "1gK9k4eVfbQ1U7SuQD9CWrysrIapHSK_Y", "1SlbPlTn8iIj1OA7pAkK9C9IepnX_rzob", "1MdkoCG544dbT0gwGNFNMwFTIHQjgW3Bg", "1mrwkdVQKv1wFMICeyE_eCD1NyAmpcRii", "14ldPagW4XGJTkKqv916fuQY3fB-lqqMn", "1v0GG0YjOwkSr1HML68P8cLTjtj8G8Owp", "1kEMwWBd0OWxnSdqjHarap-Oh0fT7rGR9", "1XpcLhGJGZ1Pphk_EXJBgB9njYkm9Y0G4", "1SLj6bd9OBZTbXwIpSHcxPg75mvBqA8jo")

players=c("Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Chris", "Alex", "Jeff", "Drew")

IS_GOOGLE_UPSET = TRUE

get_filename <- function(id){
  if(IS_GOOGLE_UPSET){
    f = sprintf("/Users/alex/Downloads/%s.csv", id)
  } else {
    f = sprintf("https://docs.google.com/uc?id=%s&export=download", id)
  }
  return(f)
}

get_overall_raw_mean <- function(score_frame){
  return(mean(subset(na.omit(score_frame$Score),na.omit(score_frame$Score) >0)))
}


get_overall_normalized_mean <- function(score_frame, date){
  raw_mean = get_overall_raw_mean(score_frame)
  normalized_mean = raw_mean
  if (date %in% c("March18", "March25")){
    normalized_mean = raw_mean * 0.9
  }
  return(normalized_mean)
}

get_judgement_score_on_date <- function(date, player){
  score_frame = as.data.frame(scores[which(dates==date)])
  index = which(score_frame$Player==player)

  val = score_frame$Joker.Bonus[index] - mean(c(score_frame$X1[index], score_frame$X2[index],score_frame$X3[index],score_frame$X4[index],score_frame$X5[index],score_frame$X6[index],score_frame$X7[index],score_frame$X8[index]), na.rm=TRUE)
  if(identical(val, numeric(0))){
    return(NA)
  }
  return(val)
}

get_judgement_score_by_player <- function(player){
  js = sapply(dates, get_judgement_score_on_date, player=player)
  return(mean(js, na.rm=TRUE))
}

get_judgement_table <- function(){
  t = sapply(players, get_judgement_score_by_player)
  coul <- colorRampPalette(brewer.pal(8, "YlGn"), bias=1)(25)
  t = as.matrix(sort(t, decreasing=TRUE))
  
  heatmaply(t, limits=c(0, 1.9),dendrogram = "none", colors=coul, ylab="Player", main="Average points gained by using judgement to select joker", showticklabels = c(FALSE, TRUE))
  
}

get_round_score <- function(player, creator, date){
  score_frame = as.data.frame(scores[which(dates==date)])
  # creator's row in dataframe
  creator_row_index = which(score_frame$Player==creator)
  # creator's round number
  creators_round = score_frame$Creator.Round[creator_row_index]
  # Err if player is not in scoresheet or player did not make a round
  if(is.integer(creators_round) && length(creators_round) == 0L || is.na(creators_round)){
    return(NA)
  }
  # scores on creator's round
  creators_scores = score_frame[creators_round+4]
  # player index in creators_scores
  index = which(score_frame$Player==player)
  # Err if player is not in scoresheet
  if(is.integer(index) && length(index) == 0L){
    return(NA)
  }
  
  round_score = unlist(creators_scores, use.names=FALSE)[index]
  
  return(round_score)
}

correct_joker_score_by_date_and_player <- function(date, player){
  correct_jokers = 0
  play_count = 0
  score_frame = as.data.frame(scores[which(dates==date)])
  

  
  index = which(score_frame$Player==player)
  
  # exit if not in scoresheet
  if(is.integer(index) && length(index) == 0L || is.na(index)){
    return(c(play_count, correct_jokers))
  }
  
  # exit if no rounds scores
  if(max(score_frame$X1[index], score_frame$X2[index], score_frame$X3[index], score_frame$X4[index], score_frame$X5[index], score_frame$X6[index], score_frame$X7[index], score_frame$X8[index], na.rm=TRUE) == -Inf){
    return(c(play_count, correct_jokers))
  }
  
  play_count = play_count + 1
  if (max(score_frame$X1[index], score_frame$X2[index], score_frame$X3[index], score_frame$X4[index], score_frame$X5[index], score_frame$X6[index], score_frame$X7[index], score_frame$X8[index], na.rm=TRUE) == score_frame$Joker.Bonus[index]){
    correct_jokers=correct_jokers+1
  }
  return(c(play_count, correct_jokers))
}

ichigo_coefficient <- function(date, player){ 
  score_frame = as.data.frame(scores[which(dates==date)])
  index = which(score_frame$Player==player)
  ichigo_index = which(score_frame$Player=="Ichigo")
  ichigo_joker_round = score_frame$Joker.Round[ichigo_index]
  
  if(is.integer(ichigo_index) && length(ichigo_index) == 0L){
    return(NA)
  }
  if(is.integer(index) && length(index) == 0L){
    return(NA)
  }
  if(is.na(score_frame[index, ichigo_joker_round + 4])){
    return(0)
  }
  
  return(score_frame[index, ichigo_joker_round + 4] - as.numeric(score_frame$Joker.Bonus[index]))
}

ichigo_coefficient_by_player <- function(player){
  is = sapply(dates, ichigo_coefficient, player=player)
  return(mean(is, na.rm=TRUE))
}

ichigo_coefficient_table <- function(){
  ist = sapply(players, ichigo_coefficient_by_player)
  coul <- colorRampPalette(brewer.pal(8, "PiYG"), bias=1)(25)
  ist = as.matrix(sort(ist, decreasing=TRUE))
  
  heatmaply(ist, limits=c(-1.1, 1.1),dendrogram = "none", colors=coul, ylab="Player", main="Points gained by copying Ichigo's Joker", showticklabels = c(FALSE, TRUE))
}


chris_coefficient <- function(date, player){ 
  score_frame = as.data.frame(scores[which(dates==date)])
  index = which(score_frame$Player==player)
  chris_index = which(score_frame$Player=="Chris")
  chris_joker_round = score_frame$Joker.Round[chris_index]
  
  if(is.integer(chris_index) && length(chris_index) == 0L){
    return(NA)
  }
  if(is.integer(index) && length(index) == 0L){
    return(NA)
  }
  if(is.null(score_frame[index, chris_joker_round + 4]) || is.na(score_frame[index, chris_joker_round + 4])){
    return(0)
  }
  
  return(score_frame[index, chris_joker_round + 4] - as.numeric(score_frame$Joker.Bonus[index]))
}

chris_coefficient_by_player <- function(player){
  is = sapply(dates, chris_coefficient, player=player)
  return(mean(is, na.rm=TRUE))
}

chris_coefficient_table <- function(){
  ist = sapply(players, chris_coefficient_by_player)
  coul <- colorRampPalette(brewer.pal(8, "PiYG"), bias=1)(25)
  ist = as.matrix(sort(ist, decreasing=TRUE))
  
  heatmaply(ist, limits=c(-1.1, 1.1),dendrogram = "none", colors=coul, ylab="Player", main="Points gained by copying Chris's Joker", showticklabels = c(FALSE, TRUE))
}

jenny_coefficient <- function(date, player){ 
  score_frame = as.data.frame(scores[which(dates==date)])
  index = which(score_frame$Player==player)
  jenny_index = which(score_frame$Player=="Jenny")
  jenny_joker_round = score_frame$Joker.Round[jenny_index]
  
  if(is.integer(jenny_index) && length(jenny_index) == 0L){
    return(NA)
  }
  if(is.integer(index) && length(index) == 0L){
    return(NA)
  }
  if(is.null(score_frame[index, jenny_joker_round + 4]) || is.na(score_frame[index, jenny_joker_round + 4])){
    return(0)
  }
  
  return(score_frame[index, jenny_joker_round + 4] - as.numeric(score_frame$Joker.Bonus[index]))
}

jenny_coefficient_by_player <- function(player){
  is = sapply(dates, jenny_coefficient, player=player)
  return(mean(is, na.rm=TRUE))
}

jenny_coefficient_table <- function(){
  ist = sapply(players, jenny_coefficient_by_player)
  coul <- colorRampPalette(brewer.pal(8, "PiYG"), bias=1)(25)
  ist = as.matrix(sort(ist, decreasing=TRUE))
  
  heatmaply(ist, limits=c(-1.1, 1.1),dendrogram = "none", colors=coul, ylab="Player", main="Points gained by copying Jenny's Joker", showticklabels = c(FALSE, TRUE))
}

correct_joker_score_by_player <-function(player){
  scores_1 = sapply(dates, correct_joker_score_by_date_and_player, player=rep(player, length(dates)))
  return(sum(scores_1[2,])/sum(scores_1[1,]))
}

correct_jokers <- function(){
  scores_2 = sapply(players, correct_joker_score_by_player)
  return(sort(scores_2, decreasing=TRUE))
}

correct_joker_plot <- function(){
  cj = as.matrix(correct_jokers())
  coul <- colorRampPalette(brewer.pal(8, "Blues"))(25)
  heatmaply(cj, dendrogram = "none", colors=coul, ylab="Player", main="% of Correct Jokers", showticklabels = c(FALSE, TRUE))
}

get_creator_round_scores <- function(creator, date){
  score_frame = as.data.frame(scores[which(dates==date)])
  # creator's row in dataframe
  creator_row_index = which(score_frame$Player==creator)
  # creator's round number
  creators_round = score_frame$Creator.Round[creator_row_index]
  # Err if player is not in scoresheet or player did not make a round
  if(is.integer(creators_round) && length(creators_round) == 0L || is.na(creators_round)){
    return(NA)
  }
  # scores on creator's round
  creators_scores = score_frame[creators_round+4]
  return (creators_scores)
}

get_final_score <- function(player, date){
  score_frame = as.data.frame(scores[which(dates==date)])
  index = which(score_frame$Player==player)
  # Err if player is not in scoresheet
  if(is.integer(index) && length(index) == 0L){
    return(NA)
  }
  final_score = score_frame$Score[index]
  return(final_score)
}


get_normalized_final_score <- function(player, date){
  raw_score = get_final_score(player, date)
  normalized_score = raw_score
  if (date %in% c("March18", "March25")){
    normalized_score = raw_score * 0.9
  }
  return(normalized_score)
}

get_all_scores_by_player <- function(player){
  l = lapply(players, function(creator){data.frame(day = dates, val = sapply(dates, get_round_score, player=player, creator=creator), class=creator)})
  ka=rbind(l[[1]], l[[2]], l[[3]], l[[4]], l[[5]], l[[6]], l[[7]], l[[8]], l[[9]], l[[10]])
  return(ka)
}

scores_boxplot_player <- function(player){
  s = get_all_scores_by_player(player)
  s <- na.omit(s)
  s <- s[s$class != player,]
  
  #s %>%
  #  mutate(name = fct_reorder(class, val, .fun="mean")) %>%
  #  ggplot( aes(x=reorder(class, val), y=val, fill=class)) + 
  #  geom_boxplot() +
  #  xlab("creator") +
  #  theme(legend.position="none") +
  #  ylab("score")
  
  s %>%
    mutate(name = fct_reorder(class, val, .fun="mean")) %>%
    ggplot( aes(y=reorder(class, val), x=val, fill=class)) + 
    geom_density_ridges(alpha=0.6, stat="binline", bins=8) +
    theme_ridges() +
    xlab("Score") +
    theme(legend.position="none",
          panel.spacing = unit(0.2, "lines"),
          strip.text.x = element_text(size = 8)) +
    ylab("Creator") + 
    ggtitle(paste(player, "'s Scores by Creator")) +
    theme(plot.title=element_text(vjust=0.5))
}

get_mean_round_score <- function(player, creator){
  #return(mean(mapply(get_round_score, player, creator, dates), na.rm=TRUE))
  return(mean(sapply(dates, get_round_score, player=player, creator=creator), na.rm=TRUE))
}

get_winner <- function(date){
  score_frame = as.data.frame(scores[which(dates==date)])
  player_w=score_frame$Player[which(score_frame$Score == max(score_frame$Score, na.rm=TRUE))]
  if (date=="May27"){
    player_w = "Dad"
  }
  return(player_w)
}

get_winner_wo_joker <- function(date){
  score_frame = as.data.frame(scores[which(dates==date)])
  player_w=score_frame$Player[which(score_frame$Score - score_frame$Joker.Bonus == max(score_frame$Score - score_frame$Joker.Bonus, na.rm=TRUE))]
  return(player_w)
}


joker_win_percent <- function(){
  wj = sapply(dates, get_winner)
  woj = sapply(dates, get_winner_wo_joker)
  return((1 - sum(wj == woj)/length(dates)) * 100)
}

get_mean_by_player <- function(player){
  scores = (sapply(players, get_mean_round_score, player=player))
  return (scores)
}

get_mean_by_creator <- function(creator){
  scores = (sapply(players, get_mean_round_score, creator=creator))
  return (scores)
}

get_joker_bonus_by_night <- function(date){
  score_frame = as.data.frame(scores[which(dates==date)])
  jokers = score_frame$Joker.Bonus
  jokers = jokers[!is.na(jokers)]
  jokers = jokers[jokers>0]
  
  return(jokers)
}

get_all_joker_bonuses <- function(){
  js = sapply(dates, get_joker_bonus_by_night)
  return(js)
}

get_creator_score <- function(creator){
  return (mean(get_mean_by_creator(creator)[!is.nan(get_mean_by_creator(creator))], na.omit=TRUE))
}

get_player_score <- function(player){
  return (mean(get_mean_by_player(player)[!is.nan(get_mean_by_player(player))], na.omit=TRUE))
}

joker_by_player <- function(player){
  joker_by_player = get_mean_by_player(player) - get_player_score(player)
  return(joker_by_player)
}


joker_by_creator <- function(creator){
  joker_by_creator = get_mean_by_creator(creator) - sapply(players, get_player_score)
  return(joker_by_creator)
}


bias_by_creator <- function(creator){
  bias = joker_by_creator(creator) - round_difficulty_by_player()[creator]
  return (bias)
}

bias_by_player <- function(player){
  bias = joker_by_player(player) - round_difficulty_by_player()
  return(bias)
}

normalized_mean_by_night <- function(){
  return(mapply(get_overall_normalized_mean, scores, dates))
}

normalized_mean_final_score_to_date <- function(){
  means = normalized_mean_by_night()
  means_to_date = sapply(seq(from=1, to=length(dates)), function(i){mean(means[1:i])})
  return (means_to_date)
}


final_scores_by_night <- function(){
  score_table=sapply(dates, function(date){sapply(players,function(player, date){get_final_score(player, date)}, date=date)})
  score_table[score_table == 0] <- NA
  return(score_table)
}

performance_by_player <- function(player){
  #normalized final_score_by_date_and_player
  scores_by_date = sapply(dates, function(player, date){get_normalized_final_score(player, date)}, player=player)
  scores_by_date[scores_by_date<=23] <- NA
  #normalized_mean_final_score_to_date_by_player
  means_to_date = sapply(seq(from=1, to=length(dates)), function(i){return(mean(scores_by_date[1:i], na.rm=TRUE))})
  new_means_to_date = prepend(means_to_date, means_to_date[1])
  perf = scores_by_date - (head(new_means_to_date, -1) * night_difficulty_to_date())
  perf[is.nan(perf)] <- 0
  return (perf)
}


performance_table <- function(){
  t(sapply(players, performance_by_player))
}

bias_table <- function(){
  return(sapply(players, bias_by_creator))
}

joker_table <- function(){
  return(sapply(players, joker_by_creator))
}


round_difficulty_by_player <- function(){
  return(sapply(players, function(creator){mean(joker_by_creator(creator)[!is.nan(joker_by_creator(creator))])}))
}

player_scores <- function(){
  sapply(players, get_player_score)
}

creator_scores <- function(){
  sapply(players, get_creator_score)
}

mean_table <- function(){
  sapply(players, get_mean_score_by_player)
}

night_difficulty_to_date <- function(){
  means_to_date = normalized_mean_final_score_to_date()
  new_means_to_date = prepend(means_to_date, means_to_date[1])
  return(normalized_mean_by_night() / head(new_means_to_date, -1))
}

# get the normalized  mean score for a night
get_normalized_mean_overall <- function(score_frame, date){
  raw_mean = get_overall_raw_mean(score_frame)
  normalized_mean = raw_mean
  if (date %in% c("March18", "March25")){
    normalized_mean = raw_mean * 0.9
  }
  return(normalized_mean)
}


night_difficulty <- function(){
  overall_mean = mean(mapply(get_normalized_mean_overall, date=dates, score_frame=scores))
  nightly_mean = mapply(get_overall_normalized_mean, date=dates, score_frame = scores)
  return(nightly_mean / overall_mean)
}

generate_player_plot <- function(player){
  
  scores_by_date = sapply(dates, function(player, date){get_normalized_final_score(player, date)}, player=player) / night_difficulty()
  #scores_by_date = sapply(dates, function(player, date){get_final_score(player, date)}, player=player) / night_difficulty()
  
  scores_by_date = scores_by_date[scores_by_date>24]
  #plot(seq(1, length(scores_by_date)), scores_by_date, type='b')
  plot(seq(1, length(scores_by_date)), scores_by_date, xlab = "Trivia Week", ylab="Total Score", main = paste("Normalized Scores by Week for", player))
  #scores_by_date_2 = sapply(dates, function(player, date){get_normalized_final_score(player, date)}, player="Jenny") / night_difficulty()
  scores_by_date = scores_by_date[scores_by_date>24]
  #scores_by_date_2 = scores_by_date_2[scores_by_date_2>24]
  
  #points(scores_by_date_2, col=2)
  
  regression = lm(scores_by_date ~  seq(1, length(scores_by_date)))
  
  #regression_2 = lm(scores_by_date_2 ~  seq(1, length(scores_by_date_2)))
  abline(regression, col="red")
  #abline(regression_2, col=2)
  summary(regression)
}

generate_player_plot_absolute <- function(player){
  #Absolute scores for Alex(see final_scores_by_night(), row10)
  scores_by_date = final_scores_by_night()[8,]

  #scores_by_date = sapply(dates, function(player, date){get_final_score(player, date)}, player=player) / night_difficulty()

  scores_by_date = scores_by_date[scores_by_date>24]
  plot(seq(1, length(scores_by_date)), scores_by_date, type='b', xlab = "Trivia Week", ylab="Total Score", main = paste("Raw Scores by Week for", player))
  regression = lm(scores_by_date ~  seq(1, length(scores_by_date)))
  abline(regression, col='red')
  summary(regression)
}
  
generate_score_plot <- function(){
  scores_by_date = mapply(get_normalized_mean_overall, score=scores, date=dates)
  # divide scores_by_date by 64 (or total possible score) once normalization is complete
  plot(seq(1, length(scores_by_date)), scores_by_date, xlab = "Trivia Week", ylab="Total Score", main = "Final Scores by Week")
  regression = lm(scores_by_date ~  seq(1, length(scores_by_date)))
  abline(regression, col="red")
  summary(regression)
  
  # partially transparent points by setting `alpha = 0.5`
  # ggplot(data.frame(seq(1, length(scores_by_date)), scores_by_date), aes(seq(1, length(scores_by_date)), scores_by_date)) +
  #   geom_point(position=position_jitter(w=0.05, h=0.05),
  #              shape = 21, alpha = 0.5, size = 3, colour="#01587A", fill="#077DAA") +
  #   theme_bw() +
  #   xlab("Week") + 
  #   ylab("Total Score") + 
  #   ggtitle(label=paste("How well everyone did on's Rounds"), subtitle = "(red points represent the average)") + 
  #   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

generate_creator_plot <- function(creator){ 
  u = sapply(dates, function(creator, date){s = get_creator_round_scores(creator, date)
                                            t = unlist(s[!is.na(s)], use.names=FALSE)}, creator=creator)
  xs <- double()
  ys <- double()
  means <- double()
  for (i in 1:length(dates)){
    d = dates[i]
    xvals = rep(i, length(unlist(u[d], use.names = FALSE)))
    yvals = unlist(u[d], use.names = FALSE)
    xs <- c(xs, xvals)
    ys <- c(ys, yvals)
    means <- c(means, mean(yvals))
  }
  
  mean_data = data.frame(seq(1:length(means)), means)
  # partially transparent points by setting `alpha = 0.5`
   p <- ggplot(data.frame(xs, ys), aes(xs, ys)) +
    geom_point(position=position_jitter(w=0.05, h=0.05),
               shape = 21, alpha = 0.5, size = 3, colour="#01587A", fill="#077DAA") +
    theme_bw() +
    geom_point(data=mean_data, mapping=aes(x=seq(1:length(means)), y=means), colour="#ed7474", shape = 21, alpha = 1, size = 3, fill="#b05656") +
    geom_line(data=mean_data,aes(x=seq(1:length(means)), y=means), colour="#ed7474") + 
    xlab("Trivia Week") + 
    ylab("Round Score") + 
    ggtitle(label=paste("How well everyone did on", creator, "'s rounds"), subtitle = "(red points represent the average)") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
    ylim(-0.1, 10.1) 
   
   ggplotly(p)
}

creator_mean_data <- function(creator){
  u = sapply(dates, function(creator, date){s = get_creator_round_scores(creator, date)
  t = unlist(s[!is.na(s)], use.names=FALSE)}, creator=creator)
  xs <- double()
  ys <- double()
  means <- double()
  for (i in 1:length(dates)){
    d = dates[i]
    xvals = rep(i, length(unlist(u[d], use.names = FALSE)))
    yvals = unlist(u[d], use.names = FALSE)
    xs <- c(xs, xvals)
    ys <- c(ys, yvals)
    means <- c(means, mean(yvals))
  }
  
  mean_data = data.frame(weeks=seq(1:length(means)), means, groups=creator)
  return(mean_data)
}

creator_overlay_plot <- function(){
  da = lapply(players, creator_mean_data)
  all_data = rbind(da[[1]], da[[2]], da[[3]], da[[4]], da[[5]], da[[6]], da[[7]], da[[8]], da[[9]], da[[10]])
  d = highlight_key(all_data, ~groups)
  # partially transparent points by setting `alpha = 0.5`
  p <- ggplot(d, aes(x=weeks, y=means, group=groups))+
    geom_point(shape = 21, alpha = 0.5, size = 3, colour="#01587A", fill="#077DAA") +
    geom_line(data=d,aes(x=weeks, y=means), colour="#077DAA") +
    ylim(-0.1, 10.1) +
    theme_bw() +
    xlab("Trivia Week") + 
    ylab("Round Score") + 
    ggtitle(label=paste("How well everyone did on everyone else's Rounds (hover to see detail)")) + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
  
  gg <- ggplotly(p, tooltip="groups")
  highlight( gg, on = "plotly_hover", off = "plotly_deselect", color = "red" )
}

bias_table_heatmap <- function(){
  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmaply(bt, dendrogram = "none", labCol = colnames(bt), labRow = rownames(bt), colors=coul, xlab="Creator", ylab="Player", main="Bias Table")
}

joker_table_heatmap <- function(){
  jt = joker_table()
  jt[4, 4] = NA # Jenny is a problem hack
  
  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmaply(jt, dendrogram = "none", labCol = colnames(jt), labRow = rownames(jt), colors=coul, xlab="Creator", ylab="Player", main="Who do I Joker Table")
}



# don't run this too much or google gets upset
if(IS_GOOGLE_UPSET){
  filenames = lapply(dates, get_filename)
} else{
  filenames = lapply(google_ids, get_filename)
}

scores = lapply(filenames, read.csv)
bt = bias_table()
bt[4, 4] = NA # Jenny had a bias on her own rounds, this is a hack


