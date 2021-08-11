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
library(useful)


# should match csv file names
dates=c("March18", "March25", "April1", "April8", "April15", "April22", "April29", "May6", "May13", "May20", "May27", "June2", "June10", "June24", "July2", "July8", "July15", "July22", "July29", "August5", "August12", "August19", "September3", "September9", "September17", "September24", "October1", "October8", "October15", "October22", "October29", "November5", "November12", "November19", "December3", "December10", "December17", "December31", "January7-21", "January14-21", "January21-21", "January28-21", "February4-21", "February11-21", "February25-21", "March4-21", "March11-21", "March18-21", "March25-21", "April1-21", "April8-21", "April15-21", "April22-21", "April29-21", "May13-21", "May28-21", "June3-21", "June10-21", "June16-21", "June24-21", "July1-21", "July8-21", "July22-21", "July29-21", "August5-21")

# to get id:
# right click on file in drive -> get shareable link -> copy id substring
# NEEDS TO BE UPDATED, ONLY VALID UP TILL LIKE JULY22
google_ids=c("1B8jDE1_m9ilwz2c3L__vRBVNlFRkYFY-", "1oYpePFo3TofyYfukndqi7cNR7d19NH9Y", "1qZAzHPoLqWtY-MwiiEzC2cwd2f57uCiP", "1SXQ46m6kqOu3N7uzghNcscKAe171TDz5", "1BSo_bW-Armwnn6qy3fPTimGLontMTTVa", "1mrl_UgL-PGat2d586NfNlXT6OfXkXDtZ", "11RXp1dqKN8DDiFfq0lKrACmbeb_yb-W2", "1KuFoEDJFprCrkGUzE7Bp_9V5Q_nDuA4T", "11RkmBiMfCrOeV1dY2C3l_KUTqAXT8s8R", "1-N2R3ZJp-ahf7WRJa9ODjVP7QaTNHShD", "1J0Kx3Kf-4kTb-tjvxbDMsz0gtU_CoFMM", "1gK9k4eVfbQ1U7SuQD9CWrysrIapHSK_Y", "1SlbPlTn8iIj1OA7pAkK9C9IepnX_rzob", "1MdkoCG544dbT0gwGNFNMwFTIHQjgW3Bg", "1mrwkdVQKv1wFMICeyE_eCD1NyAmpcRii", "14ldPagW4XGJTkKqv916fuQY3fB-lqqMn", "1v0GG0YjOwkSr1HML68P8cLTjtj8G8Owp", "1kEMwWBd0OWxnSdqjHarap-Oh0fT7rGR9")

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
  
  heatmaply(ist, limits=c(-1.3, 1.3),dendrogram = "none", colors=coul, ylab="Player", main="Points gained by copying Ichigo's Joker", showticklabels = c(FALSE, TRUE))
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

get_winner_wo_creator <- function(date){
  score_frame = as.data.frame(scores[which(dates==date)])
  player_w=score_frame$Player[which(score_frame$Score - score_frame$Creator.Bonus == max(score_frame$Score - score_frame$Joker.Bonus, na.rm=TRUE))]
  return(player_w)
}

joker_win_percent <- function(){
  wj = sapply(dates, get_winner)
  woj = sapply(dates, get_winner_wo_joker)
  return((1 - sum(compare.list(wj, woj))/length(dates)) * 100)
}

no_creator_bonus_win_percent <- function(){
  wj = sapply(dates, get_winner)
  woj = sapply(dates, get_winner_wo_creator)
  return((1 - sum(compare.list(wj, woj))/length(dates)) * 100)
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

mean_round_score_by_date <- function(date){
  s = scores[which(dates==date)][[1]]
  
  numP = 0
  for (i in 1:length(players)){
    if (!is.na(s[i,]$Score) && s[i,]$Score>0){
      numP = numP+1
    }
  }
  
  sumScore = sum(c(na.omit(s$X1), 
                      na.omit(s$X2),
                      na.omit(s$X3),
                      na.omit(s$X4),
                      na.omit(s$X5),
                      na.omit(s$X6),
                      na.omit(s$X7),
                      na.omit(s$X8)))
  
  numRounds = sum(any(!is.na(s$X1)),
                  any(!is.na(s$X2)),
                  any(!is.na(s$X3)),
                  any(!is.na(s$X4)),
                  any(!is.na(s$X5)),
                  any(!is.na(s$X6)),
                  any(!is.na(s$X7)),
                  any(!is.na(s$X8)))
  
  meanRoundScore=sumScore/(numP-1)/numRounds
  
  return (c(meanRoundScore, numRounds))
}

overall_mean_round_score <- function(){
  s = sapply(dates, mean_round_score_by_date)
  sum=0
  for (week in 1:length(s[1,])){
    sum = sum + s[1,week] * s[2,week]
  }
  avg = sum/sum(s[2,])[[1]]
  return(avg)
}

get_normalized_round_score_by_creator_and_date <- function(creator, date){
  s <- get_creator_round_scores(creator, date)
  l <- overall_mean_round_score()
  if(any(!is.na(s))){
    m = mean(na.omit(s)[[1]])
    
    r <- sapply(players,function(p){rawScore=get_round_score(p, creator, date)
                              return(rawScore+(l[[1]]-m))})
    return(r)
  }
}

get_raw_round_score_by_creator_and_date <- function(creator, date){
  s <- get_creator_round_scores(creator, date)
  l <- overall_mean_round_score()
  if(any(!is.na(s))){
    m = mean(na.omit(s)[[1]])
    
    r <- sapply(players,function(p){rawScore=get_round_score(p, creator, date)
    return(rawScore)})
    return(r)
  }
}

get_raw_player_covariances <- function(){
  
  m = matrix(NA, ncol=10)
  colnames(m) <- players
  for (c in players){
    s = lapply(dates, get_raw_round_score_by_creator_and_date, creator=c)
    x = matrix(unlist(s), ncol = 10, byrow=TRUE)
    m = rbind(m, x)
  }
  
  cov(m, use="pairwise.complete.obs")
}

get_player_covariances <- function(){
  
  m = matrix(NA, ncol=10)
  colnames(m) <- players
  for (c in players){
    s = lapply(dates, get_normalized_round_score_by_creator_and_date, creator=c)
    x = matrix(unlist(s), ncol = 10, byrow=TRUE)
    m = rbind(m, x)
  }
  
  cov(m, use="pairwise.complete.obs")
}

get_all_scores <- function(){
  m = matrix(NA, ncol=10)
  colnames(m) <- players
  for (c in players){
    s = lapply(dates, get_raw_round_score_by_creator_and_date, creator=c)
    x = matrix(unlist(s), ncol = 10, byrow=TRUE)
    m = rbind(m, x)
  }
  return(m)
}

get_player_similarity_scores <- function(){
  cs = get_player_covariances()
  crs = cov2cor(cs)
  diag(crs) = NA
  drs = crs - mean(crs, na.rm=TRUE)
  return(drs)
}

get_player_covariance_scores <- function(){
  cs = get_raw_player_covariances()
  return(cs)
}

get_average_joker_round <- function(player){
  js = sapply(dates, function(date){s = scores[which(dates==date)]
  d = s[[1]]
  r = d[which(d$Player==player),]
  j = r$Joker.Bonus
  })
  mean(na.omit(js))
}

player_similarity_table <- function(){
  ss = get_player_similarity_scores()
  
  cbarMax = max(max(ss, na.rm=TRUE), abs(min(ss, na.rm=TRUE)))
  
  for(i in 1:length(ss[,1])){
    for (j in 1:length(ss[1,])){
      if (j>i){
        #srs[i,j]=0
      }
    }
  }
  
  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmaply(ss, dendrogram = "none", labCol = colnames(ss), labRow = rownames(ss), xlab="Player", ylab="Player", main="Player Similarity Table", 
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "firebrick", 
                                                                    high = "forestgreen", 
                                                                    midpoint = 0, 
                                                                    limits = c(-1*cbarMax, cbarMax)))
}

player_covariance_table <- function(){
  ss = get_player_covariance_scores()
  
  cbarMax = max(max(ss, na.rm=TRUE), abs(min(ss, na.rm=TRUE)))
  
  for(i in 1:length(ss[,1])){
    for (j in 1:length(ss[1,])){
      if (j>i){
        #srs[i,j]=0
      }
    }
  }
  
  coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
  heatmaply(ss, dendrogram = "none", labCol = colnames(ss), labRow = rownames(ss), xlab="Player", ylab="Player", main="Player Covariance Table", 
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "firebrick", 
                                                                    high = "forestgreen",
                                                                    midpoint = 0, 
                                                                    limits = c(-1*cbarMax, cbarMax)))
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
  cbarMax = max(max(bt, na.rm=TRUE), abs(min(bt, na.rm=TRUE)))
  
  heatmaply(bt, dendrogram = "none", labCol = colnames(bt), labRow = rownames(bt), xlab="Creator", ylab="Player", main="Bias Table", 
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "violetred4", 
                                                                    high = "palegreen4", 
                                                                    midpoint = 0, 
                                                                    limits = c(-1*cbarMax, cbarMax)))
}

plot_3d_similarity_rgl <- function(){
  library(pcaMethods)
  library(rgl)
  my_bpca = pca(m[2:length(m[,1]),c(1:8, 10)], "bpca", nPcs=8)
  dat <- read.table(textConnection(" Name             PC2         PC3         PC4
Zach    0.2241316 -0.22353987  0.16206292
Megan   0.5844217 -0.31900462 -0.02282969
Ichigo -0.4183210 -0.06697746 -0.89514362
Jenny   0.8316958 -0.46624533  0.06484492
Mom     0.7026414  0.69724700 -0.10678688
Dad    -0.4264668 -0.31471333  0.57878424
Chris  -0.5044915  0.40759882  0.04716477
Alex   -0.6761419 -0.30740131 -0.12161503
Drew   -0.2873242  1.09548299  0.34870422"),
                    header=TRUE,as.is=TRUE)
  
  # scale aspect to size of each principal component
  with(dat,plot3d(PC2, PC3, PC4, size=10, aspect=c(0.09867, 0.07604, 0.05431)))
  with(dat,text3d(PC2, PC3, PC4, Name))
}

plot_3d_similarity_plotly <- function(){
  dat <- read.table(textConnection(" Name             PC2         PC3         PC4
Zach    0.2241316 -0.22353987  0.16206292
Megan   0.5844217 -0.31900462 -0.02282969
Ichigo -0.4183210 -0.06697746 -0.89514362
Jenny   0.8316958 -0.46624533  0.06484492
Mom     0.7026414  0.69724700 -0.10678688
Dad    -0.4264668 -0.31471333  0.57878424
Chris  -0.5044915  0.40759882  0.04716477
Alex   -0.6761419 -0.30740131 -0.12161503
Drew   -0.2873242  1.09548299  0.34870422"),
                    header=TRUE,as.is=TRUE)
  
  fig1 <- plot_ly(dat, x = ~PC2, y = ~PC3, z = ~PC4, name=~Name, scene="scene1",  type="scatter3d", text=~Name, mode="markers+text", showlegend=FALSE)
  fig1 <- fig1 %>% layout(title = "3D Player Similarity (proximity implies similarity)",
                        # select the type of aspectmode
                        scene1 = list(aspectmode='manual',
                        aspectratio = list(x=0.9867, y=0.7604, z=0.5431)))
  add_trace(fig1, text=~Name, textposition="top center")
  fig1
}

krig <- function(){
  drs = get_raw_player_covariances()
  alexvec <- 1:length(drs[1,])
  alexnum <- 8
  alexvec <- alexvec[!alexvec %in% alexnum]
  sigma21 <- as.matrix(drs[alexnum,alexvec])
  sigma12 <- t(sigma21)
  sigma22 <- drs[alexvec,alexvec]
  sigma11 <- drs[alexnum, alexnum]
  h <- get_all_scores()
  alexmean <- mean(na.omit(h[,8]))
  othermean <- c(get_player_score("Zach"),
                 get_player_score("Megan"),
                 get_player_score("Ichigo"),
                 get_player_score("Jenny"),
                 get_player_score("Mom"),
                 get_player_score("Dad"),
                 get_player_score("Chris"),
                 get_player_score("Jeff"),
                 get_player_score("Drew"))
  
  roundscores <- c(7, 7, 4, 8, 3, 2, 5, 2, 2)
  
  mu <- alexmean + sigma12 %*% solve(sigma22) %*% (roundscores - othermean)
  sig <- sigma11 - sigma12 %*% solve(sigma22) %*% sigma21
  rnorm(1, mu, sig)
}

joker_table_heatmap <- function(){
  jt = joker_table()
  jt[4, 4] = NA # Jenny is a problem hack
  cbarMax = max(max(jt, na.rm=TRUE), abs(min(jt, na.rm=TRUE)))
  
  heatmaply(jt, dendrogram = "none", labCol = colnames(jt), labRow = rownames(jt), xlab="Creator", ylab="Player", main="Who do I Joker Table", 
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "indianred4", 
                                                                    high = "darkolivegreen", 
                                                                    midpoint = 0, 
                                                                    limits = c(-1*cbarMax, cbarMax)))
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


