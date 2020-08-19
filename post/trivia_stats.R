library(plyr)
library(rlang)
library(ggplot2)

# should match excel sheet names
dates=c("March18", "March25", "April1", "April8", "April15", "April22", "April29", "May6", "May13", "May20", "May27", "June3", "June10", "June24", "July2", "July9", "July15", "July22", "July29")

# to get id:
# right click on file in drive -> get shareable link -> copy id substring
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


get_mean_round_score <- function(player, creator){
  #return(mean(mapply(get_round_score, player, creator, dates), na.rm=TRUE))
  return(mean(sapply(dates, get_round_score, player=player, creator=creator), na.rm=TRUE))
}

get_mean_by_player <- function(player){
  scores = (sapply(players, get_mean_round_score, player=player))
  return (scores)
}

get_mean_by_creator <- function(creator){
  scores = (sapply(players, get_mean_round_score, creator=creator))
  return (scores)
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
  ggplot(data.frame(xs, ys), aes(xs, ys)) +
    geom_point(position=position_jitter(w=0.05, h=0.05),
               shape = 21, alpha = 0.5, size = 3, colour="#01587A", fill="#077DAA") +
    theme_bw() +
    geom_point(data=mean_data, mapping=aes(x=seq(1:length(means)), y=means), colour="#ed7474", shape = 21, alpha = 1, size = 3, fill="#b05656") +
    geom_line(data=mean_data,aes(x=seq(1:length(means)), y=means), colour="#ed7474") + 
    xlab("Trivia Week") + 
    ylab("Round Score") + 
    ggtitle(label=paste("How well everyone did on", creator, "'s Rounds"), subtitle = "(red points represent the average)") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
    ylim(-0.1, 10.1) 
    
}

# don't run this too much or google gets upset
if(IS_GOOGLE_UPSET){
  filenames = lapply(dates, get_filename)
} else{
  filenames = lapply(google_ids, get_filename)
}

scores = lapply(filenames, read.csv)

