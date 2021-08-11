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
library(stringr) # regex (str_extract)
library(R.utils) # insert
library(gridExtra) # for grid.arrange
library(grid) # for textGrob


# should match csv file names
dates=c("March18", "March25", "April1", "April8", "April15", "April22", "April29", "May6", "May13", "May20", "May27", "June2", "June10", "June24", "July2", "July8", "July15", "July22", "July29", "August5", "August12", "August19", "September3", "September9", "September17", "September24", "October1", "October8", "October15", "October22", "October29", "November5", "November12", "November19", "December3", "December10", "December17", "December31", "January7-21", "January14-21", "January21-21", "January28-21", "February4-21", "February11-21", "February25-21", "March4-21", "March11-21", "March18-21", "March25-21", "April1-21", "April8-21", "April15-21", "April22-21", "April29-21", "May13-21")


# This is probably inefficient
replace1 = str_replace_all(dates, "March", "3.")
replace2 = str_replace_all(replace1, "April", "4.")
replace3 = str_replace_all(replace2, "May", "5.")
replace4 = str_replace_all(replace3, "June", "6.")
replace5 = str_replace_all(replace4, "July", "7.")
replace6 = str_replace_all(replace5, "August", "8.")
replace7 = str_replace_all(replace6, "September", "9.")
replace8 = str_replace_all(replace7, "October", "10.")
replace9 = str_replace_all(replace8, "November", "11.")
replace10 = str_replace_all(replace9, "December", "12.")
replace11 = str_replace_all(replace10, "January", "1.")
replace12 = str_replace_all(replace11, "February", "2.")
replace21 = str_replace_all(replace12, "-21", ".21")
round_dates = str_replace_all(replace21, "(?<!.21)$", ".20")


# to get id:
# right click on file in drive -> get shareable link -> copy id substring
# NEEDS TO BE UPDATED, ONLY VALID UP TILL LIKE JULY22
google_ids=c("1B8jDE1_m9ilwz2c3L__vRBVNlFRkYFY-", "1oYpePFo3TofyYfukndqi7cNR7d19NH9Y", "1qZAzHPoLqWtY-MwiiEzC2cwd2f57uCiP", "1SXQ46m6kqOu3N7uzghNcscKAe171TDz5", "1BSo_bW-Armwnn6qy3fPTimGLontMTTVa", "1mrl_UgL-PGat2d586NfNlXT6OfXkXDtZ", "11RXp1dqKN8DDiFfq0lKrACmbeb_yb-W2", "1KuFoEDJFprCrkGUzE7Bp_9V5Q_nDuA4T", "11RkmBiMfCrOeV1dY2C3l_KUTqAXT8s8R", "1-N2R3ZJp-ahf7WRJa9ODjVP7QaTNHShD", "1J0Kx3Kf-4kTb-tjvxbDMsz0gtU_CoFMM", "1gK9k4eVfbQ1U7SuQD9CWrysrIapHSK_Y", "1SlbPlTn8iIj1OA7pAkK9C9IepnX_rzob", "1MdkoCG544dbT0gwGNFNMwFTIHQjgW3Bg", "1mrwkdVQKv1wFMICeyE_eCD1NyAmpcRii", "14ldPagW4XGJTkKqv916fuQY3fB-lqqMn", "1v0GG0YjOwkSr1HML68P8cLTjtj8G8Owp", "1kEMwWBd0OWxnSdqjHarap-Oh0fT7rGR9")


players=c("Zach", "Megan", "Ichigo", "Jenny", "Mom", "Dad", "Chris", "Alex", "Drew")


IS_GOOGLE_UPSET_WITH_ME = TRUE

get_filename <- function(id){
  if(IS_GOOGLE_UPSET_WITH_ME){
    f = sprintf("/Users/alex/Downloads/%s.csv", id)
  } else {
    f = sprintf("https://docs.google.com/uc?id=%s&export=download", id)
  }
  return(f)
}

# read category_map.csv in google drive
category_map = read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", "1EwjfhedsNozQkUFz3IgagdC7R_EQMYTe"))

# don't run this too much or google gets upset
if(IS_GOOGLE_UPSET_WITH_ME){
  filenames = lapply(dates, get_filename)
} else{
  filenames = lapply(google_ids, get_filename)
}

scoreframes = lapply(filenames, read.csv)

create_rounds_data_frame <- function(normalize=FALSE){
  roundframes = data.frame()
  
  # Combine scoresheets and round categories list
  # Don't ask
  for (i in 1:length(dates)){ 
    my_sf = scoreframes[[which(dates==dates[i])]]
    my_cmap = category_map[which(category_map$Date==round_dates[i]),]
    
    
    o = order(my_sf$Creator.Round)
    mo = o[1:sum(!is.na(my_sf$Creator.Round))]
    
    creator_nums = my_sf$Creator.Round[which(!is.na(my_sf$Creator.Round))]
    creator_nums = creator_nums[order(creator_nums)]
    rd_nums = 1:length(my_cmap[,1])
    playerlist = my_sf$Player[mo]
    l = playerlist
    
    if(!all(rd_nums %in% creator_nums)){
      missing_index = which(!(rd_nums %in% creator_nums))
      l = insert(playerlist, missing_index, "Unknown")
    }
    
    d=data.frame("Creator" = l)
    for(player in players){
      if (player %in% my_sf$Player){
      d <- cbind(d, player=unlist(my_sf[my_sf$Player==player,5:(4+length(my_cmap[,1]))]))
      } else{
        d <- cbind(d, player=rep(NA, length(my_cmap[,1])))
      }
    }
    colnames(d) = c("Creator", players)
    
    r = cbind(my_cmap, d)
    if(normalize){
      r[10:18] = (r[10:18] / r$Possible.Points) * 10
    }
    
    roundframes <- rbind(roundframes, r)
    
     
    #}
  }
  return(roundframes)
}

roundframes = create_rounds_data_frame(normalize=TRUE)



categories = unique(tolower(roundframes$Major.Category))
categories = categories[-4] # remove current events


category_bias_table <- function(player) {
  scores = rep(NA, length(categories))
  i=1
  for (category in categories){
    q = roundframes[which(tolower(roundframes$Major.Category)==category),10:18]
    means = rep(NA, length(q[,1]))
    for(row in 1:length(q[,1])){
      means[row] = mean(unlist(q[row,]), na.rm=TRUE)
      # Actually we only want the mean of the rounds each person participated in - for example, I do bad at word games but 
      # that might be because I only compete in ones other people make, and those might be harder than the average since I make most of them
      
      catmean = mean(means)
    }
    
    
    #word = sprintf("%s: %f\n", category, mean(roundframes[which(tolower(roundframes$Major.Category)==category)]["Mom"] - catmean, na.rm=TRUE))
    scores[i] = mean(unlist(roundframes[which(tolower(roundframes$Major.Category)==category),][player] - catmean), na.rm=TRUE)
    i = i+1
  }
  
  s = scores[order(scores)]
  # ALso, maybe we want to subtract the person's average from the score instead of the mean of the scores
  s = s- mean(s)
  cats = categories[order(scores)]
  df = data.frame("scores" = s, "categories" = cats)
  cbarMax = max(max(s), abs(min(s)))
  
  p<-ggplot(data=df, aes(x=reorder(cats, -s), y=s, text=s, fill=s)) +
    geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank()) + ggtitle(player) + ggplot2::scale_fill_gradient2(low = "firebrick", 
                                                                                         high = "forestgreen", 
                                                                                         midpoint = 0, 
                                                                                         limits = c(-1*cbarMax, cbarMax))+ guides(fill=FALSE) + 
    ylim(-2, 2)
}

p1 = category_bias_table("Mom")
p2 = category_bias_table("Dad")
p3 = category_bias_table("Alex")
p4 = category_bias_table("Megan")
p5 = category_bias_table("Jenny")
p6 = category_bias_table("Ichigo")
p7 = category_bias_table("Zach")
p8 = category_bias_table("Drew")
p9 = category_bias_table("Chris")

#ggplotly(p, tooltip="text")
# see about changing baroutline color on hover or something

#grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=9, top = textGrob("Relative Category Strengths & Weaknesses",gp=gpar(fontsize=20,font=3)))

round_difficulty_chart <- function(){
  mymeans = rep(NA, length(categories))
  i=1
  for (category in categories){
    q = roundframes[which(tolower(roundframes$Major.Category)==category),10:18]
    means = rep(NA, length(q[,1]))
    for(row in 1:length(q[,1])){
      means[row] = mean(unlist(q[row,]), na.rm=TRUE)
    }    
    mymeans[i] = mean(means)
    i=i+1
  }
  mymeans = mymeans - mean(mymeans)
  
  df = data.frame("scores" = mymeans, "categories" = categories)
  cbarMax = max(max(mymeans), abs(min(mymeans)))
  
  p<-ggplot(data=df, aes(x=reorder(categories, -mymeans), y=mymeans, text=mymeans, fill=mymeans)) +
    geom_bar(stat="identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank()) + ggtitle("Round Difficulties") + ggplot2::scale_fill_gradient2(low = "firebrick", 
                                                                                          high = "forestgreen", 
                                                                                          midpoint = 0, 
                                                                                          limits = c(-1*cbarMax, cbarMax))+ guides(fill=FALSE) 
  p
}


alex_round_means <- function() {
  categories = unique(tolower(roundframes$Major.Category))
  scores = rep(NA, length(categories))
  meds = rep(NA, length(categories))
  i=1
  for (category in categories){
    q = roundframes[which(tolower(roundframes$Major.Category)==category),10:18]
    means = rep(NA, length(q[,1]))
    for(row in 1:length(q[,1])){
      means[row] = mean(unlist(q[row,]), na.rm=TRUE)
      # Actually we only want the mean of the rounds each person participated in - for example, I do bad at word games but 
      # that might be because I only compete in ones other people make, and those might be harder than the average since I make most of them
      
      catmean = mean(means)
    }
    
    
    #word = sprintf("%s: %f\n", category, mean(roundframes[which(tolower(roundframes$Major.Category)==category)]["Mom"] - catmean, na.rm=TRUE))
    scores[i] = mean(unlist(roundframes[which(tolower(roundframes$Major.Category)==category),]["Alex"]), na.rm=TRUE)
    meds[i] = median(mean(unlist(roundframes[which(tolower(roundframes$Major.Category)==category),]["Alex"]), na.rm=TRUE))
    i = i+1
  }
  
  s = scores[order(scores)]
  t = meds[order(scores)]
  # ALso, maybe we want to subtract the person's average from the score instead of the mean of the scores
  s = s
  cats = categories[order(scores)]
  df = data.frame("scores" = s, "categories" = cats, "medians" = t)
  return(df)
}

alex_creator_means <- function(){
  players = unique(tolower(roundframes$Creator))
  #players = players[-9]
  players = players[-5]
  alex_means = rep(NA, length(players))
  i=1
  for(player in players){
    c = rdf[which(tolower(rdf$Creator)==player),]
    creator_rds = c[which(!is.na(c$Alex)),]
    alex_means[i] = mean(creator_rds$Alex)
    i=i+1
  }
  return(data.frame("scores"=alex_means, "creators"=players))
}




