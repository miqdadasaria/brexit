################################################################################################
# Some toy code to predict Brexit referendum results based on FT poll of polls data from:
# https://ig.ft.com/sites/brexit-polling/
#
# Author: Miqdad Asaria
# Date: 19 June 2016
################################################################################################

library(readr)
library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

##############################################################
# 1. load the data and do some basic data cleaning work
##############################################################
load_data = function(imputation, cached){
  
  if(cached){
    poll_results = read_html("EU referendum poll of polls.html") %>% 
      html_node("table") %>%
      html_table(header = TRUE)
  } else {
    # read in the poll results from the FT webpage
    poll_results = read_html("https://ig.ft.com/sites/brexit-polling/") %>% 
      html_node("table") %>%
      html_table(header = TRUE)
   }
  
  # rename columns removing special characters
  names(poll_results) = str_replace(names(poll_results),"[\\s,%].*","")
  
  # clean up the sample size field
  poll_results$Sample = as.integer(str_replace(poll_results$Sample,"[\\,,-]",""))
  
  # fix spelling discrepancies in Pollster names
  poll_results$Pollster = str_replace(poll_results$Pollster,"Comres","ComRes")
  
  imputed=0
  # handle missing sample sizes appropriately
  if(imputation=="min") {
    imputed = min(poll_results$Sample,na.rm=TRUE)
  } else if(imputation=="mean") {
    imputed = round(mean(poll_results$Sample,na.rm=TRUE))
  } else if(imputation=="complete") {
    poll_results = poll_results %>% filter(!is.na(Sample))
  }
  poll_results$Sample = ifelse(is.na(poll_results$Sample),imputed,poll_results$Sample)
  
  # convert dates back into date format
  poll_results$Date = as.Date(poll_results$Date, format="%B %d, %Y")
  
  # filter data for start and end dates of interest
  start_date = "Sep 9, 2010"
  end_date = "Jun 20, 2016"
  poll_results = poll_results %>% 
    filter(Date >= as.Date(start_date, format="%B %d, %Y") & 
             Date <= as.Date(end_date, format="%B %d, %Y"))
  
  return(poll_results)
}
################################################################################################
# 2. replicate FT analysis
#
# select 7 polls from unique pollsters in date order - discard repeat polls from same pollster
# discard poll with maximum remain prediction - if ties remove oldest
# discard poll with maximum leave prediction - if ties remove oldest
# prediction is simple mean of 5 remaining polls
# repeat process to produce moving average shifting window by one poll at a time
################################################################################################

##############################################################
# calculate moving average of polls over time using FT method
##############################################################
calculate_moving_average = function(poll_results){
  results = poll_results[FALSE,1:4]
  
  for (res in 1:(nrow(poll_results)-7)) {
    group_of_seven = poll_results[FALSE,]
    for (i in res:nrow(poll_results)) {
      poll = poll_results[i,]
      if(nrow(group_of_seven)>0){
        if(!(poll$Pollster %in% group_of_seven$Pollster))
          group_of_seven = bind_rows(poll,group_of_seven)
      } else {
        group_of_seven = bind_rows(poll,group_of_seven)
      }
      if(nrow(group_of_seven)==7){
        group_of_seven=group_of_seven[-which.max(group_of_seven$Remain),]
        group_of_seven=group_of_seven[-which.max(group_of_seven$Leave),]
        results = bind_rows(results,
          group_of_seven %>% select(1:4) %>% summarise_each(funs(mean)))
        break
      }
    }
  }
  
  return(results %>% gather(position,vote,-Date))
}

#########################################################
# plot poll results and the FT moving average over time
#########################################################
plot_ft_ma_graph = function(poll_results, ft_graph_start_date){
  graph_data_ma = calculate_moving_average(poll_results)
  graph_data_polls = poll_results %>% 
    select(1:4) %>%
    gather(position,vote,-Date)
  
  ft_plot = ggplot() +
    ggtitle("FT poll of polls moving average") +
    geom_line(size=1.1, data=filter(graph_data_ma,Date>=ft_graph_start_date), mapping=aes(x=Date,y=vote,group=position,colour=position)) + 
    geom_point(alpha=0.3, data=filter(graph_data_polls,Date>=ft_graph_start_date), mapping=aes(x=Date,y=vote,group=position,colour=position)) + 
    geom_hline(yintercept=50, linetype=2, colour="darkgrey") +
    geom_hline(yintercept=25, linetype=2, colour="darkgrey") +
    scale_x_date(date_breaks="1 month", date_labels="%b %y") +
    ylab("Vote (%)") +
    theme_bw()

    # save plot to file
    ggsave("ft_plot.png",ft_plot,width=30,height=20,units="cm",dpi=300)  
  
  return(ft_plot)
}

#########################################################################################
# 3. run some basic diagnostics
#
# check if it is worth doing a multilevel model with between and within pollster effects
#########################################################################################

##########################################################################
# look for pollster effects on each outcome position by plotting outcomes 
# as facets and grouping polls by pollster 
##########################################################################
plot_pollster_effects_graph = function(poll_results){ 
  graph_data_polls = poll_results %>% 
    select(1:5) %>%
    gather(position,vote,-Date,-Pollster)

  pollster_plot = ggplot(data=graph_data_polls, mapping=aes(x=Date,y=vote,group=Pollster,colour=Pollster)) +
    ggtitle("Exploring pollster specific trends") +
    geom_point(alpha=0.5) + 
    geom_hline(yintercept=50, linetype=2, colour="darkgrey") +
    geom_hline(yintercept=25, linetype=2, colour="darkgrey") +
    facet_grid(position~.) +
    scale_x_date(date_breaks="3 months", date_labels="%b %y") +
    ylab("Vote (%)") +
    theme_bw()

  # save plot to file
  ggsave("pollster_plot.png",pollster_plot,width=40,height=20,units="cm",dpi=300)  

  return(pollster_plot)
}

#################################################################
# some simple regressions to check for pollster specific effects
# perhaps should have done a multinomial model here
#################################################################
run_pollster_regressions = function(poll_results){
  sink(file="pollster_regressions.txt")
    pollster_regression_1 = lm(Remain~Date+as.factor(Pollster),data=poll_results,weights=Sample)
    print(summary(pollster_regression_1))
    pollster_regression_2 = lm(Leave~Date+as.factor(Pollster),data=poll_results,weights=Sample)
    print(summary(pollster_regression_2))
    pollster_regression_3 = lm(Undecided~Date+as.factor(Pollster),data=poll_results,weights=Sample)
    print(summary(pollster_regression_3))
  sink()
}


##############################################
# load data and run some basic descriptives
##############################################
# load data
poll_results = load_data(imputation="mean", cached=TRUE)

# replicate FT analysis
ft_plot = plot_ft_ma_graph(poll_results, ft_graph_start_date=as.Date("Aug 31, 2015", format="%B %d, %Y"))

# run some diagnostics to check for pollster effects
pollster_plot = plot_pollster_effects_graph(poll_results)
run_pollster_regressions(poll_results)
## regression results suggest little case for pollster effeect so 
## model will treat each poll result as independent observation
