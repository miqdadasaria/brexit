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

# predictions made for Brexit EU referendum on 23rd June 2016
election_date = as.Date("Jun 23, 2016", format="%B %d, %Y")

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

################################################
# 4. prediction model for brexit
################################################


#############################################################################
# calculate how many days from election each poll date was and use this to
# derive time_weights for the polls with more recent polls given more weight
#############################################################################
get_time_weights = function(days, method){
  weights = days
  if(method=="none"){
    weights = days/days
  } else if(method=="linear"){
    weights = 1/days
  } else if(method=="exponential"){
    weights = 1/exp(days)
  }
  # normalise weights
  weights = weights/max(weights)  
 
  return(weights)
}

#############################################################################
# calculate sample size based weights use either fixed or random effects
# based weights to give more importance to polls with larger samples
#############################################################################
get_size_weights = function(se, probs, size_weights_method){
  weights = se
  if(size_weights_method=="FE"){
    weights = 1/se^2
  } else if(size_weights_method=="RE"){
    numerator = max(sum(probs^2*1/se^2) - sum(probs*1/se^2)^2/sum(1/se^2) - (length(se)-1),0)
    tau_sq = numerator/(sum(1/se^2)-sum(1/se^4)/sum(1/se^2))
    weights = 1/(se^2+tau_sq)
  }
  # normalise weights
  weights = weights/max(weights)  
  
  return(weights)
}

#######################################################################################
# calculate predictions by combining assumptions on the proportion of undecided people 
# who vote with various weighting assumptions around sample size and timeliness 
#######################################################################################
generate_predictions = function(proportion_undecided_vote, proportion_undecided_remain, time_weights_method, size_weights_method, relative_weight_time_to_size, confidence_interval){
  z = qnorm((1+confidence_interval)/2)
  # assumes undecided voter proportion is constant over time
  model_poll_results = poll_results %>% 
    mutate(days_to_election=as.numeric(election_date-Date), 
           time_weight=get_time_weights(days_to_election, time_weights_method),
           undecided_voters = round(Sample*(Undecided/100)*proportion_undecided_vote),
           total = round(Sample*(Remain+Leave)/100)+undecided_voters,
           total_remain = total*((Remain+(Undecided*proportion_undecided_vote*proportion_undecided_remain))/
                              (Remain+Leave+(Undecided*proportion_undecided_vote))), 
           total_leave = total-total_remain,
           se = sqrt(total_remain*total_leave/total^3),
           remain = total_remain/total,
           leave = total_leave/total,
           ci = se*z,
           size_weight = get_size_weights(se, remain, size_weights_method),
           overall_weight = size_weight+relative_weight_time_to_size*time_weight) 
  # normalise weights so that they add up to 1
  model_poll_results$overall_weight = model_poll_results$overall_weight/sum(model_poll_results$overall_weight)
  
  return(model_poll_results)
}

#########################################################################
# calculate a weighted average of the polls with confidence intervals
#########################################################################
summarise_prediction = function(results, outcome, confidence_interval){
  z = qnorm((1+confidence_interval)/2)
  
  wa = weighted.mean(results[,outcome], results$overall_weight)
  wa_se = sqrt(sum(results$overall_weight*(results[,outcome]-wa)^2))
  wa_uci = wa + z*wa_se
  wa_lci = wa - z*wa_se

  outcome_summary = paste("Predicted probabilty of ", outcome,": ",
      round(wa*100,2),"% with ",
      confidence_interval*100,"% CI of (",
      round(wa_lci*100,2)," to ",
      round(wa_uci*100,2),")",sep="")

  return(outcome_summary)
}

###############################################################
# plot poll results modified for use in model with CIs
###############################################################
plot_poll_data = function(predicted_poll_data){
  predictions_data = predicted_poll_data %>% 
    select(Date,remain,leave,ci) %>% 
    gather(position,vote,-Date,-ci) %>%
    mutate(vote=vote*100, ci=ci*100)
  polls_plot = ggplot(data=predictions_data, mapping=aes(x=Date,y=vote,group=position,colour=position)) +
    ggtitle("Data used for predictions") +
    geom_point(alpha=0.3) +
    geom_errorbar(aes(ymin=vote-ci, ymax=vote+ci), width=0.1, alpha=0.3) +
    geom_smooth() +
    geom_hline(yintercept=50, linetype=2, colour="darkgrey") +
    scale_x_date(date_breaks="9 months", date_labels="%b %y") +
    ylab("Vote (%)") +
    theme_bw() +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
  
  # save plot to file
  ggsave("polls_plot.png",polls_plot,width=30,height=20,units="cm",dpi=300)  
  
  return(polls_plot)
}

###################################################
# plot weights used in the prediction model
###################################################
plot_model_weights = function(predicted_poll_data){
  weight_data = predicted_poll_data %>% select(Date,weight=overall_weight) 
  weights_plot = ggplot(data=weight_data, mapping=aes(x=Date,y=weight)) +
    ggtitle("Weights used for predictions") +
    geom_point(alpha=0.3) +
    scale_x_date(date_breaks="9 months", date_labels="%b %y") +
    ylab("Relative weights") +
    theme_bw()
  
  # save plot to file
  ggsave("weights_plot.png",weights_plot,width=30,height=20,units="cm",dpi=300)  

  return(weights_plot)
}

# key model parameters - these can be manipulated in shiny
confidence_interval = 0.95
time_weights_method = "exponential"
size_weights_method = "FE"
relative_weight_time_to_size = 1
proportion_undecided_vote = 0.5
proportion_undecided_remain = 0.7

# load data
poll_results = load_data(imputation="mean", cached=TRUE)

# replicate FT analysis
ft_plot = plot_ft_ma_graph(poll_results, ft_graph_start_date=as.Date("Aug 31, 2015", format="%B %d, %Y"))

# run some diagnostics to check for pollster effects
pollster_plot = plot_pollster_effects_graph(poll_results)
run_pollster_regressions(poll_results)
## regression results suggest little case for pollster effect so 
## model will treat each poll result as independent observation

# run prediction model
predicted_poll_data = generate_predictions(proportion_undecided_vote, proportion_undecided_remain, time_weights_method, size_weights_method, relative_weight_time_to_size, confidence_interval)

# output key model results
remain_summary = summarise_prediction(predicted_poll_data, "remain", confidence_interval)
leave_summary = summarise_prediction(predicted_poll_data, "leave", confidence_interval)
polls_plot = plot_poll_data(predicted_poll_data)
weights_plot = plot_model_weights(predicted_poll_data)