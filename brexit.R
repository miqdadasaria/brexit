library(rvest)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

# read in the poll results from the FT webpage
poll_results = read_html("https://ig.ft.com/sites/brexit-polling/") %>% 
  html_node("table") %>%
  html_table(header = TRUE)

# clean up the sample size
poll_results$Sample = as.integer(str_replace(poll_results$Sample,"[\\,,-]",""))
# set missing sample sizes to the minimum poll sample size given
min_sample = min(poll_results$Sample,na.rm=TRUE)
poll_results$Sample = ifelse(is.na(poll_results$Sample),min_sample,poll_results$Sample)

# convert dates back into date format
poll_results$Date = as.Date(poll_results$Date, format="%B %d, %Y")

# start by replicating FT analysis

# break down into chunks of 7 from unique pollsters
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
      group_of_seven=group_of_seven[-which.max(group_of_seven$`Remain %`),]
      group_of_seven=group_of_seven[-which.max(group_of_seven$`Leave %`),]
      results = bind_rows(results,
        group_of_seven %>% select(1:4) %>% summarise_each(funs(mean)))
      break
    }
  }
}

# choose start date to start graphing from
start_date = "Sep 14, 2014"
graph_data_ma = results %>% gather(position,vote,-Date) %>% filter(Date > as.Date(start_date, format="%B %d, %Y"))
graph_data_polls = poll_results %>% select(1:4) %>% gather(position,vote,-Date) %>% filter(Date > as.Date(start_date, format="%B %d, %Y"))

ft_plot = ggplot() +
  ggtitle("FT poll of polls moving average") +
  geom_line(size=1.1, data=graph_data_ma, mapping=aes(x=Date,y=vote,group=position,colour=position)) + 
  geom_point(alpha=0.3, data=graph_data_polls, mapping=aes(x=Date,y=vote,group=position,colour=position)) + 
  geom_hline(yintercept=50, linetype=2, colour="darkgrey") +
  geom_hline(yintercept=25, linetype=2, colour="darkgrey") +
  scale_x_date(date_breaks="3 months", date_labels="%b %Y") +
  ylab("Vote (%)") +
  theme_bw()

ggsave("ft_plot.png",ft_plot,width=30,height=20,units="cm",dpi=300)  