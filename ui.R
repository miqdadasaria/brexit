# Shiny ui for brexit prediction model
# 
# Author: Miqdad Asaria
# Date: 19/06/2016
###############################################################################

library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("Brexit Prediction Model"),
	
  sidebarPanel(
			
    selectInput("imputation", 
      "Missing sample size imputation:",
      list("Mean" = "mean", 
        "Minumum" = "min", 
        "Exclude" = "complete"), selected="min"),
	  
    sliderInput("prop_leave_vote", 
      "Proportion of leave voters polled who will actually vote", 
      min=0, max=1, value=0.6),
	   
    sliderInput("prop_remain_vote", 
      "Proportion of remain voters polled who will actually vote", 
      min=0, max=1, value=0.65),
	   
    sliderInput("prop_und_vote", 
      "Proportion of undecided voters polled who will actually vote", 
      min=0, max=1, value=0.3),
			
    sliderInput("prop_und_remain", 
      "Proportion of undecided voters who will actually vote who will vote to remain", 
      min=0, max=1, value=0.8),

    selectInput("time_wts", 
      "Time weighting method:",
      list("Exponential" = "exponential", 
        "Linear" = "linear", 
        "None" = "none"), 
      selected="linear"),
			
    selectInput("size_wts", 
      "Sample size weighting:",
      list("Fixed Effects" = "FE", 
        "Random Effects" = "RE"), 
      selected="FE"),
			
    sliderInput("rel_time_size_wt", 
      "Relative weight of timeliness as compared to sample size", 
      min=0, max=10, value=1),
			
    sliderInput("ci", 
      "Confidence Interval", 
      min=0.5, max=0.99, value=0.95)
  ),
	
  mainPanel(   
    h4(htmlOutput("result")),
    tabsetPanel(
      tabPanel("MA Plot", plotOutput("FT_plot")),
      tabPanel("Pollsters", plotOutput("pollster_plot")),
      tabPanel("Prediction", plotOutput("polls_plot")),
      tabPanel("Weights", plotOutput("weights_plot")),
      tabPanel("Data", dataTableOutput("poll_data"))
		)
	)
  
 )
)