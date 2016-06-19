# Shiny ui for brexit prediction model
# 
# Author: Miqdad Asaria
# Date: 19/06/2016
###############################################################################

library(shiny)

shinyUI(pageWithSidebar(
			
				# Application title
				headerPanel("Brexit Prediction Model"),
				
				sidebarPanel(
						sliderInput("ci", "Confidence Interval", min=0.5, max=0.99, value=0.95),
						
						selectInput("time_wts", "Time weighting method:",
								list("Exponential" = "exponential", 
										"Linear" = "linear", 
										"None" = "none"), selected="exponential"),
			
						selectInput("size_wts", "Sample size weighting:",
								list("Fixed Effects" = "FE", 
										"Random Effects" = "RE"), selected="FE"),
						
						sliderInput("rel_time_size_wt", "Relative weight of timeliness as compared to sample size", min=0, max=10, value=1),
						
						sliderInput("prop_und_vote", "Proportion of undecided voters who will vote", min=0, max=1, value=0.5),
						
						sliderInput("prop_und_remain", "Proportion of undecided voters who will vote to remain", min=0, max=1, value=0.5)
						
						),
				
				mainPanel(   
					h4(textOutput("caption_1")),
					h4(textOutput("caption_2")),
					plotOutput("FT_plot"),
					plotOutput("pollster_plot"),
					plotOutput("polls_plot"),
					plotOutput("weights_plot")
				)
))