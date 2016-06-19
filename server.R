# Shiny server for brexit prediction model example
# 
# Author: Miqdad Asaria
# Date: 19/06/2016
###############################################################################

library(shiny)
source("brexit.R")
# load data
poll_results = load_data(imputation="mean", cached=TRUE)

# replicate FT analysis
ft_plot = plot_ft_ma_graph(poll_results, ft_graph_start_date=as.Date("Aug 31, 2015", format="%B %d, %Y"))

# run some diagnostics to check for pollster effects
pollster_plot = plot_pollster_effects_graph(poll_results)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
	
  # run prediction model
  dataInput = reactive({
    generate_predictions(poll_results,
                         as.double(input$prop_und_vote), 
                         as.double(input$prop_und_remain), 
                         input$time_wts, 
                         input$size_wts, 
                         as.double(input$rel_time_size_wt), 
                         as.double(input$ci))
  })

  output$caption_1 = renderText({
    print(summarise_prediction(dataInput(), "remain", as.double(input$ci)))
	})
  
  output$caption_2 = renderText({
    print(summarise_prediction(dataInput(), "leave", as.double(input$ci)))
  })

  output$FT_plot = renderPlot({
    print(ft_plot)
  })
  
  output$pollster_plot = renderPlot({
    print(pollster_plot)
  })
			
	output$polls_plot = renderPlot({
		print(plot_poll_data(dataInput()))
	})

	output$weights_plot = renderPlot({
	  print(plot_model_weights(dataInput()))
	})
	
})
