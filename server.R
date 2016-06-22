# Shiny server for brexit prediction model example
# 
# Author: Miqdad Asaria
# Date: 19/06/2016
###############################################################################

library(shiny)
source("brexit.R")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
	
  # load data
  loadPollData = reactive({
    load_data(imputation=input$imputation, cached=TRUE)
  })
  
  # run prediction model
  dataInput = reactive({
    generate_predictions(loadPollData(),
                         as.double(input$prop_leave_vote), 
                         as.double(input$prop_remain_vote), 
                         as.double(input$prop_und_vote), 
                         as.double(input$prop_und_remain), 
                         input$time_wts, 
                         input$size_wts, 
                         as.double(input$rel_time_size_wt), 
                         as.double(input$ci))
  })

  output$result = renderText({
    print(summarise_prediction(dataInput(), as.double(input$ci)))
  })
  
  # replicate FT analysis
  output$FT_plot = renderPlot({
    print(plot_ft_ma_graph(loadPollData(), ft_graph_start_date=as.Date("Aug 31, 2015", format="%B %d, %Y"))
    )
  })
  
  output$pollster_plot = renderPlot({
    # run some diagnostics to check for pollster effects
    print(plot_pollster_effects_graph(loadPollData()))
  })
			
	output$polls_plot = renderPlot({
		print(plot_poll_data(dataInput()))
	})

	output$weights_plot = renderPlot({
	  print(plot_model_weights(dataInput()))
	})
	
	output$poll_data = renderDataTable({
	  print(loadPollData())
	})
})
