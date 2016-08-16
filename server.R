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
    load_data(imputation=input$imputation, 
      cached=TRUE)
  })
  
  # run prediction model as cache data for use in plots and tables 
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

  # print prediction
  output$result = renderText({
    print(summarise_prediction(dataInput(), 
      as.double(input$ci)))
  })
  
  # replicate moving average FT analysis
  output$FT_plot = renderPlot({
    print(plot_ft_ma_graph(loadPollData(), 
      ft_graph_start_date=as.Date("Aug 31, 2015", format="%B %d, %Y"))
    )
  })
  
  # run some diagnostics to check for pollster effects
  output$pollster_plot = renderPlot({
    print(plot_pollster_effects_graph(loadPollData()))
  })
			
  # plot individual poll results with confidence intervals
  output$polls_plot = renderPlot({
    print(plot_poll_data(dataInput()))
  })

  # plot weights given to individual poll results
  output$weights_plot = renderPlot({
    print(plot_model_weights(dataInput()))
  })

  # tabulate individual poll results
  output$poll_data = renderDataTable({
    print(loadPollData())
  })
})
