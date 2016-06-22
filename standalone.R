# Standalone brexit prediction model 
# 
# Author: Miqdad Asaria
# Date: 19/06/2016
###############################################################################

source("brexit.R")

# key model parameters - these can be manipulated in shiny
confidence_interval = 0.95
time_weights_method = "exponential"
size_weights_method = "FE"
relative_weight_time_to_size = 1
proportion_leave_vote = 0.9
proportion_remain_vote = 0.7
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
predicted_poll_data = generate_predictions(poll_results, proportion_leave_vote, proportion_remain_vote, proportion_undecided_vote, proportion_undecided_remain, time_weights_method, size_weights_method, relative_weight_time_to_size, confidence_interval)

# output key model results
result_summary = summarise_prediction(predicted_poll_data, confidence_interval)

polls_plot = plot_poll_data(predicted_poll_data)
weights_plot = plot_model_weights(predicted_poll_data)