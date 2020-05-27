# Given some data, generate a changepoint dataframe
generate_changepoint <- function(data_input, graph_input) {
  model <-
    gfpop(
      data = data_input,
      mygraph = graph_input,
      type = "mean"
    )
  num_changepoints <- length(model$changepoints)
  changepoint_data <-
    data.frame(
      changepoint = model$changepoints,
      changepoint_end = c(0, model$changepoints[1:num_changepoints -
        1]),
      y = model$parameters
    )
  return(changepoint_data)
}

# Gets the changepoint location associated with this location on the x axis
get_associated_changepoint <-
  function(x_loc, changepoint_data) {
    consistent_changepoint_data <-
      subset(
        changepoint_data,
        (changepoint >= x_loc) & (changepoint_end <= x_loc)
      )
    return(head(consistent_changepoint_data, 1))
  }

# Given data and it's associated changepoint data, annotates data points
# with the changepoint interval in which that data falls
annotate_data_with_changepoint <-
  function(data_input, changepoint_data) {
    matched_changepoints <-
      plyr::ldply(
        by(
          data_input, 1:nrow(data_input),
          function(row) {
            get_associated_changepoint(row$X, changepoint_data)
          }
        ),
        rbind
      )
    annotated_data <- cbind(data_input, matched_changepoints)
    return(
      data.frame(
        X = annotated_data$X,
        Y = annotated_data$Y,
        changepoint = annotated_data$changepoint,
        changepoint_end = annotated_data$changepoint_end,
        y = annotated_data$y,
        CP_Data = paste(
          "Assoc. CP: ",
          "\n",
          "\t",
          "Begin:",
          annotated_data$changepoint_end,
          "\n",
          "\t",
          "End:",
          annotated_data$changepoint,
          "\n",
          "\t",
          "CP_Y:",
          annotated_data$y
        )
      )
    )
  }
