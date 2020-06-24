#' Adds changepoints annotations to a plotly plot.
#' @param plotly_obj a plotly object
#' @param original_data A dataset with "X" and "Y" columns (column names required)
#' @param changepoint_data The output of gfpop::gfpop() with original_data
#' @returns a plotly_object with additional traces corresponding to changepoints
#' @import plotly
#' @export
add_changepoints <- function(plotly_obj, original_data, changepoint_data) {
  # Initialize plotly object to return
  return_plotly <- plotly_obj %>%
    hide_legend()
  
  changepoint_annotations_regions = data.frame(x = c(), y = c(), text = c())
  changepoint_annotations = data.frame(x = c(), y = c(), text = c())

  changepoints <- changepoint_data$changepoints

  # Note: ds = dataspace, since changepoint data refers to indicates, not in dataspace
  previous_changepoint <- 1
  previous_changepoint_ds <- original_data$X[1]
  i <- 1
  # Add each changepoint to the given plotly object
  for (i in 1:length(changepoints)) {
    changepoint <- changepoints[i]
    changepoint_ds <- original_data$X[changepoint]

    # The region preceeding a changepoint, or between two changepoints
    changeregion <- seq(previous_changepoint, changepoint)
    changeregion_ds <- seq(previous_changepoint_ds,
      changepoint_ds,
      length.out = length(changeregion)
    )

    changepoint_annotations_regions <- rbind(
      changepoint_annotations_regions,
      data.frame(x = c(changeregion_ds, NA),
                 y = c(rep(changepoint_data$parameters[i], length(changeregion_ds)), NA),
                 text = c(rep(
                   paste0(
                   "State: ", changepoint_data$states[i], "\n",
                   "Region mean: ", round(changepoint_data$parameters[i], 2), "\n",
                   "Next changepoint: ", round(changepoint_ds, 2)
                 ),
                 length(changeregion_ds)), NA)
      )
    )
    # If this isn't the first region, connect this region with the last
    if (i > 1) {
      changepoint_annotations <- rbind(
        changepoint_annotations,
        data.frame(
          x = c(rep(previous_changepoint_ds, 50), NA), 
          y = c(seq(changepoint_data$parameters[i - 1],
                                  changepoint_data$parameters[i],
                                  length.out = 50
          ), NA),
          text = c(rep(paste0("Changepoint #", i-1, ": ", round(previous_changepoint_ds, 2)), 50),
                   NA)
        )
      )
    }

    # Update the previous changepoints
    previous_changepoint <- changepoint
    previous_changepoint_ds <- changepoint_ds
  }

  return_plotly <- return_plotly %>%
    add_lines(data = changepoint_annotations_regions,
              x = ~x,
              y = ~y, 
              color = ~I("#40B0A6"),
              hoverinfo = "text", text = ~text,
              connectgaps = F,
              line = list(width = 7)) 
  
  if(nrow(changepoint_annotations) > 0) {
    return_plotly <- return_plotly %>%
      add_lines(data = changepoint_annotations,
                x = ~x,
                y = ~y, 
                color = ~I("#E1BE6A"),
                hoverinfo = "text", text = ~text,
                connectgaps = F,
                line = list(width = 7)) 
  }
  
  return_plotly %>% layout(hovermode = "x unified")
    
}
