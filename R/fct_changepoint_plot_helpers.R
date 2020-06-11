#' Adds changepoints annotations to a plotly plot.
#' @param plotly_obj a plotly object
#' @param original_data A dataset with "X" and "Y" columns (column names required)
#' @param changepoint_data The output of gfpop::gfpop() with original_data
#' @returns a plotly_object with additional traces corresponding to changepoints
#' @importFrom plotly hide_legend add_lines plot_ly
#' @export
add_changepoints <- function(plotly_obj, original_data, changepoint_data) {
  # Initialize plotly object to return
  return_plotly <- plotly_obj %>%
    hide_legend()

  changepoints <- changepoint_data$changepoints

  # Note: ds = dataspace, since changepoint data refers to indicices, not in dataspace
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

    return_plotly <- return_plotly %>%
      add_lines(
        x = changeregion_ds, y = rep(changepoint_data$parameters[i]),
        color = I("#40B0A6"), hoverinfo = "text",
        text = paste0(
          "State: ", changepoint_data$states[i], "\n",
          "Region mean: ", round(changepoint_data$parameters[i], 2), "\n",
          "Next changepoint: ", round(changepoint_ds, 2)
        ),
        line = list(width = 7)
      )

    # If this isn't the first region, connect this region with the last
    if (i > 1) {
      return_plotly <- return_plotly %>%
        add_lines(
          x = rep(previous_changepoint_ds, 50), y = seq(changepoint_data$parameters[i - 1],
            changepoint_data$parameters[i],
            length.out = 50
          ),
          color = I("#E1BE6A"), hoverinfo = "text",
          text = paste0("Changepoint #", i-1, ": ", round(previous_changepoint_ds, 2)),
          line = list(width = 7)
        )
    }

    # Update the previous changepoints
    previous_changepoint <- changepoint
    previous_changepoint_ds <- changepoint_ds
  }

  return_plotly
}

#' Makes a ggplot object with the changepoint data
#' @param data_input The data associated with the given changepoints
#' @param changepoint_data The changepoints, in a dataframe format
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot geom_point aes geom_segment xlab ylab
#' @importFrom rlang .data
#' @examples
#' data <- data.frame(X = 1:100, Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1),
#'   c(1, 2, 1, 3, 1),
#'   sigma = 1
#' ))
#' graph <- gfpop::graph(type = "std", penalty = 15)
#' changepoint_data <- generate_changepoint(data$Y, graph)
#' plot_changepoint(data, changepoint_data)
#' @export
plot_changepoint <- function(data_input, changepoint_data) {
  annotated_data <- annotate_data_with_changepoint(data_input, changepoint_data)
  ggplot(annotated_data, aes(
    x = .data$X,
    y = .data$Y,
    text = .data$CP_Data
  )) +
    geom_point() +
    geom_segment(
      aes(
        x = .data$changepoint,
        xend = .data$changepoint_end,
        y = .data$y,
        yend = .data$y
      ),
      size = 1.5,
      col = "red"
    ) +
    xlab("X units (arbitrary)") +
    ylab("Univariate gaussian data (randomly generated)")
}
