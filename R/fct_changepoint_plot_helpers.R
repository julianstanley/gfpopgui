#' Adds changepoints annotations to a plotly plot.
#' @param plotly_obj a plotly object
#' @param original_data A dataset with "X" and "Y" columns (column names required)
#' @param changepoint_data The output of gfpop::gfpop() with original_data
#' @returns A list of 3 objects: "plot" (the plotly plot with changepoints added),
#' "changepoint_annotations", and "changepoint_annotations_regions"
#' @import plotly
#' @export
add_changepoints <- function(plotly_obj, original_data, changepoint_data) {
  # Initialize plotly object to return
  return_plotly <- plotly_obj %>%
    hide_legend()

  # Changepoint_annotations describe actual (vertical) changepoints
  # changepoints_annotations_regions describe the segments (horizontal)
  # between each changepoint.
  changepoint_annotations_regions <- data.table(
    x = c(), y = c(), text = c(),
    state = c()
  )
  changepoint_annotations <- data.table(
    x = c(), y = c(), text = c(),
    state = c()
  )

  changepoints <- changepoint_data$changepoints

  # Note: ds = dataspace, since changepoint data refers to indicates, not in dataspace
  previous_changepoint <- 1
  previous_changepoint_ds <- original_data$X[1]
  i <- 1
  # Add each changepoint to the given plotly object
  # TODO: carefully add support for 0 or 1 changepoints
  if (length(changepoints) > 1) {
    for (i in 1:length(changepoints)) {
      changepoint <- changepoints[i]
      changepoint_ds <- original_data$X[changepoint]

      # The region preceeding a changepoint, or between two changepoints
      changeregion <- seq(previous_changepoint, changepoint)
      changeregion_ds <- seq(previous_changepoint_ds,
        changepoint_ds,
        length.out = 3
      )

      changepoint_annotations_regions <- rbind(
        changepoint_annotations_regions,
        data.table(
          x = c(changeregion_ds, NA),
          y = c(rep(changepoint_data$parameters[i], length(changeregion_ds)), NA),
          text = c(rep(
            paste0(
              "State: ", changepoint_data$states[i], "\n",
              "Region mean: ", round(changepoint_data$parameters[i], 2), "\n",
              "Next changepoint: ", round(changepoint_ds, 2)
            ),
            length(changeregion_ds)
          ), NA),
          state = changepoint_data$states[i]
        )
      )
      # If this isn't the first region, connect this region with the last
      if (i > 1) {
        changepoint_annotations <- rbind(
          changepoint_annotations,
          data.table(
            x = c(rep(previous_changepoint_ds, 5), NA),
            y = c(seq(changepoint_data$parameters[i - 1],
              changepoint_data$parameters[i],
              length.out = 2
            ), NA),
            text = c(
              rep(paste0("Changepoint #", i - 1, ": ", round(previous_changepoint_ds, 2)), 5),
              NA
            ),
            state = paste0(changepoint_data$states[i - 1], ",TO,", changepoint_data$states[i])
          )
        )
      }


      # Update the previous changepoints
      previous_changepoint <- changepoint
      previous_changepoint_ds <- changepoint_ds
    }
  } else {
    changepoint_annotations_regions <-
      data.table(
        x = seq(min(original_data$X), max(original_data$X), length.out = 3),
        y = changepoint_data$parameters[1],
        text =
          paste0(
            "State: ", changepoint_data$states[1], "\n",
            "Region mean: ", round(changepoint_data$parameters[1], 2), "\n",
            "Next changepoint: ", round(max(original_data$X), 2)
          ),
        state = changepoint_data$states[i]
      )
  }


  return_plotly <- return_plotly %>%
    add_lines(
      data = changepoint_annotations_regions,
      x = ~x,
      y = ~y,
      color = ~ I("#40B0A6"),
      hoverinfo = "text", text = ~text,
      connectgaps = F,
      key = ~state,
      line = list(width = 7)
    )

  if (nrow(changepoint_annotations) > 0) {
    return_plotly <- return_plotly %>%
      add_lines(
        data = changepoint_annotations,
        x = ~x,
        y = ~y,
        color = ~ I("#E1BE6A"),
        hoverinfo = "text", text = ~text,
        key = ~state,
        connectgaps = F,
        line = list(width = 7)
      )
  }

  list(
    plot = return_plotly %>% layout(hovermode = "x unified"),
    changepoint_annotations = changepoint_annotations,
    changepoint_annotations_regions = changepoint_annotations_regions
  )
}
