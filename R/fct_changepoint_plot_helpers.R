#' Adds changepoints annotations to a plotly plot.
#' @param plotly_obj a plotly object
#' @param original_data A dataset with two columns
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
  # changepoints_annotations_regions describe the segments (horizontal) between
  changepoint_annotations_regions <- data.table()
  changepoint_annotations <- data.table()
  
  # Full range of X data
  data_range <- max(original_data[[1]]) - min(original_data[[1]])
  
  # How many total points to render for changeregions
  total_changeregion_granularity <- 1000

  # ds = dataspace, since changepoint data refers to indices, not in dataspace
  changepoints <- changepoint_data$changepoints
  previous_changepoint <- 1
  previous_changepoint_ds <- original_data[[1]][1]

  # Add each changepoint to the given plotly object
  if (length(changepoints) > 1) {
    lapply(1:length(changepoints), FUN = function(i) {
      changepoint <- changepoints[i]
      previous_changepoint <- if (i > 1) changepoints[i - 1] else 1
      changepoint_ds <- original_data[[1]][changepoint]
      previous_changepoint_ds <- original_data[[1]][previous_changepoint]
      
      # Decide on how many points to render in this changeregion
      num_points <- ((changepoint_ds - previous_changepoint_ds) / (data_range)) * total_changeregion_granularity

      changeregion <- seq(previous_changepoint, changepoint)
      changeregion_ds <- seq(previous_changepoint_ds,
        changepoint_ds,
        length.out = num_points
      )

      changepoint_annotations_regions <<- rbindlist(
        list(
          changepoint_annotations_regions,
          make_region(
            region_x = changeregion_ds,
            parameter_y = changepoint_data$parameters[i],
            state = changepoint_data$states[i],
            next_cp = changepoint_ds, add_na = TRUE
          )
        )
      )

      if (i > 1) {
        changepoint_annotations <<- rbindlist(
          list(
            changepoint_annotations,
            make_changepoint(
              parameter_x = previous_changepoint_ds,
              region_y = seq(changepoint_data$parameters[i - 1],
                changepoint_data$parameters[i],
                length.out = 2
              ),
              new_state = changepoint_data$states[i],
              previous_state = changepoint_data$states[i - 1],
              changepoint_number = i - 1
            )
          )
        )
      }
    })
  } else {
    changepoint_annotations_regions <-
      make_region(
        region_x = seq(min(original_data[[1]]), max(original_data[[1]]),
          length.out = total_changeregion_granularity
        ),
        parameter_y = changepoint_data$parameters[1],
        state = changepoint_data$states[1],
        next_cp = max(original_data[[1]]),
        add_na = FALSE
      )
  }

  # Add the changeregions on top of the base plotly plot
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
    # There must also be (vertical) changepoints, so add those on top of the
    # base plotly plot
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

#' Make a changeregion data.table
#' @param region_x The x values over which this region spans
#' @param parameter_y The median of this region
#' @param state The state of this region
#' @param next_cp The median of the next region
#' @param add_na Whether to add a row of NAs at the bottom of the return table
#' @returns (data.table) describing the changeregion
make_region <- function(region_x, parameter_y, state, next_cp = NaN, add_na = TRUE) {
  x <- region_x
  y <- rep(parameter_y, length(x))
  text_individual <- paste0(
    "State: ", state, "\n",
    "Region mean: ", round(parameter_y, 2), "\n",
    "Next changepoint: ", round(next_cp, 2)
  )
  text <- rep(text_individual, length(x))
  state <- rep(state, length(x))

  if (add_na) {
    x <- c(x, NA)
    y <- c(y, NA)
    text <- c(text, NA)
    state <- c(state, NA)
  }

  data.table(x = x, y = y, text = text, state = state)
}

#' Make a changepoint data.table
#' @param parameter_x The x location of this changepoint
#' @param region_y A range of values between the mean of the previous changeregion
#' and the mean of the next changeregion
#' @param new_state The name of the next changeregion's state
#' @param previous_state The name of the previous changeregion's state
#' @param changepoint_number The index/number of this changepoint (default NaN)
#' @param add_na Whether to add a row of NAs at the bottom of the return table
#' @returns (data.table) describing the changepoint
make_changepoint <- function(parameter_x, region_y, new_state, previous_state, changepoint_number = NaN,
                             add_na = TRUE) {
  y <- region_y
  x <- rep(parameter_x, length(y))

  state_individual <- paste0(previous_state, ",TO,", new_state)
  state <- rep(state_individual, length(y))

  text_individual <- paste0(
    "Changepoint at: ", round(parameter_x, 2), " ; ", state_individual
  )
  text_individual <- if (is.nan(changepoint_number)) {
    text_individual
  } else {
    paste0(text_individual, " (CP #", changepoint_number, ")")
  }

  text <- rep(text_individual, length(y))

  if (add_na) {
    x <- c(x, NA)
    y <- c(y, NA)
    text <- c(text, NA)
    state <- c(state, NA)
  }

  data.table(x = x, y = y, text = text, state = state)
}
