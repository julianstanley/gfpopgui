#' Given data and a constraint graph, generate a changepoint output
#' A wrapper around gfpop::gfpop()
#' @param data_input vector of data to segment
#' @param graph_input dataframe of class "graph" to constraint the changepoint inference
#' @param type a string defining the cost model to use: "mean", "variance", "poisson",
#' "exp", "negbin"
#' @returns A dataframe with columns:
#' \itemize{
#' \item{changepoint}{a vector of X locations where the changepoints begin}
#' \item{changepoint_end}{a vector of X locations where the changepoints end}
#' \item{y}{A vector of Y locations where the changepoints are centered}
#' }
#' @import gfpop
#' @examples 
#' data <- gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
#' c(1, 2, 1, 3, 1), sigma = 1)
#' graph <- gfpop::graph(type = "std")
#' generate_changepoint(data, graph)
#' @export
generate_changepoint <- function(data_input, graph_input, type = "mean") {
  model <-
    gfpop::gfpop(
      data = data_input,
      mygraph = graph_input,
      type = type
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

#' Gets the changepoint location associated with this location on the x axis
#' @param x_loc The X locations to querey
#' @param changepoint_data A dataframe containing changepoint data 
#' (see generate_changepoint).
#' @returns A data.frame/list with the 0 or 1 changepoint location consistent
#' with the given x location
#' @importFrom dplyr filter
#' @importFrom utils head
#' @importFrom rlang .data
#' @examples 
#' data <- gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
#' c(1, 2, 1, 3, 1), sigma = 1)
#' graph <- gfpop::graph(type = "std")
#' changepoint_data <- generate_changepoint(data, graph)
#' get_associated_changepoint(3, changepoint_data)
#' @export
get_associated_changepoint <-
  function(x_loc, changepoint_data) {
    consistent_changepoint_data <- changepoint_data %>%
      dplyr::filter((.data$changepoint >= x_loc) &
                      (.data$changepoint_end <= x_loc))
    return(head(consistent_changepoint_data, 1))
  }

#' Given data and it's associated changepoint data, annotates data points
#' with the changepoint interval in which that data falls
#' @param data_input The data associated with the given changepoints
#' @param changepoint_data The changepoints, in a dataframe format 
#' (see generate_changepoint)
#' @returns A dataframe of the data_input annotated with associated changepoint
#' @importFrom plyr ldply
#' @examples 
#' data <- data.frame(X = 1:10, Y = gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
#' c(1, 2, 1, 3, 1), sigma = 1))
#' graph <- gfpop::graph(type = "std")
#' changepoint_data <- generate_changepoint(data$Y, graph)
#' annotate_data_with_changepoint(data, changepoint_data)
#' @export
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
