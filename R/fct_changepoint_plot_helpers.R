#' Makes a ggplot object with the changepoint data
#' @param data_input The data associated with the given changepoints
#' @param changepoint_data The changepoints, in a dataframe format 
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot geom_point aes geom_segment xlab ylab
#' @importFrom rlang .data 
#' @examples
#' data <- data.frame(X = 1:100, Y = gfpop::dataGenerator(100, c(0.1, 0.3, 0.5, 0.8, 1), 
#' c(1, 2, 1, 3, 1), sigma = 1))
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
