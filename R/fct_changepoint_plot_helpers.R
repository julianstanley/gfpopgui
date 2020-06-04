#' Makes a ggplot object with the changepoint data
#' @param annotated_data an annotated changepoint dataset
#' @returns a ggplot object
#' @importFrom ggplot2 ggplot geom_point aes geom_segment xlab ylab
#' @examples
#' data <- data.frame(X = 1:10, Y = gfpop::dataGenerator(10, c(0.1, 0.3, 0.5, 0.8, 1), 
#' c(1, 2, 1, 3, 1), sigma = 1))
#' graph <- gfpop::graph(type = "std")
#' changepoint_data <- generate_changepoint(data$Y, graph)
#' annotated_data <- annotate_data_with_changepoint(data, changepoint_data)
#' plot_changepoint(annotated_data)
#' @export
plot_changepoint <- function(annotated_data) {
  ggplot(annotated_data, aes(
    x = X,
    y = Y,
    text = CP_Data
  )) +
    geom_point() +
    geom_segment(
      aes(
        x = changepoint,
        xend = changepoint_end,
        y = y,
        yend = y
      ),
      size = 1.5,
      col = "red"
    ) +
    xlab("X units (arbitrary)") +
    ylab("Univariate gaussian data (randomly generated)")
}
