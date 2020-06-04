#' Selects the columns from this graph dataframe that are relevant to gfpop
#' @param graph_df the graph to process
#' @returns a dataframe with 9 columns, compatable with gfpop
#' @importFrom dplyr select
#' @importFrom rlang .data 
#' @import gfpop
#' @examples 
#' graph <- gfpop::graph(type = "std")
#' graph$test <- c(1,2)
#' select_graph_columns(graph)
#' @export
select_graph_columns <- function(graph_df) {
  graph_df %>% dplyr::select(.data$state1, .data$state2, .data$type, .data$parameter,
                           .data$penalty, .data$K, .data$a, .data$min, .data$max)
}
