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
  class(graph_df) <- "data.frame"
  gfpop::graph(
    dplyr::select(graph_df, .data$state1, .data$state2, .data$type, .data$parameter,
                           .data$penalty, .data$K, .data$a, .data$min, .data$max)
  )
}

#' Takes in a row from a graph dataframe, returns that row formatted as R code
#' @param edge_df One row from a gfpop::graph() graph df, with column names
#' @returns a string corresponding to the code that, when run, produces the 
#' given edge
#' @examples 
#' graph <- gfpop::graph(type = "std")
#' format_edge(graph[1,])
#' @export
format_edge <- function(edge_df) {
  paste0("gfpop::Edge(state1 = '", edge_df[["state1"]], "'",
         ", state2 = '", edge_df[["state2"]], "'",
         ", type = '", edge_df[["type"]], "'",
         ", gap = ", edge_df[["parameter"]],
         ", penalty = ", edge_df[["penalty"]],
         ", K = ", edge_df[["K"]],
         ", a = ", edge_df[["a"]], 
         ")")
}

#' Takes in a graph dataframe, returns the graph formatted as R code
#' @param graph A graph df, like that returned by gfpop::graph()
#' @returns a string corresponding to the code that, when run, produces the
#' given graph
#' @examples 
#' graph <- gfpop::graph(type = "std")
#' graph_to_R_code(graph)
#' @export
graph_to_R_code <- function(graph) {
  valid_colnames <- c("state1", "state2",
                      "type", "parameter",
                      "penalty", "K", "a", 
                      "min", "max")
  if(!all(colnames(graph) == valid_colnames)) {
    stop("Invalid column names. Is this a dataframe returned from gfpop::graph?")
  }
  
  return_command <- "gfpop::graph(\n"
  
  apply(graph, 1, function(x) {
    return_command <<- paste0(return_command, paste0("    ", 
                                                     format_edge(x), ",\n"))
  })
  
  paste0(substr(return_command, 1, nchar(return_command) - 2), "\n)")
}