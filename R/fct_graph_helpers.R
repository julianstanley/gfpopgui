#' Selects the columns from this graph dataframe that are relevant to gfpop
#' @param graph_df the graph to process
#' @returns a dataframe with 9 columns, compatable with gfpop
#' @importFrom dplyr select
#' @import gfpop
#' @examples 
#' graph <- gfpop::graph(type = "std")
#' graph$test <- c(1,2)
#' select_graph_columns(graph)
#' @export
select_graph_columns <- function(graph_df) {
  # CMD Check compatibility section
  .SD <- NULL
  # End CMD compatibility section
  
  graph_df <- data.table(graph_df)
  graph_df[, .SD, .SDcols = c("state1", "state2", "type", "parameter",
                             "penalty", "K", "a", "min", "max")]
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
#' @importFrom dplyr %>% filter
#' @examples 
#' graph <- gfpop::graph(type = "std")
#' graph_to_R_code(graph)
#' @export
graph_to_R_code <- function(graph) {
  # CMD Check compatibility section
  type <- NULL
  # End CMD compatibility section

  valid_colnames <- c("state1", "state2",
                      "type", "parameter",
                      "penalty", "K", "a", 
                      "min", "max")
  if(!all(sapply(colnames(graph), function(x) x %in% valid_colnames)) |
     !all(sapply(valid_colnames, function(x) x %in% colnames(graph)))) {
    stop("Invalid column names. Is this a dataframe returned from gfpop::graph?")
  }
  
  return_command <- "gfpop::graph(\n"
  
  graph_without_startEnd <- data.table(graph) %>%
    filter(type != "start" & type != "end")
  
  graph_with_start <- data.table(graph) %>%
    filter(type == "start")
  hasStart <- dim(graph_with_start)[1] > 0
  
  graph_with_end <- data.table(graph) %>%
    filter(type == "end")
  hasEnd <- dim(graph_with_end)[1] > 0

  apply(graph_without_startEnd, 1, function(x) {
    return_command <<- paste0(return_command, paste0("    ", 
                                                     format_edge(x), ",\n"))
  })
  
  if(hasStart & hasEnd) {
    return_command <- paste0(return_command, "    StartEnd(start = '", 
                             graph_with_start[["state1"]][1], "', end = '",
                             graph_with_end[["state1"]][1], "'),\n")
  } else if(hasStart) {
    return_command <- paste0(return_command, "    StartEnd(start = '", 
                             graph_with_start[["state1"]][1], "'),\n")
  } else if(hasEnd) {
    return_command <- paste0(return_command, "    StartEnd(end = '",
                             graph_with_end[["state1"]][1], "'),\n")
  }
  
  
  paste0(substr(return_command, 1, nchar(return_command) - 2), "\n)")
}
