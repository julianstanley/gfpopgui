# TODO: this function should deal with all gfpop graph parameters
#' Turns a graph dataframe (from gfpop) into a list that
#' can be read by visNetwork
#' @param graphdf A graph object (in the form of a dataframe) from gfpop
#' Must have the following columns:
#' \itemize{
#' \item{"state1"}
#' \item{"state2"}
#' \item{"}
#' }
#' @param edgeSep A character seperating the nodes in an edge label
#' @returns a list that can be read by visNetwork
#' @import dplyr
#' @import visNetwork
#' @examples
#' graphdf_to_visNetwork(gfpop::graph(type = "std"))
#' @export
graphdf_to_visNetwork <- function(graphdf, edgeSep = "_") {
  graphdf_nonull <- graphdf %>% dplyr::filter(.data$type != "null")

  edge_names <- paste(graphdf_nonull$state1, graphdf_nonull$state2, sep = edgeSep)
  node_names <- unique(c(graphdf_nonull$state1, graphdf_nonull$state2))

  list(
    nodes = data.frame(id = node_names, label = node_names),
    edges = data.frame(
      id = edge_names, label = edge_names,
      to = graphdf_nonull$state2, from = graphdf_nonull$state1
    )
  )
}

#' Generates a visNetwork from a list of nodes and edges
#' @param graph_data A list containing 'nodes' and 'edges' is visNetwork format
#' @returns a visNetwork object
#' @import visNetwork
#' @examples 
#' generate_visNetwork(graphdf_to_visNetwork(gfpop::graph(type = "std")))
#' @export
generate_visNetwork <- function(graph_data) {
  visNetwork(graph_data$nodes, graph_data$edges) %>%
    visEdges(
      arrows = "to", physics = FALSE,
      smooth = list(
        type = "curvedCW",
        roundness = 0.2
      )
    ) %>%
    visOptions(manipulation = list(
      enabled = TRUE,
      editEdge = TRUE
    ))
}
