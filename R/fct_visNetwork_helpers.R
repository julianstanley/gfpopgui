#' Converts all NA objects in a vector to "None"
#' @param vec a vector or unnamed list (vector equivalent)
#' @returns a unnamed list (vector equivalent) where "None"-->NA
#' @export
NAtoNone <- function(vec) {
  lapply(vec, function(x) if (is.na(x)) "None" else x)
}

#' Converts all "None" objects in a vector into NA
#' @param vec a vector or unnamed list (vector equivalent)
#' @returns a unnamed list (vector equivalent) where "None"-->NA
#' @export
NonetoNA <- function(vec) {
  vectemp <- NAtoNone(vec)
  lapply(vectemp, function(x) if (x == "None") NA else x)
}

#' Turns a graph dataframe (from gfpop) into a list that
#' can be read by visNetwork
#' @param graphdf A graph object (in the form of a dataframe) from gfpop
#' Must have the following columns:
#' \itemize{
#' \item{"state1"}
#' \item{"state2"}
#' \item{"type"}
#' \item{"parameter"}
#' \item{"penalty"}
#' \item{"K"}
#' \item{"a"}
#' \item{"min"}
#' \item{"max"}
#' }
#' @param edgeSep A character seperating the nodes in an edge label
#' @param hideNull (Boolean) hide null edges?
#' @returns a list that can be read by visNetwork
#' @importFrom dplyr filter %>%
#' @importFrom rlang .data 
#' @import visNetwork
#' @examples
#' graphdf_to_visNetwork(gfpop::graph(type = "std"))
#' @export
graphdf_to_visNetwork <- function(graphdf, edgeSep = "_", showNull = TRUE) {
  class(graphdf) <- "data.frame"

  edge_names <- paste(graphdf$state1, graphdf$state2, sep = edgeSep)
  node_names <- unique(c(graphdf$state1, graphdf$state2))

  # Set null-specific paramaters
  selfReference.angle <- c()
  hidden <- c()
  for(i in 1:nrow(graphdf)) {
    row <- graphdf[i,]
    if(row$state1 != row$state2) {
      selfReference.angle <- c(selfReference.angle, NA)
      hidden <- c(hidden, FALSE)
    } else if(row$state1 == row$state2) {
      if(row$type == "null") {
        hidden <- if(showNull) c(hidden, FALSE) else c(hidden, TRUE)
        selfReference.angle <- c(selfReference.angle, pi)
      } else {
        hidden <- c(hidden, FALSE)
        selfReference.angle <- c(selfReference.angle, 2*pi)
      }
    }
  }
  list(
    nodes = data.frame(id = node_names, label = node_names, size = rep(40, length(node_names))),
    edges = data.frame(
      id = paste(graphdf$state1, graphdf$state2, graphdf$type, sep = "_"), 
      label = paste0(graphdf$type, " | ", graphdf$penalty),
      to = graphdf$state2, from = graphdf$state1,
      type = graphdf$type, parameter = graphdf$parameter,
      penalty = graphdf$penalty, K = as.character(graphdf$K), 
      a = graphdf$a, min = as.character(NAtoNone(graphdf$min)), max = as.character(NAtoNone(graphdf$max)),
      selfReference.angle = selfReference.angle,
      selfReference.size = rep(40, length(graphdf$state1)),
      hidden = hidden
    )
  )
}


#' Turns a list that can be read by visNetwork into graph dataframe (for gfpop)
#' @param visNetwork_list A list object compatable with visNetwork
#' See graphdf_to_visNetwork.
#' @returns a dataframe/graph for gfpop
#' @importFrom dplyr filter %>%
#' @importFrom rlang .data 
#' @import visNetwork
#' @examples
#' visNetwork_to_graphdf(graphdf_to_visNetwork(gfpop::graph(type = "std")))
#' @export
visNetwork_to_graphdf <- function(visNetwork_list) {
  edges <- visNetwork_list$edges
  edges$state1 <- edges$from
  edges$state2 <- edges$to
  edges$parameter <- as.numeric(edges$parameter)
  edges$penalty <- as.numeric(edges$penalty)
  edges$K <- as.numeric(edges$K)
  edges$a <- as.numeric(edges$a)
  edges$min <- as.numeric(NonetoNA(edges$min))
  edges$max <- as.numeric(NonetoNA(edges$max))
  gfpop::graph(select_graph_columns(edges))
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
      arrows = "to", physics = F,
      smooth = list(
        type = "curvedCW",
        roundness = 0.2
      ),
      font = list(align = 'top')
    ) %>%
    visOptions(manipulation = list(
      enabled = TRUE,
      editEdgeCols = c("from", "to", 
                       "type", "parameter", "penalty", "K", "a", "min", "max", "hidden")
    )) %>%
    visLayout(randomSeed = 123)
}
