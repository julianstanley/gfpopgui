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

#' Creates a 'label' array for a graphdf, given the columns that you
#' want to include in the label and the seperator
#' @param graphdf A graph object (in the form of a dataframe) from gfpop
#' or the "edges" from a visNetwork list.
#' @param columns a character vector of columns to be included in the label.
#' @param collapse A string to seperate each item
#' @returns a character vector combining those columns as specified
create_label <- function(graphdf, columns = c("type", "penalty"), collapse = " | ") {
  apply(graphdf[, columns], 1, paste, collapse = collapse)
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

  # Keep track of starting and ending nodes, but seperate them from the rest
  starts <- graphdf %>% filter(type == "start") %>% select(state1) %>% .$state1
  ends <- graphdf %>% filter(type == "end") %>% select(state1) %>% .$state1
  graphdf <- graphdf %>% filter(type != "start" & type != "end")
  
  # Set edge and node names
  edge_names <- paste(graphdf$state1, graphdf$state2, sep = edgeSep)
  node_names <- unique(c(graphdf$state1, graphdf$state2))

  # Determine the value of selfReference.angle and hidden (edge params)
  selfReference.angle <- c()
  hidden <- c()
  apply(graphdf, 1, function(x) {
    if (x[["state1"]] != x[["state2"]]) {
      selfReference.angle <<- c(selfReference.angle, NA)
      hidden <<- c(hidden, FALSE)
    } else if (x[["type"]] == "null") {
      hidden <<- if (showNull) c(hidden, FALSE) else c(hidden, TRUE)
      selfReference.angle <<- c(selfReference.angle, pi)
    } else {
      hidden <<- c(hidden, FALSE)
      selfReference.angle <<- c(selfReference.angle, 2 * pi)
    }
  })
  
  # Determine the value of start and end (node params)
  startbool <- sapply(node_names, 
                     function(x) tolower(x) %in% tolower(starts), USE.NAMES = F)
  endbool <- sapply(node_names, 
                    function(x) tolower(x) %in% tolower(ends), USE.NAMES = F)

  

  list(
    nodes = data.frame(id = node_names, label = node_names, size = 40,
                       start = startbool, end = endbool),
    edges = data.frame(
      id = paste(graphdf$state1, graphdf$state2, graphdf$type, sep = edgeSep),
      label = create_label(graphdf), to = graphdf$state2, from = graphdf$state1,
      type = graphdf$type, parameter = graphdf$parameter,
      penalty = graphdf$penalty, K = as.character(graphdf$K), a = graphdf$a,
      min = as.character(NAtoNone(graphdf$min)), max = as.character(NAtoNone(graphdf$max)),
      selfReference.angle = selfReference.angle, selfReference.size = 40, hidden = hidden
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

additional_js <- "function(el, x) {
// Validate edge type when the save button is pressed
$('#editedge-saveButton').on('click', function() {
let type = $('#editedge-type').val();
if (!['null', 'std', 'up', 'down', 'abs'].includes(type.toLowerCase())) {
alert(`${type} is not a valid type. Defaulting to null`);
$('#editedge-type').val('null');
}
})
}
"
#' Generates a visNetwork from a list of nodes and edges
#' @param graph_data A list containing 'nodes' and 'edges' is visNetwork format
#' @returns a visNetwork object
#' @import visNetwork
#' @importFrom htmlwidgets onRender
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
      font = list(align = "top")
    ) %>%
    visOptions(manipulation = list(
      enabled = TRUE,
      editEdgeCols = c(
        "from", "to",
        "type", "parameter", "penalty", "K", "a", "min", "max"
      )
    )) %>%
    visLayout(randomSeed = 123) %>%
    onRender(additional_js)
}

#' Edits a visNetwork data based on the given command, consistent with the format
#' of commands passed to the _graphChange observers in visNetwork-shiny
#' @param event The event, with at least a $cmd entry, based on observer format
#' @param graphdata_visNetwork A list of data to be passed to visNetwork
#' @returns the same format as graphdata_visNetwork, but edited according to event
#' @import visNetwork
#' @export
modify_visNetwork <- function(event, graphdata_visNetwork) {
  graphdata_visNetwork_return <- graphdata_visNetwork
  if (!is.null(event$type)) {
    event$type <- tolower(event$type)
    if (!(event$type %in% c("null", "std", "up", "down", "abs"))) {
      warning("Invalid 'type' parameter, returning unchanged data and a refresh recommendation.")
      return(list(data = graphdata_visNetwork, refresh = TRUE))
    }
  }
  ### Edit Edge --------------------------------------------------------------
  if (event$cmd == "editEdge") {
    changed_id <- event$id
    # Decide whether we need to add selfReference.angle
    if (event$to == event$from) {
      angle <- if (event$type == "null") pi else 2 * pi
    } else {
      angle <- "NA"
    }

    graphdata_visNetwork_return$edges <- graphdata_visNetwork_return$edges %>%
      mutate_cond(id == changed_id,
        label = paste0(event$type, " | ", event$penalty),
        to = event$to, from = event$from,
        type = event$type, parameter = event$parameter,
        penalty = event$penalty, K = event$K, a = event$a,
        min = event$min, max = event$max,
        selfReference.angle = angle, selfReference.size = 40,
      )

  }

  ### Add Edge ---------------------------------------------------------------
  if (event$cmd == "addEdge") {
    new_row <- data.frame(
      id = event$id,
      label = "null | 0",
      to = event$to, from = event$from,
      type = "null", parameter = "1",
      penalty = "0", K = "Inf", a = "0",
      min = "None", max = "None",
      selfReference.angle = NA, selfReference.size = 40, hidden = FALSE
    )

    graphdata_visNetwork_return$edges <- rbind(
      graphdata_visNetwork_return$edges,
      new_row
    )
  }

  ### Delete Edge ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$edges) > 0)) {
    for (del_edge in event$edges) {
      graphdata_visNetwork_return$edges <-
        graphdata_visNetwork_return$edges %>%
        dplyr::filter(.data$id != del_edge)
    }
  }

  ### Add Node ---------------------------------------------------------------
  if (event$cmd == "addNode") {
    graphdata_visNetwork_return$nodes <- rbind(
      graphdata_visNetwork_return$nodes,
      data.frame(
        id = event$id,
        label = event$label,
        size = 40
      )
    )
  }

  ### Edit Node --------------------------------------------------------------
  if (event$cmd == "editNode") {
    graphdata_visNetwork_return$nodes <-
      graphdata_visNetwork_return$nodes %>%
      mutate_cond(id == event$id,
        label = event$label
      )
  }

  ### Delete Node ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
    for (del_node in event$nodes) {
      graphdata_visNetwork_return$nodes <-
        graphdata_visNetwork_return$nodes %>%
        dplyr::filter(.data$id != del_node)
    }
  }

  graphdata_visNetwork_return
}
#'
