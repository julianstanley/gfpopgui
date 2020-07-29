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
#' @export
create_label <- function(graphdf, columns = c("type", "penalty"), collapse = " | ") {
  # For compatability with graphdf and visNetwork edges
  if ("to" %in% colnames(graphdf)) {
    columns <- sapply(columns, function(x) {
      if (x == "state1") {
        "from"
      } else if (x == "state2") {
        "to"
      } else {
        x
      }
    }, USE.NAMES = F)
  }
  if (length(columns) > 1) {
    as.character(apply(graphdf[, columns], 1, paste, collapse = collapse))
  } else if (length(columns) == 1) {
    as.character(graphdf[, columns[1]])
  } else {
    as.character(rep(" ", dim(graphdf)[1]))
  }
}

#' Creates an individual label for an individual edge
create_label_individual <- function(state1, state2, type, parameter,
                                    penalty, K, a, min, max,
                                    columns = c("type", "penalty"), collapse = " | ") {
  paste(sapply(columns,
    function(x) eval(as.symbol(x)),
    USE.NAMES = F
  ),
  collapse = collapse
  )
}

#' Add a new node to a visNetwork node dataframe
#' @param nodedf a dataframe containing the nodes.
#' @param id id of the new node.
#' @param label label of the new node. default: ""
#' @param size size of the new node. Default: 40
#' @param start is this node a start node? Default: FALSE
#' @param end is this node an end node? Default: FALSE
#' @param shape the shape of this node. See visNetwork docs. Default: "dot"
#' @param color.background the background color of the node. Default: "lightblue"
#' @param color.border the border color of the node. Default: "lightblue"
#' @param shadow Whether this node has a shadow. Default: false
#' @returns a dataframe with one more row than nodedf
add_node <- function(nodedf, id, label = "", size = 40, start = FALSE,
                     end = FALSE, shape = "dot", color.background = "lightblue",
                     color.border = "lightblue", shadow = FALSE) {
  rbind(
    nodedf,
    data.frame(
      id = id, label = label, size = size, start = start,
      end = end, shape = shape, color.background = color.background,
      color.border = color.border, shadow = shadow
    )
  )
}

#' Add a new edge to a visNetwork edge dataframe
#' @param edgedf a dataframe containing the edges.
#' @param id edge id
#' @param label edge label
#' @param to Where does the edge go to?
#' @param from Where does the edge come from?
#' @param type What type of edge?
#' @param parameter parameter: gap, etc.
#' @param penalty edge penalty
#' @param K K (see gfpop)
#' @param a a (see gfpop)
#' @param min min (see gfpop)
#' @param max max (see gfpop)
#' @param selfReference.angle The angle of this edge, if it's recursive
#' @param selfReference.size The length of this edge, if it's recursive
#' @param hidden Is this edge hidden?
#' @param color edge color
#' @returns a dataframe with one more row than edgedf
add_edge <- function(edgedf, id, label, to, from, type, parameter,
                     penalty, K, a, min, max, selfReference.angle = NA,
                     selfReference.size = 40, hidden = FALSE, color = "black") {
  new_row <- data.frame(
    id = id, label = label, to = to, from = from, type = type,
    parameter = parameter, penalty = penalty, K = K, a = a, min = min,
    max = max, selfReference.angle = selfReference.angle,
    selfReference.size = selfReference.size, hidden = hidden, color = color
  )

  rbind(edgedf, new_row)
}

#' Adds a recursive null edge
#' @param edgedf a dataframe containing the edges
#' @param nodeid the id of the node on which to create this recursive edge
#' @returns a dataframe with one more row than edgedf
add_null_edge <- function(edgedf, nodeid) {
  add_edge(
    edgedf = edgedf,
    id = paste0(nodeid, "_", nodeid, "_null"),
    label = "null | 0", to = nodeid, from = nodeid,
    type = "null", parameter = "1", penalty = "0", K = "Inf", a = "0",
    min = "None", max = "None",
    selfReference.angle = pi, selfReference.size = 40, hidden = FALSE,
    color = "black"
  )
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
#' @param label_columns (character) An array of columns to use to make edge labels
#' @returns a list that can be read by visNetwork
#' @importFrom dplyr filter %>%
#' @importFrom rlang .data
#' @import visNetwork
#' @examples
#' graphdf_to_visNetwork(gfpop::graph(type = "std"))
#' @export
graphdf_to_visNetwork <- function(graphdf, edgeSep = "_", showNull = TRUE,
                                  label_columns = c("type", "penalty")) {
  class(graphdf) <- "data.frame"

  # Keep track of starting and ending nodes, but seperate them from the rest
  starts <- graphdf %>%
    filter(type == "start") %>%
    select(state1) %>%
    .$state1
  ends <- graphdf %>%
    filter(type == "end") %>%
    select(state1) %>%
    .$state1
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
    function(x) tolower(x) %in% tolower(starts),
    USE.NAMES = F
  )
  endbool <- sapply(node_names,
    function(x) tolower(x) %in% tolower(ends),
    USE.NAMES = F
  )
  shape <- mapply(function(start, end) {
    if (start & end) {
      "star"
    } else if (start) {
      "triangle"
    } else if (end) {
      "square"
    } else {
      "dot"
    }
  }, startbool, endbool)
  nodes <- data.frame(
    id = node_names, label = node_names, size = 40,
    start = startbool, end = endbool, shape = shape,
    color.background = "lightblue", color.border = "lightblue",
    shadow = FALSE
  )

  if ("state1_id" %notin% graphdf) {
    edges <- data.frame(
      id = paste(graphdf$state1, graphdf$state2, graphdf$type, sep = edgeSep),
      label = create_label(graphdf, columns = label_columns), to = graphdf$state2, from = graphdf$state1,
      type = graphdf$type, parameter = graphdf$parameter,
      penalty = graphdf$penalty, K = as.character(graphdf$K), a = graphdf$a,
      min = as.character(NAtoNone(graphdf$min)), max = as.character(NAtoNone(graphdf$max)),
      selfReference.angle = selfReference.angle, selfReference.size = 40, hidden = hidden,
      color = "black"
    )
  } else {
    edges <- data.frame(
      id = paste(graphdf$state1, graphdf$state2, graphdf$type, sep = edgeSep),
      label = create_label(graphdf, columns = label_columns), to = graphdf$state2_id, from = graphdf$state1_id,
      type = graphdf$type, parameter = graphdf$parameter,
      penalty = graphdf$penalty, K = as.character(graphdf$K), a = graphdf$a,
      min = as.character(NAtoNone(graphdf$min)), max = as.character(NAtoNone(graphdf$max)),
      selfReference.angle = selfReference.angle, selfReference.size = 40, hidden = hidden,
      color = "black"
    )
  }
  list(
    nodes = nodes, edges = edges
  )
}


#' Turns a list that can be read by visNetwork into graph dataframe (for gfpop)
#' @param visNetwork_list A list object compatable with visNetwork
#' See graphdf_to_visNetwork.
#' @returns a dataframe/graph for gfpop
#' @importFrom dplyr filter %>%
#' @importFrom rlang .data
#' @import visNetwork
#' @importFrom gfpop gfpop
#' @examples
#' visNetwork_to_graphdf(graphdf_to_visNetwork(gfpop::graph(type = "std")))
#' @export
visNetwork_to_graphdf <- function(visNetwork_list) {
  if (nrow(visNetwork_list$edges) == 0) {
    return(data.frame())
  }

  edges <- visNetwork_list$edges
  nodes <- visNetwork_list$nodes

  # State1 and State1 should actually be based on labels, not IDs,
  # But we should also keep track of the IDs for when we convert back to
  # visNetwork
  state1 <- unlist(
    apply(
      edges, 1,
      function(x) {
        nodes %>%
          filter(id == x[["from"]]) %>%
          select(label)
      }
    ),
    use.names = F
  )
  state1_id <- edges$from

  state2 <- unlist(
    apply(
      edges, 1,
      function(x) {
        nodes %>%
          filter(id == x[["to"]]) %>%
          select(label)
      }
    ),
    use.names = F
  )
  state2_id <- edges$to
  type <- edges$type
  parameter <- as.numeric(edges$parameter)
  penalty <- as.numeric(edges$penalty)
  K <- as.numeric(edges$K)
  a <- as.numeric(edges$a)
  min <- as.numeric(NonetoNA(edges$min))
  max <- as.numeric(NonetoNA(edges$max))
  data.frame(
    state1 = state1, state1_id = state1_id, state2 = state2, type = type,
    state2_id = state2_id, parameter = parameter, penalty = penalty,
    K = K, a = a, min = min, max = max
  )
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
    visOptions(
      manipulation = list(
        enabled = TRUE,
        editEdgeCols = c(
          "type", "parameter", "penalty", "K", "a", "min", "max"
        ),
        editNodeCols = c(
          "label"
        ),
        addNodeCols = c(
          "label"
        )
      ),
      highlightNearest = list(enabled = T, degree = 0, hover = F)
    ) %>%
    visLayout(randomSeed = 123) %>%
    onRender(additional_js)
}

#' Edits a visNetwork data based on the given command, consistent with the format
#' of commands passed to the _graphChange observers in visNetwork-shiny
#' @param event The event, with at least a $cmd entry, based on observer format
#' @param graphdata_visNetwork A list of data to be passed to visNetwork
#' @param addNull Whether to automatically add a null edge to a new node
#' @returns the same format as graphdata_visNetwork, but edited according to event
#' @import visNetwork
#' @export
modify_visNetwork <- function(event, graphdata_visNetwork, addNull = FALSE) {
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

    angle <- if (event$type == "null") pi else 2 * pi

    graphdata_visNetwork_return$edges <- graphdata_visNetwork_return$edges %>%
      mutate_cond(id == changed_id,
        label = paste0(event$type, " | ", event$penalty),
        type = event$type, parameter = event$parameter,
        penalty = event$penalty, K = event$K, a = event$a,
        min = event$min, max = event$max,
        selfReference.angle = angle, selfReference.size = 40,
      )
  }

  ### Add Edge ---------------------------------------------------------------
  if (event$cmd == "addEdge") {
    graphdata_visNetwork_return$edges <- add_edge(
      edgedf = graphdata_visNetwork_return$edges, id = event$id,
      label = "std | 10", to = event$to, from = event$from,
      type = "std", parameter = "0", penalty = "10", K = "Inf", a = "0",
      min = "None", max = "None",
      selfReference.angle = 2 * pi, selfReference.size = 40, hidden = FALSE,
      color = "black"
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
    graphdata_visNetwork_return$nodes <- add_node(graphdata_visNetwork_return$nodes,
      id = event$id, label = event$label
    )

    # If addNull is true, add a recursive null edge
    if (addNull) {
      graphdata_visNetwork_return$edges <- add_null_edge(
        edgedf = graphdata_visNetwork_return$edges, nodeid = event$id
      )
    }
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

#' Updates the given visNetwork edges, based on a new graphdf,
#' excluding updates to certain columns. This is useful as to not mess up
#' visNetwork edge IDs, which are important to keeping visNetwork happy.
#' @param old_edges A dataframe in visNetwork edges format
#' @param graphdf A graph object (in the form of a dataframe) from gfpop
#' @param edgeSep A character seperating the nodes in an edge label
#' @param hideNull (Boolean) hide null edges?
#' @param label_columns (character) An array of columns to use to make edge labels
#' @param columns_to_exclude (character) An array of columns not to include in the update
#' @export
update_visNetwork_edges <- function(old_edges, graphdf, edgeSep = "_", showNull = TRUE,
                                    label_columns = c("type", "penalty"),
                                    columns_to_exclude = c("id", "to", "from")) {
  return_edges <- old_edges
  new_edges <- graphdf_to_visNetwork(graphdf, edgeSep, showNull, label_columns)$edges
  for (column in colnames(old_edges)) {
    if (column %notin% columns_to_exclude) {
      return_edges[[column]] <- new_edges[[column]]
    }
  }

  return_edges
}
