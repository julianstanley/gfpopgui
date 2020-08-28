visNetwork_attachjs<- "function(el, x) {
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
#' @param graphdf A graph object (in the form of a data.table) from gfpop
#' or the "edges" from a visNetwork list.
#' @param columns a character vector of columns to be included in the label.
#' @param collapse A string to seperate each item
#' @returns a character vector combining those columns as specified
#' @export
create_label <- function(graphdf, columns = c("type", "penalty"), collapse = " | ") {
  graphdf <- data.table(graphdf)
  # For compatability with graphdf and visNetwork edges
  if (("to" %in% columns | "from" %in% columns) &
    ("to" %notin% colnames(graphdf))) {
    columns <- sapply(columns, function(x) {
      if (x == "from") {
        "state1"
      } else if (x == "to") {
        "state2"
      } else {
        x
      }
    }, USE.NAMES = F)
  }
  if (length(columns) > 1) {
    as.character(apply(graphdf[, columns, with = F], 1, paste, collapse = collapse))
  } else if (length(columns) == 1) {
    graphdf[, columns[1], with = F][[columns[1]]]
  } else {
    as.character(rep(" ", dim(graphdf)[1]))
  }
}

#' Creates an individual label for an individual edge
#' @param state1 The state where the edge originates
#' @param state2 The state to which the edge travels
#' @param type The type of edge (see gfpop::graph types)
#' @param parameter A parameter, such as '1' for null, or gap otherwise
#' @param penalty An edge penalty (see gfpop::graph)
#' @param K see gfpop::graph
#' @param a see gfpop::graph
#' @param min see gfpop::graph
#' @param max see gfpop::graph
#' @param columns The columns to include in this edge
#' @param collapse the character separating edge columns in this label
#' @export
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
#' @param nodedf a data.table containing the nodes.
#' @param id id of the new node.
#' @param label label of the new node. default: ""
#' @param size size of the new node. Default: 40
#' @param start is this node a start node? Default: FALSE
#' @param end is this node an end node? Default: FALSE
#' @param shape the shape of this node. See visNetwork docs. Default: "dot"
#' @param color.background the background color of the node. Default: "lightblue"
#' @param color.border the border color of the node. Default: "lightblue"
#' @param shadow Whether this node has a shadow. Default: false
#' @importFrom data.table rbindlist
#' @returns a data.table with one more row than nodedf
add_node <- function(nodedf, id, label = "", size = 40, start = FALSE,
                     end = FALSE, shape = "dot", color.background = "lightblue",
                     color.border = "lightblue", shadow = FALSE) {
  rbindlist(
    list(
      nodedf,
      list(
        id = id, label = label, size = size, start = start,
        end = end, shape = shape, color.background = color.background,
        color.border = color.border, shadow = shadow
      )
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
#' @importFrom data.table rbindlist
add_edge <- function(edgedf, id, label, to, from, type, parameter,
                     penalty, K = Inf, a = 0, min = NA, max = NA, selfReference.angle = NA,
                     selfReference.size = 40, hidden = FALSE, color = "black") {
  new_row <- list(
    id = id, label = label, to = to, from = from, type = type,
    parameter = parameter, penalty = penalty, K = K, a = a, min = min,
    max = max, selfReference.angle = selfReference.angle,
    selfReference.size = selfReference.size, hidden = hidden, color = color
  )

  rbindlist(list(edgedf, new_row))
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
    type = "null", parameter = 1, penalty = 0, K = Inf, a = 0,
    min = NA, max = NA,
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
#' @param showNull (Boolean) hide null edges?
#' @param label_columns (character) An array of columns to use to make edge labels
#' @param edge_ids An array of ids, one for each edge in the graphdf
#' @param label_collapse the character separating edge columns in this label
#' @returns a list that can be read by visNetwork
#' @import visNetwork
#' @examples
#' graphdf_to_visNetwork(gfpop::graph(type = "std"), edge_ids = 1:2)
#' @export
graphdf_to_visNetwork <- function(graphdf, edgeSep = "_", showNull = TRUE,
                                  label_columns = c("type", "penalty"),
                                  label_collapse = " | ",
                                  edge_ids = c()) {
  # CMD Check compatibility section
  type <- NULL
  state1 <- NULL
  # End CMD compatibility section

  graphdf <- data.table(graphdf)
  
  # Early return for an empty graphdf
  if(nrow(graphdf) == 0) {
    return(list(nodes = data.table(), edges = data.table()))
  }

  # Keep track of starting and ending nodes, but separate them from the rest
  starts <- graphdf[type == "start", state1]
  ends <- graphdf[type == "end", state1]
  graphdf <- graphdf[type != "start" & type != "end"]

  # Set edge and node names
  edge_names <- paste(graphdf$state1, graphdf$state2, graphdf$type, sep = edgeSep)
  node_names <- unique(c(graphdf$state1, graphdf$state2))

  if (length(edge_ids) == 0) {
    edge_ids <- edge_names
  }

  # Determine the value of dynamic edge parameters
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

  # Determine which nodes are start or end nodes
  startbool <- sapply(node_names,
    function(x) tolower(x) %in% tolower(starts),
    USE.NAMES = F
  )
  endbool <- sapply(node_names,
    function(x) tolower(x) %in% tolower(ends),
    USE.NAMES = F
  )

  # Based on what nodes are start or end nodes, determine the node shape
  shape <- mapply(function(start, end) {
    if (start & end) {
      "star"
    } else if (start) {
      "triangle"
    } else if (end) "square" else "dot"
  }, startbool, endbool)

  if ("state1_id" %notin% colnames(graphdf)) {
    nodes <- data.table(
      id = node_names, label = node_names, size = 40,
      start = startbool, end = endbool, shape = shape,
      color.background = "lightblue", color.border = "lightblue",
      shadow = FALSE
    )

    edges <- data.table(
      id = edge_ids,
      label = create_label(graphdf, columns = label_columns, collapse = label_collapse), to = graphdf$state2, from = graphdf$state1,
      type = graphdf$type, parameter = graphdf$parameter,
      penalty = graphdf$penalty, K = as.character(graphdf$K), a = graphdf$a,
      min = as.character(NAtoNone(graphdf$min)), max = as.character(NAtoNone(graphdf$max)),
      selfReference.angle = selfReference.angle, selfReference.size = 40, hidden = hidden,
      color = "black"
    )
  } else {
    nodes <- data.table(
      id = unique(c(graphdf$state1_id, graphdf$state2_id)), label = node_names, size = 40,
      start = startbool, end = endbool, shape = shape,
      color.background = "lightblue", color.border = "lightblue",
      shadow = FALSE
    )

    edges <- data.table(
      id = edge_ids,
      label = create_label(graphdf, columns = label_columns, collapse = label_collapse), to = graphdf$state2_id, from = graphdf$state1_id,
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
#' @importFrom dplyr %>%
#' @import visNetwork
#' @importFrom gfpop gfpop
#' @examples
#' visNetwork_to_graphdf(graphdf_to_visNetwork(gfpop::graph(type = "std")))
#' @export
visNetwork_to_graphdf <- function(visNetwork_list) {
  # CMD Check compatibility section
  label <- NULL
  # End CMD compatibility section

  if (nrow(visNetwork_list$edges) == 0) {
    return(data.table())
  }

  edges <- visNetwork_list$edges
  nodes <- visNetwork_list$nodes

  # Note: graphdf "state1" and "state2" are based on node labels, not on ids.
  # But visNetwork "to" and "from" are based on ids. So, need to map.

  # For each edge, find the node label associated with state1
  state1 <- unlist(
    apply(
      edges, 1,
      function(x) {
        nodes[id == x[["from"]], "label"]
      }
    ),
    use.names = F
  )
  state1_id <- edges$from

  # For each edge, find the node label associated with state 2
  state2 <- unlist(
    apply(
      edges, 1,
      function(x) {
        nodes[id == x[["to"]], "label"]
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

  data.table(
    state1 = state1, state1_id = state1_id, state2 = state2, type = type,
    state2_id = state2_id, parameter = parameter, penalty = penalty,
    K = K, a = a, min = min, max = max
  )
}

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
    onRender(visNetwork_attachjs)
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
  # CMD Check compatibility section
  `:=` <- NULL
  . <- NULL
  label <- NULL

  # End CMD compatibility section
  graphdata_visNetwork_return <- graphdata_visNetwork
  if (!is.null(event$type)) {
    event$type <- tolower(event$type)
    if (!(event$type %in% c("null", "std", "up", "down", "abs"))) {
      warning("Invalid 'type' parameter, returning unchanged data.")
      return(graphdata_visNetwork)
    }
  }
  ### Edit Edge --------------------------------------------------------------
  if (event$cmd == "editEdge") {
    changed_id <- event$id

    angle <- if (event$type == "null") pi else 2 * pi

    edges <- data.table(
      graphdata_visNetwork_return$edges
    )

    edges[
      id == changed_id,
      c(
        "label", "type", "parameter", "penalty", "K", "a", "min", "max",
        "selfReference.angle", "selfReference.size"
      ) :=
        .(
          "", event$type, event$parameter, event$penalty, event$K,
          event$a, event$min, event$max, angle, 40
        )
    ]

    graphdata_visNetwork_return$edges <- edges
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
    edges <- data.table(graphdata_visNetwork_return$edges)
    for (del_edge in event$edges) {
      graphdata_visNetwork_return$edges <- edges[id != del_edge]
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
    nodes <- data.table(graphdata_visNetwork_return$nodes)
    nodes[id == event$id, label := event$label]
    graphdata_visNetwork_return$nodes <- nodes
  }

  ### Delete Node ------------------------------------------------------------
  if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
    nodes <- data.table(graphdata_visNetwork_return$nodes)
    for (del_node in event$nodes) {
      graphdata_visNetwork_return$nodes <- nodes[id != del_node]
    }
  }

  graphdata_visNetwork_return
}

#' Allows to update old visNetwork edges from a new gfpop graphdf,
#' while only updating certain columns
#' @param old_edges A dataframe in visNetwork edges format
#' @param graphdf A graph object (in the form of a dataframe) from gfpop
#' @param edgeSep A character seperating the nodes in an edge label
#' @param showNull (Boolean) hide null edges?
#' @param label_columns (character) An array of columns to use to make edge labels
#' @param columns_to_exclude (character) An array of columns not to include in the update
#' @param label_collapse the character separating edge columns in this label
#' @export
update_visNetwork_edges <- function(old_edges, graphdf, edgeSep = "_", showNull = TRUE,
                                    label_columns = c("type", "penalty"),
                                    label_collapse = " | ",
                                    columns_to_exclude = c("id", "to", "from")) {
  return_edges <- old_edges
  new_edges <- graphdf_to_visNetwork(graphdf, edgeSep, showNull, label_columns, label_collapse)$edges
  for (column in colnames(old_edges)) {
    if (column %notin% columns_to_exclude) {
      return_edges[[column]] <- new_edges[[column]]
    }
  }

  return_edges
}
