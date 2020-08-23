library(data.table)
library(gfpop)

# NAtoNone ---------------------------------------------------------------------
test_that("NAtoNone doesn't affect an empty vector", {
  expect_equal(NAtoNone(c()), list())
})

test_that("NAtoNone converts NAs to the string 'None'", {
  expect_equal(NAtoNone(rep(NA, 3)), as.list(rep("None", 3)))
})

test_that("NAtoNone keeps non-NA values constant", {
  expect_equal(NAtoNone(c(NA, 3, NA)), list("None", 3, "None"))
})

test_that("NAtoNone doesn't affect 'None' strings", {
  expect_equal(NAtoNone(list(NA, 3, NA, "None")), list("None", 3, "None", "None"))
})

# NonetoNA ---------------------------------------------------------------------
test_that("NonetoNA doesn't affect an empty vector", {
  expect_equal(NonetoNA(c()), list())
})

test_that("NonetoNA converts the string 'None' to NA", {
  expect_equal(NonetoNA(rep("None", 3)), as.list(rep(NA, 3)))
})

test_that("NonetoNA keeps non-None values constant", {
  expect_equal(NonetoNA(c("None", "test", "None")), list(NA, "test", NA))
})

test_that("NonetoNA doesn't affect existing NA", {
  expect_equal(NonetoNA(c(NA, "test", NA, "None")), list(NA, "test", NA, NA))
})

test_that("NonetoNA is the inverse of NAtoNone", {
  result <- NonetoNA(NAtoNone(c(1, 2, 4, 2, NA, 2, 4, NA)))
  expect_equal(NonetoNA(NAtoNone(c(1, 2, 4, 2, NA, 2, 4, NA))), list(1, 2, 4, 2, NA, 2, 4, NA))
})

# create_label -----------------------------------------------------------------
test_that("Create label works with data.tables", {
  expect_equal(
    create_label(gfpop::graph(type = "updown")),
    c("null | 0", "null | 0", "up | 0", "down | 0")
  )
})

test_that("Create label works the same with graphdfs and visNetwork lists", {
  expect_equal(
    create_label(gfpop::graph(type = "updown")),
    create_label(graphdf_to_visNetwork(data.table(gfpop::graph(type = "updown")),
      edge_ids = c("std1", "std2")
    )$edges)
  )
})

test_that("Create label works when just one column is specified", {
  expect_equal(
    create_label(gfpop::graph(type = "updown"), columns = c("type")),
    c("null", "null", "up", "down")
  )
})

test_that("Create label works when all columns are specified", {
  graph <- gfpop::graph(type = "std")
  expect_equal(
    create_label(graph, columns = colnames(graph)),
    c(
      "Std | Std | null | 1 | 0 | Inf | 0 | NA | NA",
      "Std | Std | std | 0 | 0 | Inf | 0 | NA | NA"
    )
  )
})

test_that("Can also specify 'to' and 'from', even with a gfpop::graph", {
  graph <- gfpop::graph(type = "std")
  expect_equal(
    create_label(graph, columns = c(
      "to", "from", "type", "parameter",
      "penalty", "K", "a", "min", "max"
    )),
    c(
      "Std | Std | null | 1 | 0 | Inf | 0 | NA | NA",
      "Std | Std | std | 0 | 0 | Inf | 0 | NA | NA"
    )
  )
})

# create_label_individual ------------------------------------------------------
test_that("create_label_individual works as expected", {
  expect_equal(
    create_label_individual("from", "to", "type", 10, 100, 1e5, 1e6, 1e7, 1e8),
    "type | 100"
  )
})

test_that("create label can make label with all columns", {
  expect_equal(
    create_label_individual("from", "to", "type", 10, 100, 1e5, 1e6, 1e7, 1e8,
      columns = colnames(gfpop::graph(type = "std"))
    ),
    "from | to | type | 10 | 100 | 1e+05 | 1e+06 | 1e+07 | 1e+08"
  )
})

test_that("create label fails if one of the requested parameters doesn't exist", {
  expect_error(
    create_label_individual("from", "to", "type",
      columns = colnames(gfpop::graph(type = "std"))
    )
  )
})

test_that("create label can work with a different collapse arg", {
  expect_equal(
    create_label_individual("from", "to", "type", 10, 100, 1e5, 1e6, 1e7, 1e8,
      columns = colnames(gfpop::graph(type = "std")),
      collapse = ","
    ),
    "from,to,type,10,100,1e+05,1e+06,1e+07,1e+08"
  )
})

# add_node ---------------------------------------------------------------------
test_that("Add_node can add a node to an empty data table", {
  expect_equal(
    add_node(data.table(), "id"),
    data.table(
      id = "id", label = "", size = 40, start = FALSE,
      end = FALSE, shape = "dot", color.background = "lightblue",
      color.border = "lightblue", shadow = FALSE
    )
  )
})

test_that("Add_node can add a node to an existing node data table", {
  nodes <- graphdf_to_visNetwork(data.table(gfpop::graph(type = "std")),
    edge_ids = c("std1", "std2")
  )$nodes
  expect_equal(
    add_node(nodes, "id"),
    rbindlist(list(
      nodes,
      data.table(
        id = "id", label = "", size = 40, start = FALSE,
        end = FALSE, shape = "dot", color.background = "lightblue",
        color.border = "lightblue", shadow = FALSE
      )
    ))
  )
})

# add_edge ---------------------------------------------------------------------
test_that("add_edge can add an edge to an empty data table", {
  expect_equal(
    add_edge(data.table(), "id", "", "Std", "Std", "null", 1, 100),
    data.table(
      id = "id", label = "", to = "Std", from = "Std",
      type = "null", parameter = 1, penalty = 100, K = Inf, a = 0,
      min = NA, max = NA, selfReference.angle = NA,
      selfReference.size = 40, hidden = FALSE, color = "black"
    )
  )
})

test_that("add_edge can add an edge to an existing graph data table", {
  edges <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )$edges
  expect_equal(
    add_edge(edges, "id", "", "Std", "Std", "null", 1, 100),
    rbindlist(list(
      edges,
      data.table(
        id = "id", label = "", to = "Std", from = "Std",
        type = "null", parameter = 1, penalty = 100, K = Inf, a = 0,
        min = NA, max = NA, selfReference.angle = NA,
        selfReference.size = 40, hidden = FALSE, color = "black"
      )
    ))
  )
})

# add_null_edge ----------------------------------------------------------------
test_that("add_null_edge works", {
  edges <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )$edges
  expect_equal(
    add_null_edge(edges, "random"),
    rbindlist(list(
      edges,
      data.table(
        id = "random_random_null", label = "null | 0",
        to = "random", from = "random",
        type = "null", parameter = 1, penalty = 0, K = Inf, a = 0,
        min = NA, max = NA, selfReference.angle = pi,
        selfReference.size = 40, hidden = FALSE, color = "black"
      )
    ))
  )
})


# graphdf_to_visNetwork --------------------------------------------------------
test_that("graphdf_to_visNetwork can handle empty data", {
  expect_silent(graphdf_to_visNetwork(gfpop::graph()))
})

test_that("graphdf_to_visNetwork generate a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )
  expect_equal(typeof(visNetwork_list), "list")
})

test_that("graphdf_to_visNetwork has 'nodes' and 'edges' as labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )
  expect_equal(labels(visNetwork_list), c("nodes", "edges"))
})

test_that("graphdf_to_visNetwork is the inverse of visNetwork_to_graphdf", {
  expect_equal(
    data.table(gfpop::graph(type = "std")),
    visNetwork_to_graphdf(graphdf_to_visNetwork(gfpop::graph(type = "std"),
      edge_ids = c("std1", "std2")
    )) %>%
      select_graph_columns()
  )
})

test_that("graphdf_to_visNetwork can deal with starting nodes", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(start = 0)
  )

  expect_equal(
    graphdf_to_visNetwork(graph,
      edge_ids = 1:5
    )$nodes$shape,
    c("triangle", "dot", "dot")
  )
})

test_that("graphdf_to_visNetwork's startEnd also works in the middle of df", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(start = 1)
  )

  expect_equal(
    graphdf_to_visNetwork(graph,
      edge_ids = 1:5
    )$nodes$shape,
    c("dot", "triangle", "dot")
  )
})


test_that("graphdf_to_visNetwork can deal with ending nodes", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(end = 0)
  )

  expect_equal(
    graphdf_to_visNetwork(graph,
      edge_ids = 1:5
    )$nodes$shape,
    c("square", "dot", "dot")
  )
})

test_that("graphdf_to_visNetwork can deal with different start and end nodes", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(end = 0, start = 1)
  )

  expect_equal(
    graphdf_to_visNetwork(graph,
      edge_ids = 1:5
    )$nodes$shape,
    c("square", "triangle", "dot")
  )
})

test_that("graphdf_to_visNetwork can deal with same start and end nodes", {
  graph <- gfpop::graph(
    Edge(0, 1, "up", 0),
    Edge(1, 2, "up", 0),
    Edge(0, 0, "null"),
    Edge(1, 1, "null"),
    Edge(2, 2, "null"),
    StartEnd(end = 0, start = 0)
  )

  expect_equal(
    graphdf_to_visNetwork(graph,
      edge_ids = 1:5
    )$nodes$shape,
    c("star", "dot", "dot")
  )
})


# visNetwork_to_graphdf --------------------------------------------------------
test_that("visNetwork_to_graphdf generates an empty data table from an empty list", {
  expect_equal(
    visNetwork_to_graphdf(list(nodes = data.frame(), edges = data.frame())),
    data.table()
  )
})

test_that("visNetwork_to_graphdf accurately labels edges based on node labels", {
  initial_list <- graphdf_to_visNetwork(gfpop::graph(type = "updown"))
  nodes <- initial_list$nodes
  edges <- initial_list$edges

  nodes$label <- c("LabelOne", "LabelTwo")

  expect_equal(
    visNetwork_to_graphdf(list(nodes = nodes, edges = edges))[, c("state1", "state2")],
    data.table(
      state1 = c("LabelOne", "LabelTwo", "LabelOne", "LabelTwo"),
      state2 = c("LabelOne", "LabelTwo", "LabelTwo", "LabelOne")
    )
  )

  # Should still keep track of original node ids
  expect_equal(
    visNetwork_to_graphdf(list(nodes = nodes, edges = edges))[, c(
      "state1_id",
      "state2_id"
    )],
    data.table(
      state1_id = c("Dw", "Up", "Dw", "Up"),
      state2_id = c("Dw", "Up", "Up", "Dw")
    )
  )
})

test_that("visNetwork_to_graphdf converts 'None' to 'NA' with min and max", {
  expect_equal(
    visNetwork_to_graphdf(
      graphdf_to_visNetwork(gfpop::graph(type = "updown"))
    )[, c("min", "max")],
    data.table(min = rep(as.numeric(NA), 4), max = rep(as.numeric(NA), 4))
  )
})


# generate_visNetwork ----------------------------------------------------------
test_that("visNetwork function generates a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )
  expect_equal(typeof(generate_visNetwork(visNetwork_list)), "list")
})

test_that("visNetwork function generates a list with certain labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"),
    edge_ids = c("std1", "std2")
  )
  associated_visNetwork <- generate_visNetwork(visNetwork_list)

  expect_equal(
    labels(associated_visNetwork),
    c(
      "x", "width", "height", "sizingPolicy", "dependencies",
      "elementId", "preRenderHook", "jsHooks"
    )
  )
})

# modify_visNetwork -----------------------------------------------------------
data <- graphdf_to_visNetwork(gfpop::graph(type = "std", penalty = 15),
  edge_ids = c(
    "Std_Std_null",
    "Std_Std_std"
  )
)

test_that("modify_visNetwork appropriately responds to invalid edge types", {
  event <- list(
    cmd = "editEdge",
    to = "Std", from = "Std",
    id = "Std_Std_std", type = "incorrect", penalty = 200, parameter = 0,
    K = Inf, a = 0, min = "None", max = "None", hidden = "FALSE"
  )

  expect_warning(
    new_data <- modify_visNetwork(event, data),
    "Invalid 'type' parameter, returning unchanged data."
  )

  # Penalty should remain unchanged
  expect_equal(new_data$edges$penalty, c(0, 15))
})

test_that(
  "can edit a basic edge penalty",
  {
    event <- list(
      cmd = "editEdge",
      to = "Std", from = "Std",
      id = "Std_Std_std", type = "std", penalty = 200, parameter = 0,
      K = Inf, a = 0, min = "None", max = "None", hidden = "FALSE"
    )
    new_data <- modify_visNetwork(event, data)
    expect_equal(new_data$edges$penalty, c(0, 200))
  }
)

test_that("type parameter is case-insensitive", {
  event <- list(
    cmd = "editEdge",
    to = "Std", from = "Std",
    id = "Std_Std_std", type = "STD", penalty = 200, parameter = 0,
    K = Inf, a = 0, min = "None", max = "None", hidden = "FALSE"
  )

  new_data <- modify_visNetwork(event, data)
  expect_equal(new_data$edges$penalty, c(0, 200))
})

test_that("Editing an edge turns the edge type lowercase", {
  event <- list(
    cmd = "editEdge",
    to = "Std", from = "Std",
    id = "Std_Std_std", type = "Std", penalty = 200, parameter = 0,
    K = Inf, a = 0, min = "None", max = "None", hidden = "FALSE"
  )
  new_data <- modify_visNetwork(event, data)
  expect_equal(new_data$edges$type, c("null", "std"))
})

test_that(
  "can add node",
  {
    event <- list(
      cmd = "addNode",
      id = "new_node", label = "new node"
    )
    new_data <- modify_visNetwork(event, data)
    nodes_test <- new_data$nodes

    expect_equal(nodes_test$id, c("Std", "new_node"))
    expect_equal(nodes_test$label, c("Std", "new node"))
    expect_equal(nodes_test$size, c(40, 40))
  }
)

test_that("Adding a node automatically adds a recursive edge, if addNull", {
  event <- list(
    cmd = "addNode",
    id = "new_node", label = "new node"
  )
  new_data <- modify_visNetwork(event, data, addNull = TRUE)
  edges_test <- new_data$edges

  expect_equal(edges_test$id, c(
    "Std_Std_null", "Std_Std_std",
    "new_node_new_node_null"
  ))

  expect_equal(
    edges_test[3, ],
    data.table(
      id = "new_node_new_node_null",
      label = "null | 0", to = "new_node",
      from = "new_node", type = "null", parameter = 1,
      penalty = 0, K = "Inf", a = 0, min = as.character(NA),
      max = as.character(NA), selfReference.angle = pi,
      selfReference.size = 40, hidden = FALSE, color = "black"
    )
  )
})

test_that(
  "can edit a node",
  {
    event <- list(
      cmd = "editNode",
      id = "Std", label = "my edited node"
    )
    new_data <- modify_visNetwork(event, data)
    nodes_test <- new_data$nodes

    expect_equal(nodes_test$id, c("Std"))
    expect_equal(nodes_test$label, c("my edited node"))
    expect_equal(nodes_test$size, c(40))
  }
)

test_that(
  "can edit a node without affecting others",
  {
    event1 <- list(
      cmd = "addNode",
      id = "new_node", label = "new node"
    )

    new_data1 <- modify_visNetwork(event1, data)

    event2 <- list(
      cmd = "editNode",
      id = "new_node", label = "my edited node"
    )

    new_data2 <- modify_visNetwork(event2, new_data1)
    nodes_test <- new_data2$nodes

    expect_equal(nodes_test$id, c("Std", "new_node"))
    expect_equal(nodes_test$label, c("Std", "my edited node"))
    expect_equal(nodes_test$size, c(40, 40))
  }
)

test_that("Can add an edge", {
  event <- list(
    cmd = "addEdge", id = "new_std_null",
    to = "new_node", from = "Std"
  )

  new_data <- modify_visNetwork(event, data)

  expect_equal(new_data$edges$penalty, as.character(c(0, 15, 10)))
})

test_that("Can delete node without deleting edge", {
  event1 <- list(
    cmd = "addNode",
    id = "new_node", label = "new node"
  )
  new_data1 <- modify_visNetwork(event1, data)
  expect_equal(new_data1$nodes$id, c("Std", "new_node"))

  event2 <- list(
    cmd = "deleteElements", nodes = c("new_node")
  )
  new_data2 <- modify_visNetwork(event2, new_data1)

  expect_equal(new_data2$nodes$id, c("Std"))
  expect_equal(new_data2$edges$penalty, c(0, 15))
})

test_that("Can delete edge without deleting node", {
  event <- list(
    cmd = "deleteElements", nodes = c(),
    edges = c("Std_Std_null")
  )

  new_data <- modify_visNetwork(event, data)

  expect_equal(new_data$edges$penalty, c(15))
})

# update_visNetwork_edges ------------------------------------------------------
test_that("update_visNetwork_edges appropriately updates", {
  gfpop_edges <- data.table(gfpop::graph(type = "updown"))
  visNetwork_edges <- graphdf_to_visNetwork(gfpop_edges)$edges
  
  gfpop_edges[, parameter := 1:4]
  
  expect_equal(
    update_visNetwork_edges(visNetwork_edges, gfpop_edges)$parameter,
    1:4)
})

test_that("update_visNetwork_edges appropriately excludes specified columns", {
  gfpop_edges <- data.table(gfpop::graph(type = "updown"))
  visNetwork_edges <- graphdf_to_visNetwork(gfpop_edges)$edges
  
  gfpop_edges[, parameter := 1:4]
  
  expect_equal(
    update_visNetwork_edges(visNetwork_edges, gfpop_edges, 
                            columns_to_exclude = c("parameter"))$parameter,
    c(1, 1, 0, 0))
})

test_that("update_visNetwork_edges appropriately labels specified columns", {
  gfpop_edges <- data.table(gfpop::graph(type = "updown"))
  visNetwork_edges <- graphdf_to_visNetwork(gfpop_edges)$edges
  
  gfpop_edges[, parameter := 1:4]
  
  expect_equal(
    update_visNetwork_edges(visNetwork_edges, gfpop_edges, 
                            label_columns = c("to", "parameter"))$label,
    paste0(gfpop_edges$state2, " | ", gfpop_edges$parameter)
  )
})