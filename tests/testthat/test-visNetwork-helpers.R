# NAtoNone
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

# NonetoNA
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

# graphdf_to_visNetwork
test_that("graphdf_to_visNetwork generate a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(typeof(visNetwork_list), "list")
})

test_that("graphdf_to_visNetwork has 'nodes' and 'edges' as labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(labels(visNetwork_list), c("nodes", "edges"))
})

# visNetwork_to_graphdf
test_that("visNetwork_to_graphdf is inverse of graphdf_to_visNetwork", {
  expect_equivalent(visNetwork_to_graphdf(
    graphdf_to_visNetwork(gfpop::graph(type = "std")) 
  ) %>% select_graph_columns(), gfpop::graph(type = "std"))
})

# generate_visNetwork
test_that("visNetwork function generates a list", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  expect_equal(typeof(generate_visNetwork(visNetwork_list)), "list")
})

test_that("visNetwork function generates a list with certain labels", {
  visNetwork_list <- graphdf_to_visNetwork(gfpop::graph(type = "std"))
  associated_visNetwork <- generate_visNetwork(visNetwork_list)

  expect_equal(
    labels(associated_visNetwork),
    c(
      "x", "width", "height", "sizingPolicy", "dependencies",
      "elementId", "preRenderHook", "jsHooks"
    )
  )
})

# Editing a visNetwork

# Standard data produced by:
# dput(gfpopgui::graphdf_to_visNetwork(gfpop::graph(type = "std", penalty = 15)))
data <- list(
  nodes = structure(list(id = "Std", label = "Std", size = 40, start = FALSE, 
                         end = FALSE, shape = "dot", color.background = "lightblue",
                         color.border = "lightblue", shadow = FALSE), 
  class = "data.frame", row.names = c(NA,-1L)), 
  edges = structure(list(id = c("Std_Std_null", "Std_Std_std"), 
  label = c("null | 0", "std | 15"), to = c("Std", "Std"), 
  from = c("Std","Std"), type = c("null", "std"), parameter = c(1, 0), 
  penalty = c(0,15), K = c("Inf", "Inf"), a = c(0, 0), min = c("None", "None"), 
  max = c("None", "None"), selfReference.angle = c(3.14159265358979,6.28318530717959), 
  selfReference.size = c(40, 40), hidden = c(FALSE,FALSE), color = c("black", "black")),
  class = "data.frame", row.names = c(NA, -2L)))


test_that(
  "can edit an edge, scenario 1",
  {
    event <- list(
      cmd = "editEdge",
      to = "Std", from = "Std",
      id = "Std_Std_std", type = "std", penalty = "200", parameter = "0",
      K = "Inf", a = "0", min = "None", max = "None", hidden = "FALSE"
    )
    new_data <- modify_visNetwork(event, data)
    expect_equal(new_data$edges$penalty, c(0, 200))
  }
)

test_that("Editing an edge turns the edge type lowercase", {
  event <- list(
    cmd = "editEdge",
    to = "Std", from = "Std",
    id = "Std_Std_std", type = "Std", penalty = "200", parameter = "0",
    K = "Inf", a = "0", min = "None", max = "None", hidden = "FALSE"
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

test_that(
  "can edit a node", {
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
  "can edit a node without affecting others", {
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
  event1 <-  list(
    cmd = "addNode",
    id = "new_node", label = "new node"
  )
  new_data1 <- modify_visNetwork(event1, data)
  expect_equal(new_data1$nodes$id, c("Std", "new_node"))

  event2 <- list(
    cmd = "deleteElements", nodes = c("new_node"))
  new_data2 <- modify_visNetwork(event2, new_data1)
  
  expect_equal(new_data2$nodes$id, c("Std"))
  expect_equal(new_data2$edges$penalty, c(0, 15))
})

test_that("Can delete edge without deleting node", {
  event <- list(
    cmd = "deleteElements", nodes = c(),
    edges = c("Std_Std_null"))

  new_data <- modify_visNetwork(event, data)
  
  expect_equal(new_data$edges$penalty, c(15))
})

test_that("Create label works with data.tables", {
  expect_equal(
    create_label(data.table(gfpop::graph(type = "updown"))),
    c("null | 0", "null | 0","up | 0", "down | 0")
  )
})

test_that("Create label works the same with graphdfs and visNetwork lists", {
  expect_equal(
    create_label(data.table(gfpop::graph(type = "updown"))),
    create_label(graphdf_to_visNetwork(data.table(gfpop::graph(type = "updown")))$edges)
  )
})
