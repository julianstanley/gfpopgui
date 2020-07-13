---
id: june29_july03
title: 29 June 2020 - 03 July 2020
---

## Goals

* Clean up basic app functionality for the end of Phase 1.

## Adding 'Save' and 'Load' buttons

In `gfpop`, users should be able to save and load their analyses, complete with their graphical constraint and main `gfpop` parameters.

Implementing this at a basic level was fairly straightforward: when a user presses "save", all inputs that are necessary to reproduce the analysis should be saved in a `reactiveValues()` list, with some user-supplied identifier.

Then, users can choose from saved identifiers to re-load saved analyses.

Practically, it looks something like this:

#### Initalize reactive values list

```r
saved_analyses <- reactiveValues(
    saved_full = list(),
    saved_descriptions = data.table()
)
```

The idea here is that this data structure allows for more complex saving implemtations as well, if we end up wanting that. `saved_full` saves _all_ data associated with a given save point, whereas `saved_descriptions` is a table that describes each entry in `saved_full`. 

In this implementation, `saved_descriptions` just has identifiers, but we could also add more identifying information if that ends up being useful.

#### Add button to save

Here, I need to use `reactiveValuesToList` because, if I literally saved gfpop_data in its reactiveValue form, it would continue to update as inputs change. But I want to save a snapshot, so I save it as a list.

```r
# In UI
h2("Save"),
textInput(
    inputId = ns("saveId"),
    label = "Unique Save Name"
),
actionButton(
    inputId = ns("saveButton"),
    label = "Save Analysis"
)

# In Server
observeEvent(input$saveButton, 
    req(input$saveId)
    saveId <- input$saveId

    # Make sure the save id is unique!
    if (saveId %in% names(saved_analyses$saved_full)) {
        shinyalert(paste0(
        "Error: '", saveId,
        "' already exists.\nIDs must be unique."
        ))

    # The saveId should be the key for the saved_full list.
    # And, for now (as mentioned in text above), `saved_descriptions`
    # is just the id, but could be built up later if necessary
    } else {
        saved_analyses$saved_full[[saveId]] <- reactiveValuesToList(gfpop_data)
        saved_analyses$saved_descriptions <- rbind(
        saved_analyses$saved_descriptions,
        data.table(id = input$saveId)
        )
    }

    # Clear the text input after saving
    updateTextInput(session, "saveId", value = "")
    })
```

#### Add UI element with a list of saved environments

```r
# In UI
h2("Load"),
uiOutput(ns("uiLoadId"))

# In Server
output$uiLoadId <- renderUI({
selectInput(ns("loadId"), "Select a previous analysis",
    choices = saved_analyses$saved_descriptions$id
)
})
```

#### Add a loading button

This part was a little tricky. First, to load the saved analyses, I need to coerce them into a reactive object, since I used `reactiveValuesToList` when saving. Then, I need to use a `<<-` to assign `gfpop_data` globally, not just within the observeEvent. Then, I use `do.call` to make each component of the saved list reactive.

In addition, overwriting `gfpop_data` does not update the graph (since there's a manual button to update that graph). So, I use `updateNumericInput` to manually refresh the graph.

```r
# In UI
actionButton(
        inputId = ns("loadButton"),
        label = "Load Analysis"
    )

# In Server
observeEvent(input$loadButton, {
req(input$loadId)
gfpop_data <<- do.call("reactiveValues", saved_analyses$saved_full[[input$loadId]])

updateNumericInput(
    session = session, inputId = "graph_refresh_helper",
    value = input$graph_refresh_helper + 1
)
})
```

Now, all together, users can load and save data!

## Downloading and loading .Rdata

This is simple, but was tricky to figure out how to do in the first place.

I wanted users to be able to download and upload .Rdata files with complete analyses.

To upload:

```r
# input$completed_analysis is defined above, that's the input 
# where users can upload their .Rdata file
rdata_name <- load(input$completed_analysis$datapath)

# Use mget to load rdata_name into a variable
gfpop_data_list <- mget(rdata_name, environment())

# The .Rdata contains a list, so go over each element in that list
# and add it to `gfpop_data`
# Since gfpop_data is already defined as a reactiveValues object, no
# need to coerse list items into reactive values: shiny takes care of that.
lapply(names(gfpop_data_list[[1]]), 
           function(x) gfpop_data[[x]] <- gfpop_data_list[[1]][[x]])
  })
```

To download:

```r
# Just save gfpop_data as a list
output$downloadData <- downloadHandler(
    filename = function() "gfpopgui_data.Rdata",
    content = function(file) {
        gfpop_data_list <- reactiveValuesToList(gfpop_data, all.names=T)
        save(gfpop_data_list, file = file)
    }
)
```

## Converting a graph dataframe to R code

I still need to work on this more. For now, I just take each column in a gfpop graph dataframe and make it into an argument in the `gfpop::Edge()` function.

This method works for most simple graphs, but doesn't consider things like "Node" columns, etc.

So, to format an individual row in a graph dataframe:

```r
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
```

And then, to wrap each row together:

```r
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
```
