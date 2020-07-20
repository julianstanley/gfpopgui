---
id: july13_july19
title: 13 July 2020 - 19 July 2020
---

## Goals

* Fix visNetwork refresh problem
* Allow users to use start/end nodes
* Fix bugs, build on existing features

## Fixing the visNetwork graph refresh problem

This was deceptively easy. I just needed to use the `visNetworkProxy` functionality. I had tried to do this before, but thought I was on the wrong path because of some small code bugs.

In short, if anyone is reading this and trying to resolve a similar problem, just look at the visNetwork example shiny application:

```r
shiny::runApp(system.file("shiny", package = "visNetwork"))
```

Essentially, I previously re-generated the visNetwork plot at certain times. Now, instead of re-generating the visNetwork plot, I just call:

```r
visNetworkProxy(ns("gfpopGraph")) %>%
    visUpdateNodes(nodes = gfpop_data$graphdata_visNetwork$nodes) %>%
    visUpdateEdges(edges = gfpop_data$graphdata_visNetwork$edges)
```

And visNetwork will update the graph without moving the nodes.

## Setting starting/ending nodes

Users need to be able to pick _one_  starting node, and one ending node.

They also need to be able to not pick a node at all. By default, all nodes should be able to be starting or ending nodes.

So, I created dropdown boxes with all current nodes, plus the string "N/A" that users can choose between to set the starting and ending nodes. For example:

```r
output$uiSetStart <- renderUI({
selectInput(ns("setStart"), "Select a starting node",
    choices = c(
    "N/A",
    gfpop_data$graphdata_visNetwork$nodes$label
    ),
    selected = startEnd$start
)
})
```

Notice that I set `selected = startEnd$start`. What was that about? The problem is that, each time the graphdata updates, the `setStart` selectInput will be refreshed. But, we want, for example, the starting node dropdown should have the current start node selected. So, I had to create a reactive value that holds the _current_ starting node, and then have that value be the one that is selected in the dropdown.

Then, I use the value from the dropdown to set the starting node in the gfpop data:

```r
observeEvent(input$setStartEnd_button, {

    # new_val: the new start or new end
    # val_type: "start" or "end"
    set_startEnd <- function(new_val, val_type) {
        if (new_val != "N/A") {
        gfpop_data$graphdata <<- gfpop::graph(
            gfpop_data$graphdata %>%
            rbind.fill(data.frame(state1 = new_val, type = val_type))
        )
        } else {
        gfpop_data$graphdata <<- gfpop::graph(
            data.frame(gfpop_data$graphdata) %>%
            filter(type != val_type)
        )
        }
    }

    set_startEnd(input$setStart, "start")
    set_startEnd(input$setEnd, "end")

    # Set these so that the "start" and "end" dropdown boxes, which are
    # refreshed when graphdata updates, knows about the current start & end
    startEnd$start <- input$setStart
    startEnd$end <- input$setEnd

    # Update the visNetwork data to match the gfpop data
    gfpop_data$graphdata_visNetwork <- graphdf_to_visNetwork(
        gfpop_data$graphdata
    )
})
```

So, modifying the 'start' node means adding a row to the gfpop dataframe. Once I do that, I need to (1) set the reactiveValues that indicate which node is start/end, and (2) update the visNetwork data to be consistent with the gfpop dataframe.

## Common error: over-isolating inputs

Lots of errors have popped up while developing this application. One common error is that I tend to overuse the `shiny::isolate()` function.

The `isolate()` function removes data from shiny's reactive structure. So, for example, in this function:

```R
output$myOut <- renderPlot(x <- 1:isolate(input$xmax))
```

The `myOut` plot will only update if I update it manually--unlike it's counterpart without `isolate()`, which would update each time `input$xmax` is updated.

This can be really handy for when outputs are computationally intensive to calculate. For example, I have the user press a "run gfpop" button whenever they want to run gfpop. Without `isolate()`, the gfpop results would update each time an input was changed. And, if a user were changing lots of inputs, that might bog down the app unnecessarily.

But I have to be careful about using `isolate()`. For example, when building the function that generates some data for gfpop, I initally used it in such a way that isolate was necessary, and it looked a bit like this:

```R
primary_input <- data.frame(
    X = 1:isolate(input$ndata)
    Y = dataGenerator(isolate(input$ndata),
    c(0.1, 0.3, 0.5, 0.8, 1), c(1, 2, 1, 3, 1), sigma = isolate(input$sigma))
    )

gfpop_data$main_data <- primary_input
```

I initally meant for this to be very temporary but, increasingly, I think a version of this generate data function needs to be in the final app for demonstration purposes.

So, when I put this into an observeEvent() call, it no longer needed the isolate calls, but I left them there anyways:

```R
observeEvent(input$genData, {
primary_input <- data.frame(
    X = 1:isolate(input$ndata)
    Y = dataGenerator(isolate(input$ndata),
    c(0.1, 0.3, 0.5, 0.8, 1), c(1, 2, 1, 3, 1), sigma = isolate(input$sigma))
    )

gfpop_data$main_data <- primary_input
})
```

But, the `isolate()` calls here will prevent this expression from being run more than once! So, when I noticed that I couldn't generate data more than once this week, I had to remove the isolate calls:

```R
observeEvent(input$genData, {
primary_input <- data.frame(
    X = 1:input$ndata
    Y = dataGenerator(input$ndata,
    c(0.1, 0.3, 0.5, 0.8, 1), c(1, 2, 1, 3, 1), sigma = input$sigma)
    )

gfpop_data$main_data <- primary_input
})
```
