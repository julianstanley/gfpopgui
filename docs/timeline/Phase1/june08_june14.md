---
id: june08_june14
title: 08 June 2020 - 14 June 2020
---

## Goals

This week is a feature development-heavy week. I would like to:

* Create a working, interactive graph
* Improve the plotly visualization (namely: tooltip should only work on the segments/changepoints and give useful info)

## Quick Reflection

I'm writing this on June 15th (I'm late this week!), but last week went very well.

June 01-07 was difficult for me because it was mostly getting testing initially setup and debugged, which was frustrating because it felt like I wasn't making significant progress.

This week, in contrast, felt like I was making a lot of progress because I was directly working on new app features. This short timeline post will cover a couple things that I learned while making two of those features.

## Overlaying segment markers on a plotly visualization

The [2020 gfpop paper](https://arxiv.org/abs/2002.03646) provided me two different examples of what a changepoint plot should look like:

![changeplot plot 1](assets/june08_june14/changepoints_1.png)
![changeplot plot 2](assets/june08_june14/changepoints_2.png)

So, it was clear that I needed to have a base plot of the main data, and overlain bars to indicate the changepoints.

The main data was easy enough. The user provides that data in `gfpop_data$main_data`, so I can just plot the X and Y columns of that data.

Importantly, I want to disable tooltips with `hoverinfo = 'none'` so that they don't interfere when the user tries to hover over a changepoint bar:

```R
base_plot <- plot_ly(gfpop_data$main_data, x = ~X, y = ~Y, hoverinfo = 'none')
```

Then comes the harder part. I have changepoint data (from running `gfpop::gfpop()` on `gfpop_data$main_data` and the user-provided graph), and I need to overlay that data on the plot.

Originally, I overlayed that data using the `geom_segment` layer, or the plotly equivalent `add_segments()`. That works well, but has a big downside: segments cannot have their own tooltips. I should post an issue on the plotly GitHub page to make sure that's the case.

So, instead, I need to create many points along where the changepoint regions are supposed to go, and then connect them with a line, since a line can have a tooltip. I also will need a separate trace for segments (e.g. spans along the X axis without changepoints) and for the changepoints themselves, since they need to be two different colors.

I ended up writing a function that I am not super proud of called `add_changepoints`. It loops through each of the changepoints returned by `gfpop::gfpop` and creates a dataframe that includes many points and a description for each changepoint, adds that to an accumulator dataframe, and then uses that accumulator dataframe to draw lines. This just seems unnecessarily resource intensive and verbose for what I want to do, but here's the code:

```R
add_changepoints <- function(plotly_obj, original_data, changepoint_data) {
  # Initialize plotly object to return
  return_plotly <- plotly_obj %>%
    hide_legend()
  
  changepoint_annotations_regions = data.frame(x = c(), y = c(), text = c())
  changepoint_annotations = data.frame(x = c(), y = c(), text = c())

  changepoints <- changepoint_data$changepoints

  # Note: ds = dataspace, since changepoint data refers to indicates, not in dataspace
  previous_changepoint <- 1
  previous_changepoint_ds <- original_data$X[1]
  i <- 1
  # Add each changepoint to the given plotly object
  for (i in 1:length(changepoints)) {
    changepoint <- changepoints[i]
    changepoint_ds <- original_data$X[changepoint]

    # The region preceeding a changepoint, or between two changepoints
    changeregion <- seq(previous_changepoint, changepoint)
    changeregion_ds <- seq(previous_changepoint_ds,
      changepoint_ds,
      length.out = length(changeregion)
    )

    changepoint_annotations_regions <- rbind(
      changepoint_annotations_regions,
      data.frame(x = c(changeregion_ds, NA),
                 y = c(rep(changepoint_data$parameters[i], length(changeregion_ds)), NA),
                 text = c(rep(
                   paste0(
                   "State: ", changepoint_data$states[i], "\n",
                   "Region mean: ", round(changepoint_data$parameters[i], 2), "\n",
                   "Next changepoint: ", round(changepoint_ds, 2)
                 ),
                 length(changeregion_ds)), NA)
      )
    )
    # If this isn't the first region, connect this region with the last
    if (i > 1) {
      changepoint_annotations <- rbind(
        changepoint_annotations,
        data.frame(
          x = c(rep(previous_changepoint_ds, 50), NA), 
          y = c(seq(changepoint_data$parameters[i - 1],
                                  changepoint_data$parameters[i],
                                  length.out = 50
          ), NA),
          text = c(rep(paste0("Changepoint #", i-1, ": ", round(previous_changepoint_ds, 2)), 50),
                   NA)
        )
      )
    }

    # Update the previous changepoints
    previous_changepoint <- changepoint
    previous_changepoint_ds <- changepoint_ds
  }

  return_plotly %>%
    add_lines(data = changepoint_annotations_regions,
              x = ~x,
              y = ~y, 
              color = ~I("#40B0A6"),
              hoverinfo = "text", text = ~text,
              connectgaps = F,
              line = list(width = 7)) %>%
    add_lines(data = changepoint_annotations,
              x = ~x,
              y = ~y, 
              color = ~I("#E1BE6A"),
              hoverinfo = "text", text = ~text,
              connectgaps = F,
              line = list(width = 7)) %>%
    layout(hovermode = "x unified")
}
```

And, in the app, it generates plots like this:

![changeplot plot 3](assets/june08_june14/changepoints_3.png)

For the time being, this accomplishes what I need it to do--but I should come back to this later in the project.

In the meantime, I [posted this on RStudio community](https://community.rstudio.com/t/tooltip-for-geom-segment-add-segments-in-plotly/69910).

## Monitoring user changes in visNetwork

When a user edits a visNetwork plot, the data underlying the plot remains unchanged.

So, in the case of gfpop-gui, where I want to take user's visNetwork graph edits and use the resulting graph to estimate changepoints, I need additional code to watch for graph edits and edit the underlying graph data.

visNetwork provides that functionality by passing user edits in an object contained in `input${graph name}_graphChange`. So, in the case of gfpop-gui, the graph name is `gfpopGraph`, so the input to observe is `input$gfpopGraph_graphChange`.

The `input$gfpopGraph_graphChange` object has a `cmd` entry that specifies the type of change. Then, in response to those different graphChange commands, I can edit the data:

```R
event <- input$gfpopGraph_graphChange
if (event$cmd == "editEdge") {
    # What happens when the user edits an edge?
    # In this case, I used `mutate_cond`
}

### Add Edge ---------------------------------------------------------------
if (event$cmd == "addEdge") {
    # Add edge response
}

### Delete Edge ------------------------------------------------------------
if (event$cmd == "deleteElements" && (length(event$edges) > 0)) {
    # When the user deletes elements, the resulting event has $edges and $nodes 
    # that are changed
}

### Add Node ---------------------------------------------------------------
if (event$cmd == "addNode") {
    # Add node response
}

### Edit Node --------------------------------------------------------------
if (event$cmd == "editNode") {
    # Edit node response
}

### Delete Node ------------------------------------------------------------
if (event$cmd == "deleteElements" && (length(event$nodes) > 0)) {
    # Delete node response
}
```