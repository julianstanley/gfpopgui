---
id: july20_july26
title: 20 July 2020 - 26 July 2020
---

## Goals

* Implement crosstalk between visNetwork and plotly visualizations
* Finish up smaller issues that have come up over the weeks

## Cross-talk from plotly to visNetwork

When the user hovers over a changepoint segment in the plotly visualization, the associated node should be highlighted in the visNetwork (constraint graph) visualization.

If a plotly visualization includes a `key` attribute (e.g. `plot_ly(key = ~info)`), then that attribute is passed through the browser when the user hovers over a datapoint with that attribute.

That information can be observed through shiny via the `event_data()` plotly function.

For example, in my case, I observed `event_data("plotly_hover", "gfpopPlot")`, which returns a data table.

The return value from that observation is a table, and, when I gave each changeregion a `key` attribute with the id of of the changeregion's state, that id is present in the "key" column of the data table, in the second row.

So, under certain conditions, I used `visNetworkProxy` to change the color of nodes that have the same id as the `key` of the selected change region.

For more information, see [the associated commit](https://github.com/julianstanley/gfpopgui/commit/f1cc6204c5d03c8aa3d69c33377bf18fc50a4786).

## Cross-talk from visNetwork to plotly

This was a little more difficult to me. The big problem here is that each changeregion is _not_ a separate plotly trace, so I can't change their colors individually.

I can make each changeregion a separate trace, but that makes the application a lot slower for many changeregions.

So, instead, I decided to "highlight" a changeregion by just drawing a _new_ trace over the location of the associated changeregion.

This is definitely not fast, but I think it may be fast _enough_ for our purposes. 

To accomplish this, first I observe the `input$gfpopGraph_highlight_color_id` input. This comes from visNetwork (`gfpopGraph` is the id of the visNetwork visualization) and changes each time a node in the visNetwork plot is highlighted (which can happen on hover or on click). For now, I set this to on-click.

So, from that variable I know the ID of the node being highlighted. Then, I subset the changepoint information (on which the plotly graph was built) to only contain changeregions associated with the highlighted ID. Then, I use `plotlyProxy()` and `plotlyProxyInvoke("addTraces")` to make a new plotly trace. That trace can be later deleted with the `plotlyProxyInvoke("deleteTraces")` command.

For example:

```R
highlighted_id <- input$gfpopGraph_highlight_color_id
segments_to_highlight <- gfpop_data$changepoints %>% filter(state == highlighted_id)

plotlyProxy("gfpopPlot", session) %>%
    plotlyProxyInvoke(
        "addTraces",
        list(
            x = segments_to_highlight$x,
            y = segments_to_highlight$y,
            text = segments_to_highlight$text,
            line = list(color = "red", width = 10)
        )
    )
```

For more information, see the [associated commit](https://github.com/julianstanley/gfpopgui/commit/fe75aad7c0123c6b2bae9298adaf12aefab0a5f3).


