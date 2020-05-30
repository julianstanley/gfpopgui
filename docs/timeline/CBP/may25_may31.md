---
id: may25_may31
title: 25 May 2020 - 31 May 2020
---

This is the last week in the community bonding period! From now on, these timeline posts will be more like a reflective blog than a day-by-day update. Those updates [have now moved to the issues tab of the repo](https://github.com/julianstanley/gfpop-gui/issues).

So, I'll write these posts at the end of the week and highlight some of the things that I learned.

## Goals

* Hone in on whether plotly or visNetwork are best for making the constraint graph visualization.
* Make a shell application
* Start on testing

## Quick Reflection

Although this is a community bonding period week, I felt that I needed to get started coding to get a feel for what additional things I needed to learn from the community.

As a result, I think this week was very productive: I built a semi-functional shiny application and feel that I have a very clear direction for this project.

## `golem` and Shiny project structure

`golem` is a fantastic package, and [the documentation](https://thinkr-open.github.io/golem/articles/a_start.html) by itself was basically all I needed to get started.

It's been useful to me mostly as just a guideline for `shiny` best practices. For example, it encouraged me to use `usethat` functions to setup things like `covr`, which was much faster and less problematic than setting it up on my own.

It also convinced me to use a proper package structure for my shiny app. So far, that has been great to keep track of dependencies and use build tools.

There were a few things that I struggled with this week that I should share:

### Organizing tabs

When working on an application with multiple tabs, I don't think the typical module structure is the best way to organize the tabs.

Instead, just put the ui and server components of each tab in a separate file and folder--for example, the ui for the home tab goes in `R/ui/tab_home.R`. Then, in your main `app_ui.R` file, just call:

```R
source(file.path("R", "ui", "tab_home.R"), local = TRUE)$value
```

to source the file. Nothing fancy, just keeping different tabs in different files.

### Sourcing seperate HTML files

My home tab has a big block of HTML that I wanted to move to a separate file.

`golem` creates a shortcut from `/inst/app/www` to just `www`, so I sourced my HTML file like:

```R
includeHTML("www/lorem.html")
```

That worked! But, one of the `golem` recommended tests failed.

That's because sourcing the file like that was visible from the web browser, but not to the R interpreter.

Instead, I needed to use `system.file` to source the file:

```R
includeHTML(system.file("app/www/lorem.html", package = "gfpopgui"))
```

## Travis CI, and automated shinyapp.io builds

This week, I setup my package to build/test through TravisCI, check through `covr`, and auto-deploy to shinyapps.io.

### cache:packages and r_github_packages:

This is worth its own header. In my last R project, I didn't have the `cache:packages` option set. However, caching packages speeds up the build _so, so much_.

e.g.:

```YAML
language: R
cache: packages
```

I also learned about the `r_github_packages` option, which lets you install an R package from GitHub on the Travis node, e.g.:

```YAML
r_github_packages:
  - julianstanley/gfpop-gui
```

However, my auto-deploy to shinyapps.io had some weird problems.

## visNetwork works!

For the past few weeks, I've been trying to figure out how to make a proper, interactive, editable plot for the constraint graph.

This week, thanks to some updates on the visNetwork package, I have that!

It's still not perfect, and there's a small bug in the ability to edit edges, but it's good enough for now.

The new features is the ability to add custom edit attributes to graphs--and to be able to edit edge attributes at all.

So, for example, let's say I wanted users to be able to edit a nodeParam in nodes and an edgeParam in edges, I could add the `editNodeCols` and `editEdgeCols` parameters in `visOptions` like this:

```R
visNetwork(nodes = nodes, edges = edges) %>%
  visOptions(manipulation = list(
    enabled = TRUE,
    editEdgeCols = c("label", "to", "from", "edgeParam"),
    editNodeCols = c("label", "nodeParam")
  ))
```

But, even though this feature allows users to edit nodes, it doesn't provide any straightforward functionality to update the node and edge data after the user edits it in a shiny app. That's where the `input$[graph_name]_graphChange` element comes in!

If my graph is named `input$mygraph`, then user edit events to that graph will show up in `input$mygraph_graphChange`! `input$mygraph_graphChange` will be a list, where the `cmd` parameter of that list specifies the user event, and other parameters specify changes to the node/edge.

So, to respond to a user event editing an edge, I can use `observeEvent` from shiny as follows:

```R
observeEvent(input$mygraph_graphChange, {
  event <- input$mygraph_graphChange
  cmd <- event$cmd

  if (cmd == "editEdge") {
    mygraph_edges <- mygraph_edges %>%
      mutate_cond(id == event$id,
        to = event$to, from = event$from,
        label = event$label) # etc with other parameters
  }
})
```

Where `mutate_cond` is a nice fancy custom modification on `dplyr::mutate` that edits rows based on a parameter, instead of columns. You can define it like this:

```R
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}
```

So, thanks to these features, our editable graph is now feasible! I'll need to read up a bit more on visNetworkProxy to make the graph updates more efficient, and work on making the graph prettier and more user-friendly (and implement it in the first place)!

While I haven't put the graph into production yet, I wrote a quick [proof-of-concept gist](https://gist.github.com/julianstanley/74321b6b75584e9e17cd68666b2177c2), if you want to take a look!

## Issues, branches, and pull requests

### Issues

Finally, this week has really increased my appreciation for using issues, branches, and pull requests. In fact, I think I've been using them a lot more than most users (hopefully not too much!)

Just this week, I opened 12 issues in gfpop-gui, as well as [one in gfpop](https://github.com/vrunge/gfpop/issues/11), [one in shinytest](https://github.com/rstudio/shinytest/issues/312), and one in [visNetwork](https://github.com/datastorm-open/visNetwork/issues/377), and also [commented on some in golem](https://github.com/ThinkR-open/golem/issues/263).

I think that creating issues for my own repository has been a fantastic way to keep track of my progress and thoughts, which has helped me be more focused and productive. And creating issues in other repositories, as well as participating in conversations in the [RStudio community forums](https://community.rstudio.com) has really helped me to connect with the open-source community, and think more critically about how to ask good questions and contribute thoughtfully.

Finally, just seeing public comments on issues tabs and forums has had a big impact on the way that I code and think. There are so many good suggestions and helpful tidbits out there in the public!

### Branches and Pull Requests

I think I had previously underestimated the utility of creating branches and pull requesting on repositories where I am the primary contributor.

While branches _are_ most helpful for projects with a group of simultaneous contributors, I have found them really helpful for myself this week.

In previous projects, I often found myself panicking when I made build-breaking changes to the master branch. Then, in that panic, I would revert some commits or have a long string of commits as I tried to fix the repo.

With branches that build independently, I can work through problems like that in an isolated environment, and then delete changes and/or rebase before merging that branch into master, being confident that I'm pushing polished changes.

## Testing

I'm still working on testing. But, in short, `shinytest` seems awesome for most testing purposes. However, it fails when you need your tests to include client-side interactions, like panning/zooming in a plotly plot. For those interactions, RSelenium seems like the best option. I'm going to work more on getting RSelenium set up, and hopefully will write more about that in the timeline blog next week!
