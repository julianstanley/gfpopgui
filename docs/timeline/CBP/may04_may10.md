---
id: may04_may10
title: 04 May 2020 - 10 May 2020
---

Goals:

* Research existing applications with similar features to what we want with the gfpop-gui.
* Sketch the layout of the gfpop-gui in more detail. This should include an itemized checklist of desired features.

## Existing Applications

### shinyDAG

[GitHub link](https://github.com/GerkeLab/shinyDAG)

A web application that uses R and LaTeX to create publication-quality images of directed acyclic graphs (DAGs).

They allow users to create directed graphs, but I think the interface is a little bit clunky. They preview graphs with Plotly, but I don't think all of the Plolty features are necessary.

However, I really like how they add nodes: you click "Add New Node" and then you enter properties of the node (in this case, just the name) into a text box. I may want to do something similar with the gfpop-gui.

They represent nodes as a list with a name, x cord, and y cord. Seems to identify edges by the IDs of the nodes which they connect, so breaks when nodes have the same name.

Conclusion: Great reference for an interface for creating nodes and interacting between javascript and R.

### D3 Dynamic Report

[GitHub link](https://github.com/jienagu/D3_folded_charts)

Just an example of integrating D3 and Shiny. Besides that, not useful in this context, I don't think.

### Interactive tool for creating directed graphs using d3.js

[bl.cks link](https://bl.ocks.org/cjrd/6863459)

This block is central to and will probably be the basis of the gfpop-gui application.

It describes making a editable directed graph with D3 (just javascript, no Shiny).

### rstudio::conf shiny app

[Shinyapp.io link](https://gadenbuie.shinyapps.io/tweet-conf-dash/)

This app isn't remotely related to gfpop-gui, but I really like the layout. It uses the same basic shiny dashboard that people use all the time, but it just feels very nice and clean. It would be great to emulate parts of the design.

### thinkr TidyTuesday 2019-10-15

[Application link](https://connect.thinkr.fr/tidytuesday201942/)

Or maybe this sort of layout would be nicer? It's definitely simplier, and maybe more intuititive?

### More?

I'm really surprised that I'm having this much trouble finding more applications that are similar to what we would like to do. It's exciting--since it means that we're doing something somewhat unique--but also a bit scary.

I'll keep casually looking for more model applications, and make a note to ask about this in my first meeting with Guillem and Toby next week.

## Layout

I was off to a slow start this week, so I'm going to move the layout goal to early next week (starting tomorrow!).

## Future Directions

I want to make sure that my base knowledge of Shiny is up to par. I've never been super satisifed with the available books, but Hadley Wickham is writing one now! It's not nearly done yet, but I'll run through what's already written. [Link](https://mastering-shiny.org/).
