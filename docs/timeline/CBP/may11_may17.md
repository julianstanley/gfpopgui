---
id: may11_may17
title: 11 May 2020 - 17 May 2020
---


## Goals

* (Carry-over from last week): app layout sketch, itemized checklist of features we want in the app.
* Read the gfpop paper and package documentation in more detail, and make sure to write down questions to ask Guillem and Toby in meeting on Friday.
* Think about setting up coding environment: make notes about how you are going to do that, review those with Guillem and Toby on Friday, and then implement them on Friday/Saturday.

## App Layout

### Sketches

#### Welcome Tab

![alt-text](assets/may11_may17/layout_1.JPG)

#### Analysis Tab

![alt-text](assets/may11_may17/layout_2.JPG)

#### Sharing Tab

![alt-text](assets/may11_may17/layout_3.JPG)

#### Help Tab

![alt-text](assets/may11_may17/layout_4.JPG)

### Components List

1. Welcome page
    * Title and subtitle
    * Overview and instructions
    * Data upload area
        1. Upload option: .Rdata file with completed analysis
        2. Upload option: .csv file with input data
        3. Upload option: .csv file with directed graph
2. Analysis page
    * Main analysis plot (one large D3 plot)
        1. User data plot
            * Overlain: changepoint predictions
        2. Directed graph constraint
    * Post-hoc analysis
        * One large D3 plot, just with user data plot
            * Option to draw where the user expects to see changepoints
        * Quality metrics associated with those expectations (how close to expected are the predicted changepoints?)
3. Sharing/download page
    * Export all results (.Rdata)
    * Export input data and graph (.zip of .csv)
    * Export graph data (.csv)

## Coding environment notes

* Developing on this GitHub repository, just working off of the `master` branch.

* Use [renv](https://rstudio.github.io/renv/) for dependency management/reproducibility OR, for even more, maybe use [golem](https://thinkr-open.github.io/golem/).

* Extra feature: could use [shinymeta](https://github.com/rstudio/shinymeta) to produce PDF/Rmd files based on current Shiny session, which is handy. Like a "download report".

## Questions/Notes about `gfpop`

* Node names need to be unique, right? So, only one node may be named `Up`, etc.

* In Figure 11: (Side note: typo in legend: you say `in absolute vale` instead of `in absolute value`) You set a constraint that the absolute value of each change must be 1, but the absolute value of the means in the blue model around ~8 (pos in chr2) seems to be a lot smaller--closer to 0.5. What am I missing?

* Can you force a certain number of changepoints? (in the copy number example, you say that you set a beta such that they get 13 segments--did you have to do that iteratively?)

## Log/notes (meta)

05/10:

* Sketched out basic components.

05/11:

* Drew out the "home" page, but noticed that just sketching might not be the idea way to plan out the app. Just the components list does that well enough. Would be nice to go ahead and build a skeleton as an example UI. Had some trouble with that at first, so reviewing the mastering-shiny book.

* First, sitting down and giving the gfpop arxiv paper a read-through.

05/12:

* Maybe the graph/data plots don't need to be written in D3 entirely from scratch? It looks like plotly may have some event handling, and I may be able to leverage that: [stackoverflow answer](https://stackoverflow.com/a/47407363/8290926).

* Reviewed the gfpop paper yesterday, need to read through that again and ask questions.

* Also finished reading and doing all exercises for "mastering shiny" chapters 2 and 3, plus Chapter 4.1-4.3. Keeping track of progress in a [personal repo](https://github.com/julianstanley/mastering-shiny-solutions).

05/13:

* Mostly focused on improving Shiny skills. Attended a Shiny workshop hosted by the Harvard Bioinformatics Core 1PM-4PM, but it was a bit too introductory. Finished reading and completing exerciese for "mastering shiny" chapters 4, 5, 6, and 13.

* I'm going to need some more tangible outputs before Friday's meeting. Tomorrow, I need to wrap up my main goals for the week. I have a couple basic sketches of the UI: I should finish those up and load them. I also need to re-read the gfpop paper one more time and write down questions. I've been thinking about the environment, so I should write down a few notes on that and also read more (Chapters 15 and 16) to make sure I have a plan for using best practices for modules and testing.

* Another note: take a closer look at the [Tabler](tabler.io) package.

05/14:

* Off to a good start, finished up initial UI sketches. Also added some coding environment nodes.

* Note: The idea of using plotly instead of base D3 seems to be gaining more traction in my mind. You can customize tooltips with plotly, and add custom event handlers. Carson did a fantastic job documenting the package [here](https://plotly-r.com/). I bet I can do the main interactive changepoint visualization with it. But, the graph-making visualization may still need to be base D3--meaning those graphs would need to be seperated.

* I added some questions about the `gfpop` paper and package, but I think I still need a bit more time to read the manuscript through a few more times and get a better feel for the aspects of the work that will be practical in designing a good GUI.
