---
id: may11_may17
title: 11 May 2020 - 17 May 2020
---


Goals:

* (Carry-over from last week): app layout sketch, itemized checklist of features we want in the app.
* Read the gfpop paper and package documentation in more detail, and make sure to write down questions to ask Guillem and Toby in meeting on Friday.
* Think about setting up coding environment: make notes about how you are going to do that, review those with Guillem and Toby on Friday, and then implement them on Friday/Saturday.

## App Layout

TODO: sketch

### Components

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
