---
id: july04_july12
title: 04 July 2020 - 12 July 2020
---

## Goals

* Move more functionality to client-side
* Continue moving forward with testing

## Using custom JavaScript in ShinyApps

You can run custom JavaScript in a ShinyApp. This is fairly straightforward and there are some great resources available to learn more, such as [ThinkR's JS4Shiny Field Notes](http://connect.thinkr.fr/js4shinyfieldnotes) and the slides from the [2020 RStudio conf JS For Shiny Workshop](https://rstudio-conf-2020.github.io/js-for-shiny/).

### The Problem

Things get a little more complicated with htmlwidgets, the platform on which `visNetwork` is built.

JS code supplied to shiny runs when the DOM is first rendered. However, visNetwork is widget that doesn't appear in the DOM until _after_ the Shiny server renders it. Because of that, I couldn't figure out how to add custom JS to the visNetwork widget.

### Progress Towards Solution

I posted related questions on [RStudio Community](https://community.rstudio.com/t/wait-for-an-object-to-be-rendered-before-adding-a-js-event-listener/72246) and on [StackOverflow](https://stackoverflow.com/questions/62794827/r-shiny-run-js-after-htmlwidget-renders/62796035#62796035).

Stéphane Laurent gave a great answer where they introduced me to the `htmlwidgets::onRender()` function that is essentially designed for just this purpose: to add new javascript onto an existing HTMLwidget object.

So, I just add `%>% onRender(additional_js)` to the visNetwork call, and then I can put custom javascript in an `additional_js` string.

I would like to learn how to move `additional_js` to a seperate file. This is straightforward when using a JS file in Shiny generally, but I'm not sure how to do that with `onRender()`.

`onRender()` may help me implement a variety of features. In the meantime, I can use it to validate the entries that a user passes when editing an edge:

```javascript
additional_js <-"function(el, x) {
    // Validate edge type when the save button is pressed
    $('#editedge-saveButton').on('click', function() {
        let type = $('#editedge-type').val();
        if (!['null', 'std', 'up', 'down', 'abs'].includes(type.toLowerCase())) {
            alert(`${type} is not a valid type. Defaulting to null`);
            $('#editedge-type').val('null');
        }
    })
}
"
```

## Follow-up on the SauceLabs local testing issue

After some back-and-forth with the SauceLabs folks, they figured out why I was [having this problem that I described in more detail a few weeks ago](https://julianstanley.github.io/gfpopgui/docs/timeline/Phase1/june22_june28#local-testing-with-saucelabs). Basically, it was because of the way that they proxy localhost requests. Their proxy makes it easier for people to connect on certain ports but, in this case, was blocking that WebSocket handshake.

So it ended up being an easy fix: I just added 127.0.0.1 julian.local to my /etc/hosts and then pointed Selenium to julian.local:3000 instead of localhost:3000. Bam, problem gone.

The problem I experienced seems to happen with every Shiny app, since they all rely on that handshake, but there's no info about that anywhere. Later this week, I'm going to work on a blog post explaining using SauceLabs and Selenium with Shiny, so I'll include more information about this there.
