---
id: june15_june21
title: 15 June 2020 - 21 June 2020
---

## Goals

While last week was a feature-heavy week, this one is more about stepping back, cleaning up, and testing.

My goals are to:

* Abstract and clean up code
* Implement some more testing

## Quick Reflection

This week felt slower than last, but I learned a lot. First, the new `testServer` functionality in Shiny is really helpful--I'll talk about that more below. Second, I have read more carefully about the `gfpop` package itself, and can work next week to put what I learned into the application.

## testServer

The new `testServer` functionality relies on a particular module structure that is implemented in the current Shiny version, 1.4, and will be recommended after 1.5 is released.

### Background: new module structure

In the old module structure, server modules were written as follows:

```R
# In the module.R file
example_module_server <- function(input, output, session) {
    # Server code here
}
```

```R
# In the server.R file
app_server <- function(input, output, session) {
    callModule(example_module_server, "unique-id")
}
```

In the new module structure, server modules are written a bit differently: 

```R
# In the module.R file
example_module_server <- function(id) {
    moduleServer(
        id,
        function(input, output, session) {
    # Server code here
    })
}
```

```R
# In the server.R file
app_server <- function(input, output, session) {
    example_module_server("unique-id")
}
```

This new format is helpful because the `callModule` part of the old format is built-in to the module function--so, essentially, you have a **single, self-contained** server function, rather than a function that can only be called through `callModule`.

`golem` still recommends/uses the old module format, so I submitted an [issue on their repo](https://github.com/ThinkR-open/golem/issues/455) to remind them to update their recommendations once Shiny 1.5 is released.

### Testing Overview

The new `testModule` functionality relies on the new module structure, since it expects a single function to contain the whole server module.

It's really great because it allows you to access all of the inputs inside of your reactive shiny objects. For example,

```R
library(shiny)
example_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      myvals <- reactiveValues(example = "Hello, world!")
    }
  )
}

testServer(example_module_server, {
  print(myvals$example)
})
```

```R
[1] "Hello, world!"
```

It also allows you to set inputs:

```R
library(shiny)
example_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      my_func <- reactive({
        print(input$my_input)
      })
    }
  )
}

testServer(example_module_server, {
  session$setInputs(my_input = "This is my input!")
  my_func()
})
```

### Problems Testing

One problem I've had is that I can't use `testServer` to trigger click events directly:

This testServer code should print "Button was pressed", but it doesn't.]

```R
library(shiny)
example_module_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      eventReactive(input$my_button, {
        print("Button was pressed")
      })
    }
  )
}

testServer(example_module_server, {
  # my_button should already be NULL, but for good measure:
  session$setInputs(my_button = NULL)
  # This should be what happens when the button is pressed
  session$setInputs(my_button = 0)
})
```

Has no output. So, I commented on an [existing shiny issue](https://github.com/rstudio/shiny/issues/2745) to hopefully have that fixed.

## Further features

That's it for this week, but more information about this week's progress is located in [the issues tab of the gfpopgui repo](https://github.com/julianstanley/gfpopgui/issues).