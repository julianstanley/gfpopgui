---
id: june01_june07
title: 01 June 2020 - 07 June 2020
---

## Goals

* Set up an RSelenium testing framework
* Further modularize code

## Quick Reflection

This week was mostly about debugging testing. Although I did not have as much visible output of my work this week, I still think it was productive overall. I think that moving forward with this project will be much easier now that I've established a strong base of support.

## In Shiny development, modules are important and useful

Last week, I talked about using `source(file.path("R", "ui", "tab_home.R"), local = TRUE)$value`, etc. to seperate the different tabs of my application.

This week, that decision came back to bite me. The problem is that, in many shiny applications, all components of the application are stored in `inst/`, so all of those files are preserved when the package is built and distributed.

However, when building an app with `golem` (like I am), shiny logic is kept in the `R/` directory, which is modified when a package is built and distributed. 

So, all that to say that the `source` approach was making my application break at weird times.

I should have built the different parts of my application as shiny modules in the first place. The [modules section in the mastering-shiny book](https://mastering-shiny.org/scaling-modules.html) was very useful in learning more about how to make modules, and the reading was well worth the effort.

## RSelenium is finicky, but potentially worth-it

Here's a guide to setting up RSelenium with Shiny:

1. Install the RSelenium R package: `install.packages("RSelenium")
2. Install Docker (on Ubuntu: `sudo apt install docker`)
3. Run a docker Selenium instance, making sure to set `--net=host`: docker run -d --net=host selenium/standalone-firefox&
4. Open a Selenium instance from R:

```r
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444, browser = "firefox") 
remDr$open(silent = TRUE)
```

At this point, if you navigate to `http://localhost:4444/wd/hub/static/resource/hub.html` in a browser, you should see a helpful Selenium interface.

5. Run an instance of your shiny app in the background on a known port:

```r
system("${R_HOME}/bin/Rscript -e 'library(gfpopgui);options(shiny.port = 15123);run_app()' &", 
       ignore.stdout = TRUE,
       ignore.stderr = TRUE)
```

6. Use the `remDr` object to open the shiny app in a headless Selenium browser: `remDr$navigate(url = "http://127.0.0.1:15123")`.
7. Use the `remDr` object to interact with the app, or get information from it (e.g.: `appTitle <- remDr$getTitle()[[1]]`). You can now use this information in tests.

I personally put sets 4-7 within a testthat script. So, that script just requires that a Selenium instance is running. That means that my tests can run locally and also on Travis by including:

```yaml
services:
  - docker

before_install:
  - docker pull selenium/standalone-firefox
  - docker run -d --net=host -p 127.0.0.1:4444:4444 selenium/standalone-firefox
```

in my `.travis.yml`.

The only caveat is that running the shiny app in the background often takes a few seconds, so I include a `Sys.sleep(10)` statement before running `remDr$navigate` to give the app time to reload.

We'll see in the coming weeks if the effort to get this debugged was worth it!

## Use DT::renderDataTable, not shiny::renderDataTable

While modularizing my code this week, I noticed that I had some errors through Travis that I could not reproduce locally. If you're reading this and are interested, [here's a link to my community.rstudio post about the problem](https://community.rstudio.com/t/x-must-be-a-vector-travis-but-not-local/68717).

Ultimately, I was able to fix the problem by upgrading my R version, which was a few months out of date.

When I updated, I got errors in my `shiny::renderDataTable` calls. Evidently [others were also having the same problems in newer R versions](https://community.rstudio.com/t/shiny-1-3-2-renderdatatable-cant-work-now/41935/2) and the developers are moving from `shiny::renderDataTable` to `DT::renderDataTable`, and switching those functions (and having to tweak a bit to get them to work) fixed things up.

But, this episode convinced me that I needed to be more explicit about where my functions come from. So, I've completely removed `@import` statements in my ROxygen documentation comments and replaced them with `@importFrom` statements. This way, I am being explicit about the packages from which each of my function calls come.
