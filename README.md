# gfpop GUI
  <!-- badges: start -->
  [![Documentation via Docusaurus](https://img.shields.io/badge/Documentation%20and%20Timeline-Docusaurus-blue)](https://julianstanley.github.io/gfpop-gui)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
   [![Travis build status](https://travis-ci.com/julianstanley/gfpop-gui.svg?branch=master)](https://travis-ci.com/julianstanley/gfpop-gui)
   [![Codecov test coverage](https://codecov.io/gh/julianstanley/gfpop-gui/branch/master/graph/badge.svg)](https://codecov.io/gh/julianstanley/gfpop-gui?branch=master)
   [![Sauce Test Status](https://saucelabs.com/buildstatus/julianstanley?dummy=unused)](https://app.saucelabs.com/u/julianstanley)
  <!-- badges: end -->

## Overview

This is a graphical user interface (GUI) for the [gfpop](https://github.com/vrunge/gfpop) package.

Generally, `gfpop` is an R package implementing an algorithm that lets users detect changepoints
in their data. Unlike related packages (e.g. `changepoint`), `gfpop` works under the assumption
that its users have some general idea about the patterns of their changepoints. For example,
you might expect that your data alternates between "up" and "down" changepoints, each with 
a mean change of at least 1. If you can represent your assumptions about your changepoints
as a directed graph, `gfpop` can take that constraint graph as an input and very efficiently predict
changepoints in your data. 

This GUI for gfpop lets you draw a constraint graph and edit its parameters interactively. Then, it will
run the `gfpop` algorithm and let you interactively see changepoint differences.

This is an in-progress summer project. See the project description at [Google Summer of Code 2020](https://summerofcode.withgoogle.com/projects/#6502959753461760).

## Installation and Use

To install:
```R
devtools::install_github("julianstanley/gfpopgui")
```

To run, you can either [visit the web application](https://julianstanley.shinyapps.io/gfpopgui/), or run:

```R
gfpopgui::run_app()
```

See the application for more detailed usage information. [Documentation](https://julianstanley.github.io/gfpop-gui) is still in progress.

## Attributions

Project supported by the R Project for Statistical Computing            |  With funding provided by Google Summer of Code
:-------------------------:|:-------------------------:
<img src="inst/img/rlogo.png" alt="alt text" height="200px"> |  <img src="inst/img/gsoc-icon.png" alt="alt text" height="200px">

Open Source testing license generously provided by SauceLabs.com
:-------------------------:
![Testing Powered By SauceLabs](https://saucelabs.github.io/images/opensauce/powered-by-saucelabs-badge-gray.svg?sanitize=true "Testing Powered By SauceLabs")

