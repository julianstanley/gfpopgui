<h1 align = "center">
  <br>
  <img src="https://github.com/julianstanley/gfpopgui/raw/master/docs/assets/readme_assets/gfpopgui_logo.png" alt="gfpopgui" width="300">
  <br>
  gfpop gui
  <br>
</h1>

<h4 align="center">An interactive shiny-based application for exploring graph-
constrained changepoint detection with the <a href="https://github.com/vrunge/gfpop" target="_blank">gfpop</a> package.</h4>


<div align="center">
  <!-- badges: start -->
  <div id="banner" style="overflow: hidden;justify-content:space-around;">
  <a href="https://julianstanley.github.io/gfpopgui">
  <img src="https://img.shields.io/badge/Documentation%20and%20Timeline-Docusaurus-blue" 
    alt="Documentation vis Docusarus"></img>
  </a>
  <a href="https://www.tidyverse.org/lifecycle/#experimental">
  <img src="https://img.shields.io/badge/lifecycle-experimental-orange.svg" 
    alt="Lifecycle: experimental"></img>
  </a>
  <a href="https://travis-ci.com/julianstanley/gfpopgui">
  <img src="https://travis-ci.com/julianstanley/gfpopgui.svg?branch=master" 
    alt="Travis build status"></img>
  </a>
  <a href="https://codecov.io/gh/julianstanley/gfpopgui?branch=master">
  <img src="https://codecov.io/gh/julianstanley/gfpopgui/branch/master/graph/badge.svg" 
    alt="Codecov test coverage"></img>
  </a>
  <a href="https://app.saucelabs.com/u/julianstanley">
  <img src="https://saucelabs.com/buildstatus/julianstanley?dummy=unused" 
    alt="Sauce Test Status"></img>
  </a>
  </div>
  <hr>
  <!-- badges: end -->
   
  <p>
    <a href="#overview">Overview</a> •
    <a href="#installation">Installation</a> •
    <a href="#testing">Testing</a> •
    <a href="#attributions">Attributions</a>
  </p>
</div>
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

## Installation

To install:
```R
devtools::install_github("julianstanley/gfpopgui")
```

To run, you can either [visit the web application](https://julianstanley.shinyapps.io/gfpopgui/), or run:

```R
gfpopgui::run_app()
```

See the application for more detailed usage information. [Documentation](https://julianstanley.github.io/gfpopgui) is still in progress.

## Testing

Uses a combination of testing frameworks. `testthat` and `shiny::testServer` are used for most functionality and run via CI.

Other functionality is tested on the most recent version of Chrome on OSX Catalina and Windows 10:

![](https://saucelabs.com/browser-matrix/julianstanley.svg)

## Attributions

Project supported by the R Project for Statistical Computing            |  With funding provided by Google Summer of Code
:-------------------------:|:-------------------------:
<img src="inst/img/rlogo.png" alt="alt text" height="200px"> |  <img src="inst/img/gsoc-icon.png" alt="alt text" height="200px">

Open Source testing license generously provided by SauceLabs.com
:-------------------------:
![Testing Powered By SauceLabs](https://saucelabs.github.io/images/opensauce/powered-by-saucelabs-badge-gray.svg?sanitize=true "Testing Powered By SauceLabs")

