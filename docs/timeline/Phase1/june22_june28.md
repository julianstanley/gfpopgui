---
id: june22_june28
title: 22 June 2020 - 28 June 2020
---

## Goals

This week is mostly about testing

My goals are to:

* Better understand Selenium/SauceLabs testing
* Finish off little feature issues

## Quick Reflection

I felt much less productive this week than in previous weeks. A lot of the Selenium/web testing concepts are difficult and I got stuck/unmotivated at multiple points throughout the week. Still, I made some progress. I'll plan to write a more extensive blog post about setting up RSelenium/SauceLabs towards the end of this internship period, so these are mostly notes to refresh my memory with that time comes.

## Using XPath in Selenium

Especially in a ShinyApp, many elements in the DOM can be difficult to find. For example: how do I access elements in a table from DataTables?

To make it easier to find elements in the DOM, Selenium allows elements to be specified by an "XML path", or XPath.

XPath is super helpful and, luckily, RSelenium can take XPath parameters as arguments.

For more information on XPath, [this tutorial is really helpful](https://www.guru99.com/xpath-selenium.html).

But, in practice, I can generate the XPath I need using [Katalon Recorder Chrome extension](https://chrome.google.com/webstore/detail/katalon-recorder-selenium/ljdobmomdgdljniojadhoplhkpialdid?hl=en-US).

Katalon can recognize most of the elements in gfpopgui and give me the associated id or, if a simple id doesn't exist, an XPath.

For example, the XPath of the first element in the first DataTable in gfpop gui is:

`//table[@id='DataTables_Table_0']/tbody/tr/td[0]`

So, to access that element with RSelenium, I can write:

```R
remDr$findElement("xpath", "//table[@id='DataTables_Table_0']/tbody/tr/td[0]")
```

Or, if I want to recreate the entire DataTable:

```R
data.table(
    matrix(
        lapply(
            remDr$findElements("xpath", "//table[@id='DataTables_Table_0']/tbody/tr/td"),
            function(x) unlist(x$getElementAttribute("innerHTML"))
            ),
        ncol = 10,
        byrow = TRUE)
)
```

## Reporting build status with RSelenium

SauceLabs keeps track of all Selenium tests, but it doesn't by default know whether these tests passed or failed.

To report pass/fail status, you need to explicitly send pass/fail flags through Selenium.

I couldn't find any RSelenium-specific examples on how to send those flags, but I noticed that I could by sending Javascript snippets through RSelenium. For example, here's a helper function that I made for annotating Selenium jobs:

```R
submit_job_info <- function(remDr, build, name, result) {
  if (!(result %in% c("passed", "failed", "true", "false"))) {
    stop("Invalid result. Please use: passed, failed, true, or false")
  }
  
  remDr$executeScript(paste0("sauce:job-build=", build))
  remDr$executeScript(paste0("sauce:job-name=", name))
  remDr$executeScript(paste0("sauce:job-result=", result))
  
}
```

Here's how I would use that function in a testthat script:

```R
build_name <- buildName <- paste0("my-test-build",
                    format(Sys.time(), "%m-%d-%Y_%s"))

test_that("1 equals 1", {
    remDr$open(silent=T)
    test_case <- 1==1
    submit_job_info(remDr = remDr, build = build_name,
        name = "1 equals 1", result = test_case)
    expect_equal(1, 1)
    # I could also do `expect_true(test_case)`, but that gives less
    # informative error messages
})
```

That way, SauceLabs will bundle together all tests that come from `build_name` (so, the current run of the tests)
and it will let me know whether those tests passed or failed.

## Local testing with SauceLabs

Right now, all of my testing is done through the online shinyapps.io server.

Maybe that is the way I'll end up doing all the testing, but I'm worried that testing will drain
the alloted time on the free-tier shinyapps.io.

But, SauceLabs does have a mechanism to test locally-running applications. And it also supports running applications in the background on Travis, and then running SauceLabs/Selenium tests within Travis. That would be super nifty (this way, for example, I could test whether new functionality in the app passes integration tests _before_ pushing deploying that new functionality).

The [RSelenium docs](https://docs.ropensci.org/RSelenium/articles/saucelabs.html) have a guide to running tests like that. On my local computer, I installed the [Sauce Connect Proxy](https://wiki.saucelabs.com/display/DOCS/Sauce+Connect+Proxy), followed the setup instructions, and ran the proxy.

The RSelenium docs were a bit unclear on how to connect, but you create remoteDriver on port 80 pointing to `ondemand.saucelabs.com` with your normal account credentials. Since the Sauce Connect Proxy is also connected to your account, Saucelabs knows to connect any `localhost` URL requests to your local computer via the Sauce Connect Proxy tunnel. And, if you're running through Travis, you need a seperate tunnel id that corresponds to the travis build id.

This seemed to connect appropriately, but I had a big problem interacting with the Shiny app this way: I would get a WebSocket-related error that prevented me from accessing the application. Evidently WebSocket compatability was a problem in the past (~2014), but those issues have been fixed. So, I put in a SauceLabs support ticket to see if they can help.

<details>
    <summary> Here's the ticket that I submitted: </summary>

Hi SauceLabs support,

I'm hoping to get some help connecting to a local application. I'm still very new to this, so hopefully I'm just overlooking something.

I'm running my app locally on port 3000. I'm also running Sauce Connect with `-B all` (see output below).

My application works as-expected locally. SauceLabs is also able to connect to the publicly-hosted version of the application.

However, SauceLabs cannot load the localhost:3000-hosted version of the application through the SauceConnect tunnel. The app loads briefly, then greys out.

In the JS Console, the application complains that a WebSocket connection to ws://localhost:3000/websocket had a 404 failure.

My application is made with R Shiny. John Harrison, who wrote the main R package for connecting to Selenium, had a similar WebSocket error in 2014. His issue seemed to have been resolved by `--vm-version dev-varnish` and/or `-B all`. But neither of those options resolve my issue.

A live test showing my issue can be found here: https://app.saucelabs.com/tests/394fa9bf14f24ddf8e50214ae91dd7b3#1

SauceConnect seems to be working well. Output:

```bash
julian-ThinkPad-T460:bin$ ./sc -u $SAUCE_USERNAME -k $SAUCE_SECRET_KEY -B all
26 Jun 10:10:37 - Sauce Connect 4.6.2, build 5183 ad61662
26 Jun 10:10:37 - REST: Using CA certificate bundle /etc/ssl/certs/ca-certificates.crt.
26 Jun 10:10:37 - REST: Using CA certificate verify path /etc/ssl/certs.
26 Jun 10:10:37 - TUNNEL: Using CA certificate bundle /etc/ssl/certs/ca-certificates.crt.
26 Jun 10:10:37 - TUNNEL: Using CA certificate verify path /etc/ssl/certs.
26 Jun 10:10:37 - Starting up; pid 1516379
26 Jun 10:10:37 - Command line arguments: ./sc -u julianstanley -k **** -B all
26 Jun 10:10:37 - Log file: /tmp/sc.log
26 Jun 10:10:37 - Pid file: /tmp/sc_client.pid
26 Jun 10:10:37 - Timezone: EDT GMT offset: -4h
26 Jun 10:10:37 - Using no proxy for connecting to Sauce Labs REST API.
26 Jun 10:10:37 - Started scproxy on port 40155.
26 Jun 10:10:37 - Please wait for 'you may start your tests' to start your tests.
26 Jun 10:10:50 - Secure remote tunnel VM provisioned.
26 Jun 10:10:50 - Tunnel ID: 4fbe703952fc48ac88e601734020edcb
26 Jun 10:10:51 - Starting OCSP certificate check.
26 Jun 10:10:51 - Using no proxy for connecting to http://status.geotrust.com.
26 Jun 10:10:51 - Using no proxy for connecting to http://ocsp.digicert.com.
26 Jun 10:10:51 - Reached a trusted CA. Certificate chain is verified.
26 Jun 10:10:51 - Using no proxy for connecting to tunnel VM.
26 Jun 10:10:51 - Selenium listener disabled.
26 Jun 10:10:51 - Establishing secure TLS connection to tunnel...
26 Jun 10:10:53 - Sauce Connect is up, you may start your tests.
```

And the log seems to be showing the same as the console:

```R
julian-ThinkPad-T460:~$ tail -100 /tmp/sc.log | grep localhost:3000 | grep socket -A10
2020-06-26 10:11:45.331 [1516379] PROXY 127.0.0.1:34558 (10.100.29.249) -> GET http://localhost:3000/websocket/ (655 bytes)
2020-06-26 10:11:45.335 [1516379] PROXY 127.0.0.1:34558 (10.100.29.249) <- 404 localhost:3000 (176 bytes)
2020-06-26 10:11:50.058 [1516379] PROXY 127.0.0.1:34568 (10.100.29.249) -> GET http://localhost:3000/shared/shiny.min.js.map (548 bytes)
2020-06-26 10:11:50.071 [1516379] PROXY 127.0.0.1:34570 (10.100.29.249) -> GET http://localhost:3000/shared/bootstrap/css/bootstrap.min.css.map (567 bytes)
2020-06-26 10:11:50.078 [1516379] PROXY 127.0.0.1:34568 (10.100.29.249) <- 200 localhost:3000 (115012 bytes)
2020-06-26 10:11:50.087 [1516379] PROXY 127.0.0.1:34576 (10.100.29.249) -> GET http://localhost:3000/crosstalk-1.1.0.1/js/crosstalk.min.js.map (566 bytes)
2020-06-26 10:11:50.097 [1516379] PROXY 127.0.0.1:34578 (10.100.29.249) -> GET http://localhost:3000/vis-7.5.2/vis-network.min.js.map (557 bytes)
2020-06-26 10:11:50.102 [1516379] PROXY 127.0.0.1:34578 (10.100.29.249) <- 404 localhost:3000 (116 bytes)
2020-06-26 10:11:50.107 [1516379] PROXY 127.0.0.1:34576 (10.100.29.249) <- 200 localhost:3000 (50557 bytes)
2020-06-26 10:11:50.117 [1516379] PROXY 127.0.0.1:34570 (10.100.29.249) <- 200 localhost:3000 (540654 bytes)
```

</details>

## Autodeployment to shinyapps.io

On my todo list for a while has been continuously deploying the app to shinyapps.io via travis.

There were some weird things happening before. I think I've fixed those by just using `install_github('julianstanley/gfpop-gui')` _before_ deploying, but after testing. Since the deployment only happens if the tests of the current build pass (and only on the master branch), then I can assume that the most up-to-date master branch is functional, install that, and use that up-to-date installation in the deployment.

Previously, I think I was using outdated versions of `gfpop-gui` in the deployment, which was causing the problems.

Right now deploying that way is putting the app at julianstanley.shinyapps.io/gfpop-gui instead of julianstanley.shinyapps.io/gfpopgui and I think that's just because the github repository is `gfpop-gui` but the package name is `gfpopgui`. I need to pick one of these (the discrepency is there because `gfpop-gui` is not a valid R package name--they can't have dashes).
