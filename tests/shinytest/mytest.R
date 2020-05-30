app <- ShinyDriver$new("../../", seed = 123)
app$snapshotInit("mytest")

app$setInputs(ndata = 50)
app$setInputs(genData = "click")
app$setInputs(tabs = "Analysis")
app$snapshot(list(output = "gfpopPlot"))
