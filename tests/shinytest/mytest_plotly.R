app <- ShinyDriver$new("../../", seed = 123)
app$snapshotInit("mytest_plotly")

app$setInputs(ndata = 50)
app$setInputs(genData = "click")
app$setInputs(tabs = "Analysis")
app$setInputs(updateGraph = "click")
app$setInputs(runGfpop = "click")
app$snapshot()
