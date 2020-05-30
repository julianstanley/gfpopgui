app <- ShinyDriver$new("../../", seed = 100)
app$snapshotInit("initial_test")

app$snapshot()
app$setInputs(graphType = "isotonic")
app$snapshot()
app$setInputs(updateGraph = "click")
