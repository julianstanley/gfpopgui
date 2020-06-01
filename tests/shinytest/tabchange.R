app <- ShinyDriver$new("../../")
app$snapshotInit("tabchange")

app$snapshot()
app$setInputs(tabs = "Analysis")
app$snapshot()
