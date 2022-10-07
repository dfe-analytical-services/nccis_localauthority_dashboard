app <- ShinyDriver$new("../../", loadTimeout = 1.e4)
app$snapshotInit("dashboard_ui_tests", screenshot = FALSE)

app$snapshot(items = list(
  input = c("bins"),
  output = c("box_info")
))
