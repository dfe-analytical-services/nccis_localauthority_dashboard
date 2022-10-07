app <- ShinyDriver$new("../../", loadTimeout = 6.e4)
app$snapshotInit("dashboard_ui_tests", screenshot = FALSE)

listInputs <- c(
  "navlistPanel",
  "tabsetpanel",
  "LA_choice"
)

# 1. Does it load?
app$snapshot()


# 2. Does the first dashboard panel load?
app$setInputs(navlistPanel = "dashboard",wait_=FALSE, values_=FALSE)
app$setInputs(tabsetpanel = "neet",timeout_=5.e3)
app$snapshot(list(
  input = listInputs,
  output = c(
    "NEET_nk",
    "NEET",
    "Not_known"
  )
))

# 3. Does the vulnerable dashboard panel load?
app$setInputs(navlistPanel = "dashboard",wait_=FALSE, values_=FALSE)
app$setInputs(tabsetpanel = "vulnerable",timeout_=5.e3)
app$snapshot(list(
  input = listInputs,
  output = c(
    "vulnerable.bartext"
  )
))

# 4. Does the participation dashboard panel load?
app$setInputs(navlistPanel = "dashboard",wait_=FALSE, values_=FALSE)
app$setInputs(tabsetpanel = "participation",timeout_=5.e3)
app$snapshot(list(
  input = listInputs,
  output = c(
    "Participating",
    "Sept_Guarantee"
  )
))

# 5. Does the contextual dashboard panel load?
app$setInputs(navlistPanel = "dashboard",wait_=FALSE, values_=FALSE)
app$setInputs(tabsetpanel = "contextual",timeout_=5.e3)
app$snapshot(list(
  input = listInputs,
  output = c(
    "ONS_pop",
    "NCCIS_pop",
    "contextual.bartext"
  )
))

