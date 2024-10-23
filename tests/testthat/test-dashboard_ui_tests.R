library(shinytest2)

test_that("Migrated shinytest test: dashboard_ui_tests.R", {
  app <- AppDriver$new(load_timeout = 120000)

  listInputs <- c(
    "navlistPanel",
    "tabsetpanel",
    "LA_choice",
    "acc_colour_scheme"
  )

  outputs <- c(
    "vg_cohort",
    "data_description",
    "NEET",
    "NEET_nk",
    "Not_known",
    "Participating",
    "Sept_Guarantee",
    "NCCIS_pop"
  )

  # 1. Does it load?
  app$expect_values(input = listInputs, output = c())

  # 2. Does the first dashboard panel load?
  app$set_inputs(navlistPanel = "dashboard", wait_ = FALSE, tabsetpanel = "neet", timeout_ = 5000)
  app$expect_values(input = listInputs, output = outputs)

  # 3. Does the vulnerable dashboard panel load?
  app$set_inputs(navlistPanel = "dashboard", tabsetpanel = "vulnerable", timeout_ = 5000)
  app$expect_values(input = listInputs, output = outputs)

  # 4. Does the participation dashboard panel load?
  app$set_inputs(navlistPanel = "dashboard", tabsetpanel = "participation", timeout_ = 5000)
  app$expect_values(input = listInputs, output = outputs)

  # 5. Does the contextual dashboard panel load?
  app$set_inputs(navlistPanel = "dashboard", tabsetpanel = "contextual", timeout_ = 5000)
  app$expect_values(input = listInputs, output = outputs)
})
