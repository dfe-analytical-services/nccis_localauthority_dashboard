library(shinytest2)
library(diffviewer)

test_that("Migrated shinytest test: dashboard_ui_tests.R", {
  app <- AppDriver$new(
    load_timeout = 240000,
    timeout = 160000
  )

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
    "NEET_nk_vb",
    "Not_known",
    "Participating",
    "Sept_Guarantee",
    "NCCIS_pop"
  )

  # 1. Does it load?
  app$expect_values(input = listInputs, output = outputs)

  # 2. Does the LA selection update?
  app$set_inputs(LA_choice = "Southwark")
  app$expect_values(input = listInputs, output = outputs)

  # 3. Does the first dashboard panel load?
  app$set_inputs(tabsetpanel = "neet")
  app$expect_values(input = listInputs, output = outputs)

  # 4. Does the vulnerable dashboard panel load?
  app$set_inputs(tabsetpanel = "vulnerable")
  app$expect_values(input = listInputs, output = outputs)

  # 5. Does the participation dashboard panel load?
  app$set_inputs(tabsetpanel = "participation")
  app$expect_values(input = listInputs, output = outputs)

  # 6. Does the contextual dashboard panel load?
  app$set_inputs(tabsetpanel = "contextual")
  app$expect_values(input = listInputs, output = outputs)
})
