# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------

shhh <- suppressPackageStartupMessages # It's a library, so shhh!
shhh(library(shiny))
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(testthat))
shhh(library(shinytest))
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(shinyGovstyle))
shhh(library(readr))
shhh(library(dplyr))
shhh(library(plotly))
shhh(library(shinycssloaders))
shhh(library(stringr))
shhh(library(stringi))
shhh(library(highr))
shhh(library(tinytex))
shhh(library(rmarkdown))
shhh(library(webshot))

site_primary <- "https://department-for-education.shinyapps.io/nccis_localauthority_dashboard/"
site_overflow <- "https://department-for-education.shinyapps.io/nccis_localauthority_overflow/"

# Load the data required
la_ud <- read_csv("data/UD_NEETNK_LA_dashboard_2021_vg_removed.csv",
  col_types = cols(.default = "c")
)

# Set year references - TO BE UPDATED EVERY YEAR
latest_year <- 2022
last_year <- latest_year - 1
latest_year_end <- latest_year - 1
previous_year_end <- latest_year - 2

# Creating useful functions
# Here we create a function to say increased/decreased for yearly changes which we need in the text on the app.

change_ed <- function(numA) {
  if (numA == 0.0) {
    return("stable ")
  }
  if (numA < 0.0) {
    return("down ")
  }

  if (numA > 0.0) {
    return("up ")
  } else {
    return("annual change not available ")
  }
}

# Filtering the data----------------------------------------

# LA options - reordered
LA_names <- filter(la_ud, la_name != "z")

LA_options <- sort(unique(LA_names$la_name)) %>%
  as.factor()

# Reducing white space around plots

par(mar = c(4, 4, 0.1, 0.1)) # doesn't seem to make a difference

# Functions ---------------------------------------------------------------------------------

# Here's an example function for simplifying the code needed to commas separate numbers:

# cs_num ----------------------------------------------------------------------------
# Comma separating function

cs_num <- function(value) {
  format(value, big.mark = ",", trim = TRUE)
}

# tidy_code_function -------------------------------------------------------------------------------
# Code to tidy up the scripts.

tidy_code_function <- function() {
  message("----------------------------------------")
  message("App scripts")
  message("----------------------------------------")
  app_scripts <- eval(styler::style_dir(recursive = FALSE)$changed)
  message("R scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("R/", filetype = "r")$changed)
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
}


# Conditional colour function for annual changes----------------------------------------------------
cond_color <- function(condition, true_color = "green") {
  if (is.na(condition)) {
    return("black")
  }
  colours <- c("green", "#e00000")
  return(ifelse(condition, true_color, colours[!colours == true_color]))
}

# Source scripts ---------------------------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets to the server file.
# It's best to do this here instead of the server file, to improve performance.

# source("R/filename.r")


# appLoadingCSS ----------------------------------------------------------------------------
# Set up loading screen

appLoadingCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
# Create download button without the icon
myDownloadButton <- function(outputId, label = "Download") {
  tags$a(
    id = outputId, class = "btn btn-default shiny-download-link", href = "",
    target = "_blank", download = NA, NULL, label
  )
}
