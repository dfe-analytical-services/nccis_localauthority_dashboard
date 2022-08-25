# ---------------------------------------------------------
# This is the global file.
# Use it to store functions, library calls, source files etc.
# Moving these out of the server file and into here improves performance
# The global file is run only once when the app launches and stays consistent across users
# whereas the server and UI files are constantly interacting and responsive to user input.
#
# ---------------------------------------------------------


# Library calls ---------------------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(tools)
library(testthat)
library(shinytest)
library(shinydashboard)
library(shinyWidgets)
library(shinyGovstyle)
library(readr)
library(dplyr)

#Load the data required 
la_ud <- read_csv('data/UD_NEETNK_LA_dashboard_dummy_data.csv', col_types = cols(.default = "c"))

#Set year references
latest_year <- 2022
last_year   <- latest_year - 1

# Creating useful functions
# Here we create a function to say increased/decreased for yearly changes which we need in the text on the app. 

change_ed <- function(numA, numB) {
  
  if(numA < numB) {return ('increased from')}
  
  if(numA > numB) {return ('decreased from')}
  
  else {return('stayed the same at')}
  
}

#Filtering the data----------------------------------------

# LA options - reordered
LA_names <- filter(la_ud, la_name!="z")

LA_options <- sort(unique(LA_names$la_name)) %>%
  as.factor() 

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
  message("Test scripts")
  message("----------------------------------------")
  test_scripts <- eval(styler::style_dir("tests/", filetype = "r")$changed)
  script_changes <- c(app_scripts, test_scripts)
  return(script_changes)
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

source("R/support_links.R")
