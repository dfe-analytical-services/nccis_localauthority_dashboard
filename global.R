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
shhh(library(shinydashboard))
shhh(library(shinyWidgets))
shhh(library(dfeshiny))
shhh(library(dfeR))
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
shhh(library(markdown))
shhh(library(webshot))
shhh(library(scales))
shhh(library(checkmate))

site_primary <- "https://department-for-education.shinyapps.io/nccis_localauthority_dashboard/"
site_overflow <- "https://department-for-education.shinyapps.io/nccis_localauthority_overflow/"
sites_list <- c(site_primary) # We can add further mirrors where necessary. Each one can generally handle about 2,500 users simultaneously
ees_pub_name <- "Participation in education training and NEET age 16 to 17 by local authority" # Update this with your parent publication name (e.g. the EES publication)
ees_publication <- "https://explore-education-statistics.service.gov.uk/find-statistics/participation-in-education-training-and-neet-age-16-to-17-by-local-authority" # Update with parent publication link
team_email <- "post16.statistics@education.gov.uk"
google_analytics_key <- "4TJQVNWTCK"

# Load the data required
la_ud <- read_csv("data/UD_NEETNK_LA_dashboard_final.csv",
  col_types = cols(.default = "c")
)

# Set year references - TO BE UPDATED EVERY YEAR
latest_year <- 2024
last_year <- latest_year - 1
latest_year_end <- latest_year - 1
previous_year_end <- latest_year - 2

# Creating useful functions
# Here we create a function to say increased/decreased for yearly changes which we need in the text on the app.

change_ed <- function(numA) {
  if (numA == 0.0) {
    return("stable ")
  }
  if (numA == "z") {
    return("")
  }
  if (numA < 0.0) {
    return("down ")
  }

  if (numA > 0.0) {
    return("up ")
  } else {
    return(" ")
  }
}

# Comma separating

cs_num <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
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



# Reshaping data for plots-----------------------------------------
## Participation type data-----------------------------------------

# reshape the data so it plots neatly!
participation_data_fte <- la_ud %>%
  # select only participation types
  select(geographic_level, region_name, la_name, Full_time_education_percent) %>%
  # Put England and region name into LA name
  mutate(la_name = case_when(
    geographic_level == "National" ~ "England",
    geographic_level == "Regional" ~ region_name,
    TRUE ~ la_name
  ), participation_type = "Full-time education")

colnames(participation_data_fte)[colnames(participation_data_fte) == "Full_time_education_percent"] <- "value"

participation_data_fte <- participation_data_fte %>%
  select(la_name, participation_type, value)

participation_data_app <- la_ud %>%
  # select only participation types
  select(geographic_level, region_name, la_name, Apprenticeship_percent) %>%
  # Put England and region name into LA name
  mutate(la_name = case_when(
    geographic_level == "National" ~ "England",
    geographic_level == "Regional" ~ region_name,
    TRUE ~ la_name
  ), participation_type = "Apprenticeship")

colnames(participation_data_app)[colnames(participation_data_app) == "Apprenticeship_percent"] <- "value"

participation_data_app <- participation_data_app %>%
  select(la_name, participation_type, value)

participation_data_other <- la_ud %>%
  # select only participation types
  select(geographic_level, region_name, la_name, Other_education_and_training_percent) %>%
  # Put England and region name into LA name
  mutate(la_name = case_when(
    geographic_level == "National" ~ "England",
    geographic_level == "Regional" ~ region_name,
    TRUE ~ la_name
  ), participation_type = "Other")

colnames(participation_data_other)[colnames(participation_data_other) == "Other_education_and_training_percent"] <- "value"

participation_data_other <- participation_data_other %>%
  select(la_name, participation_type, value)

# pull the types together into one file
participation_data <- bind_rows(participation_data_fte, participation_data_app, participation_data_other)

participation_data <- participation_data %>%
  mutate(value = as.numeric(value))

partEng <- participation_data %>% filter(la_name == "England")

## Vulnerable groups data---------------------------------------------
# reshape the data so it plots neatly!
vulnerable_data <- la_ud %>%
  # select only vulnerable info
  select(geographic_level, region_name, la_name, NEET_NK_noSEN_percent, NEET_NK_EHCP_percent, NEET_NK_SENDsupport_percent, VG_NEET_NK_percentage) %>%
  # Put England and region name into LA name
  mutate(la_name = case_when(
    geographic_level == "National" ~ "England",
    geographic_level == "Regional" ~ region_name,
    TRUE ~ la_name
  ))


vulnerable_data <- vulnerable_data %>%
  select(la_name, NEET_NK_noSEN_percent, NEET_NK_EHCP_percent, NEET_NK_SENDsupport_percent, VG_NEET_NK_percentage)

vulnerable_data <- vulnerable_data %>%
  mutate(
    NEET_NK_noSEN_percent = as.numeric(NEET_NK_noSEN_percent), NEET_NK_EHCP_percent = as.numeric(NEET_NK_EHCP_percent), NEET_NK_SENDsupport_percent = as.numeric(NEET_NK_SENDsupport_percent),
    VG_NEET_NK_percentage = as.numeric(VG_NEET_NK_percentage)
  )


vulnerableEng <- vulnerable_data %>% filter(la_name == "England")


## Contextual data---------------------------------------------
# reshape the data so it plots neatly!
contextual_data <- la_ud %>%
  # select only contextual info
  select(geographic_level, region_name, la_name, Level_3, L2_em_GCSE_othL2, avg_att8, pt_l2basics_94, sess_overall_percent, enrolments_pa_10_exact_percent) %>%
  # Put England and region name into LA name
  mutate(la_name = case_when(
    geographic_level == "National" ~ "England",
    geographic_level == "Regional" ~ region_name,
    TRUE ~ la_name
  ))


contextual_data <- contextual_data %>%
  select(la_name, Level_3, L2_em_GCSE_othL2, avg_att8, pt_l2basics_94, sess_overall_percent, enrolments_pa_10_exact_percent)

contextual_data <- contextual_data %>%
  mutate(
    Level_3 = as.numeric(Level_3), L2_em_GCSE_othL2 = as.numeric(L2_em_GCSE_othL2), avg_att8 = as.numeric(avg_att8),
    pt_l2basics_94 = as.numeric(pt_l2basics_94), sess_overall_percent = as.numeric(sess_overall_percent), enrolments_pa_10_exact_percent = as.numeric(enrolments_pa_10_exact_percent)
  )

contextEng <- contextual_data %>% filter(la_name == "England")
