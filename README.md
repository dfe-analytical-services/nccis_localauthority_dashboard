<h1 align="center">
  <br>
DfE NEET and participation dashboard
  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

The dashboard provides users with an opportunity to investigate the proportion of 16 and 17 year olds not in education, employment or training (NEET) and participation information at a local authority level with national and regional comparisons. 

Live version of the dashboard can be accessed at

- https://department-for-education.shinyapps.io/neet-comparative-la-scorecard/

The dashboard is split across multiple tabs:

- **NEET and not known** includes information on NEET and not known rates for 16 and 17 year olds with national and regional comparisons and annual changes.

- **Vulnerable groups NEET** includes information on NEET and not known rates for those 16 and 17 year olds in a vulnerable group, with special educational need or disability (SEND), with special educational needs (SEN) support and no SEN.

- **Participation** includes information on the proportion of 16 and 17 years old participating in education or training and the proportion of offers made under September Guarantee.

- **Contextual - attainment and absence** includes information on post-16 attainment, GCSE attainment, absence and population.

The dashboard also includes further information on the data itself on the homepage tab, alongside accessibility and information on where to find further support.



---

## Requirements

### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)

  
---

## How to use


### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The function run_tests_locally() is created in the Rprofile script and is available in the RStudio console at all times to run both the unit and ui tests.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function tidy_code() is created in the Rprofile script and therefore is always available in the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.


---

## How to contribute

Our contributing guidelines can be found at https://github.com/dfe-analytical-services/nccis_localauthority_dashboard/blob/main/CONTRIBUTING.md

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the team can merge pull requests. Add AnnekaAlbon as requested reviewers, and the team will review before merging.

---

## Contact

If you have any questions about the dashboard please contact post16.statistics@education.gov.uk
