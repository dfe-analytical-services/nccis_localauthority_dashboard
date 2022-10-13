homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("NEET and participation LA scorecard"),
          h2("Introduction"),
          p("The Department for Education (DfE) publishes an estimate each year
                of the proportion of young people not in education, employment or
                training (NEET).  However, evidence shows that there are a range of
                factors that can affect the proportion NEET, and this scorecard aims
                to put the headline figure into context by setting it alongside a
                range of other related information."),
          br(),
          p("There is a separate scorecard for each local authority in England
                except the City of London and Isles of Scilly as data is not available
                for all indicators and small numbers can result in large changes
                in percentages from one year to the next."),
          br(),
          p("Each area's performance is compared with that of other local authorities in England.
            The best performing with the lowest NEET/highest participation fall into the
            green quintile, whereas local authorities with the highest NEET/lowest participation fall
            into the red quintile."),
          br(),
          p("The average England percentage may not necessarily fall in the middle of quintile 3
            because the average is calculated by averaging all the values; for example if some of the values
            are particularly large compared with the other values, the average will be larger than the middle ranked
            local authority and may fall outside the middle quintile."),
          br(),
          p("Local authorities with no value or a suppressed value are excluded from the quintile calculation")
        ),

        ## Left panel -------------------------------------------------------

        column(
          12,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Scorecard Contents")
              ),
              div(
                class = "panel-body",
                h3("NEET and not known"),
                br("This section shows the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year."),
                br("The proportion not known indicator shows how effective the arrangements are in each area for tracking
                       young people's participation in education and training."),
                br("Whilst local authorities are responsible for tracking, they depend on schools, colleges and other partners
                       who work with young people sharing information with them. There is a risk that young people NEET have not been
                       identified in areas where effective tracking arrangements are not in place."),
                h3("Vulnerable groups NEET"),
                br("This section includes the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year with special education needs
                       and disability (SEND), SEN support or those that fall into the vulnerable group."),
                br("A young person is said to be in a vulnerable group if they have any of the following characteristics
                       (taken from IC01 of the NCCIS returns): Looked after/In care (110), Refugee/Asylum seeker (130),
                       Carer-not own child (140), Disclosed substance misue (150), Care leaver (160), Supervised by YOT (170), Parent-not caring for own child (190),
                       Alternative provision (200), Mental health flag (210)."),
                h3("Participation"),
                br("This section shows the proportion of 16 and 17 year olds living in each area who were in education or training
                       at the end of March."),
                br("There is a breakdown of the three main routes that young people choose: full-time education, apprenticeship, and other education or training
                       (this includes part-time education, work based learning and employment with study towards a regulated qualification)."),
                br("It also shows the proportion of 16 and 17 year olds receiving an offer of a place in education and training under the September
                       Guarantee. Local authorities are responsible for leading the 'September Guarantee' process, working with schools and colleges
                       across their area."),
                br("There are some young people who have not yet made a decision about what they want to do next, have other plans, or who cannot be
                       contacted. These young people are at risk of becoming NEET."),
                h3("Contextual information"),
                br(
                  "This section covers ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/2020-21", "outcomes, "),
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised/2020-21", "GCSE attainment"), " and ",
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england", "school attendance"), " of young people living in each area."
                ),
                br("Surveys show that higher attainment at age 16 is the factor most closely associated with participation and a lower
                       risk of becoming NEET between the ages of 16 and 18."),
                br("Young people who have poor attendance or who are excluded from schools are at greater risk of becoming NEET."),
                br("Due to the coronavirus (COVID-19 pandemic all summer 2020 and 2021 exams were cancelled and absence statistics were
                       notably different to previous years. Therefore the contextual information on absence and attainment has not been
                       included in the scorecard this year."),
                # tags$div(
                # title = "This section is useful if you want to understand how well different industries retain graduates.",
                # h3(actionLink("link_to_app_content_tab", "App Content"))
                # ),
                br()
              )
            )
          )
        )
      )
    )
  )
}


dashboard_panel <- function() {
  tabPanel(
    value = "dashboard",
    "Dashboard",

    # Define UI for application that draws a histogram

    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          h1(textOutput("data_description")),
        ),
        column(
          width = 12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              width = 12,
              column(
                width = 6,
                p("View a local authority scorecard using the drop down below."),
                selectInput("LA_choice",
                  label = p(strong("Choose a local authority")),
                  choices = levels(LA_options),
                  selected = "LA demo"
                ),
                p("Switch between different indicators using the tabs below.")
              ),
              column(
                width = 6,
                p(strong("Download data for all local authorities", style = "color:white")),
                myDownloadButton(
                  "download_ud",
                  "Download data"
                ),
                # insert download button here
                br(),
                br(),
                p(strong("Download summary pdf for chosen local authority")),
                myDownloadButton(
                  "pdfDownload",
                  "Download report"
                )
              )
            )
          )
        ),
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanel",
            tabPanel(
              value = "neet",
              title = "NEET and not known",
              fluidRow(
                column(
                  width = 12,
                  br(),
                  p(strong("16-17 year olds at end ", latest_year_end, "(average of December, January and February)")),
                  h2("NEET and not known"),
                  column(width = 2),
                  column(width = 8, plotlyOutput("NEET_nk_gauge", width = "100%") %>% withSpinner()),
                  column(width = 2),
                  valueBoxOutput("NEET_nk", width = 12)
                )
              ),
              fluidRow(
                column(
                  6,
                  h2("NEET"),
                  plotlyOutput("NEET_gauge", width = "100%") %>% withSpinner(),
                  valueBoxOutput("NEET", width = 12),
                ),
                column(
                  6,
                  h2("Not known"),
                  plotlyOutput("Nk_gauge", width = "100%") %>% withSpinner(),
                  valueBoxOutput("Not_known", width = 12)
                )
              )
            ),
            tabPanel(
              value = "vulnerable",
              title = "Vulnerable Groups NEET",
              fluidRow(
                br(),
                p(strong("16-17 year olds NEET or activity not known at end ", latest_year_end, "(average of December, January and February)")),
                column(
                  6,
                  h2("Vulnerable group"),
                  plotlyOutput("vulnerable_plot") %>% withSpinner()
                ),
                column(
                  6,
                  br(),
                  br(),
                  br(),
                  br(),
                  p(strong("A young person is said to be in a vulnerable group if they have any of the following characteristics
                       (taken from IC01 of the NCCIS returns): Looked after/In care (110), Refugee/Asylum seeker (130),
                       Carer-not own child (140), Disclosed substance misue (150), Care leaver (160), Supervised by YOT (170), Parent-not caring for own child (190),
                       Alternative provision (200), Mental health flag (210)."))
              )
              ),
              fluidRow(
                column(
                  4,
                  h2("No SEND"),
                  plotlyOutput("No_SEN_plot") %>% withSpinner()
                ),
                column(
                  4,
                  h2("SEND (EHCP)"),
                  plotlyOutput("EHCP_plot") %>% withSpinner()
                ),
                column(
                  4,
                  h2("SEN support"),
                  plotlyOutput("SEN_support_plot") %>% withSpinner()
                )
                )
            ),
            tabPanel(
              value = "participation",
              title = "Participation",
              br(),
              p(strong("16-17 year olds March ", latest_year)),
              h2("Participating in education and training"),
              fluidRow(
                column(
                  8,
                  plotlyOutput("Participation_gauge", width = "100%") %>% withSpinner(),
                  valueBoxOutput("Participating", width = 12),
                  br(),
                  br(),
                  h2("September Guarantee: % offered an education place"),
                  plotlyOutput("Sept_Guar_gauge", width = "100%") %>% withSpinner(),
                  valueBoxOutput("Sept_Guarantee", width = 12)
                ),
                fluidRow(
                  column(
                    4,
                    p(strong(paste0("Type of education or training"))),
                    plotlyOutput("participation_types") %>% withSpinner()
                  )
                )
              )
            ),
            tabPanel(
              value = "contextual",
              title = "Contextual - attainment and attendance",
              gov_row(
                column(width = 12, br()),
                column(
                  6,
                  h2("Post 16 attainment"),
                  br(),
                  p(strong("% 19 year olds achieving level 3")),
                  plotlyOutput("level3_plot") %>% withSpinner(),
                  br(),
                  br(),
                  h2("GCSE attainment"),
                  p(strong("Average attainment 8 score per pupil")),
                  plotlyOutput("Attainment8_plot") %>% withSpinner(),
                  br(),
                  br(),
                  h2("School attendance"),
                  p(strong("Overall absence (% of sessions)")),
                  plotlyOutput("overall_abs_plot") %>% withSpinner(),
                  br(),
                  br(),
                  h2("16-17 LA population"),
                  # p("ONS estimate"),
                  valueBoxOutput("ONS_pop", width = 12)
                ),
                column(
                  6,
                  gov_row(
                    column(
                      12,
                      br(),
                      br(),
                      p(strong("% 19 year olds achieving GCSE 9-4 standard pass in
                      English and maths (or equivalent) between ages 16 and 19,
                      for those who had not achieved this level by 16")),
                      plotlyOutput("L2_EM_GCSE_plot") %>% withSpinner(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      p(strong("% 9-4 standard pass in English and maths GCSEs")),
                      plotlyOutput("EM_pass_plot") %>% withSpinner(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      p(strong("Persistent absentees (% of pupils)")),
                      plotlyOutput("Persistent_abs_plot") %>% withSpinner(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      valueBoxOutput("NCCIS_pop", width = 12)
                    )
                  )
                ),
                uiOutput("contextual.bartext")
              )
            )

            # add box to show user input
          )
        )
      )
    )
  )
}
