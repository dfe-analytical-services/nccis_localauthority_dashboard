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
                to put the headline figure into context by presenting it alongside a
                range of other related information."),
          br(),
          p("There is a separate scorecard for each local authority in England
                except the City of London and Isles of Scilly. For these two local authorities, data is not available
                for all indicators and small numbers can result in large changes
                in percentages from one year to the next."),
          br(),
          p("Each area's performance is compared with that of other local authorities in England.
            The best performing with the lowest NEET/highest participation fall into the
            green quintile, whereas local authorities with the highest NEET/lowest participation fall
            into the red quintile."),
          br(),
          p(strong("Some caution should be taken if using these figures due to the estimates being based on
            management information and there being considerable variation at local authority level in
            how well 16 and 17 year olds are tracked and hence not known proportions can impact on the
            estimates of the proportion NEET.")),
          # p(strong("The Department for Education's definitive measures for England
          # of participation and not in education, employment or training (NEET) for 16 to 18 year olds are
          # published annually in the national statistics release", a(
          # href = "https://explore-education-statistics.service.gov.uk/find-statistics/participation-in-education-and-training-and-employment",
          # "'Participation in Education, Training and Employment age 16 to 18'."
          # )))
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
                h3("NEET and activity not known"),
                br("This section shows the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year."),
                br("The proportion not known indicator shows how effective the arrangements are in each area for tracking
                       young people's participation in education and training."),
                br("Whilst local authorities are responsible for tracking, they depend on schools, colleges and other partners
                       who work with young people sharing information with them. There is a risk that young people NEET have not been
                       identified in areas where effective tracking arrangements are not in place."),
                h3("Vulnerable groups NEET"),
                br("This section includes experimental data showing the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year with special education needs
                       and disability (SEND)/education, health and care plan (EHCP), SEN support or those that fall into the vulnerable group."),
                br("A young person is said to be in a vulnerable group if they have any of the following characteristics (taken from IC01 of
                   the NCCIS returns):"),
                p("110 - Looked after/In care"),
                p("130 - Refugee/Asylum seeker"),
                p("140 - Carer-not own child"),
                p("150 - Disclosed substance misuse"),
                p("160 - Care leaver"),
                p("170 - Supervised by YOT (Youth Offending Team)"),
                p("190 - Parent-not caring for own child"),
                p("200 - Alternative provision"),
                p("210 - Mental health flag"),
                p(strong("Caution should be used in interpreting these figures due to variation in local authority reporting of these characteristics.
                    The proportion of the 16/17 cohort classified as being in the vulnerable group ranges from 0.0 percent to 10.0 percent.
                   Due to this variation and likely inaccuracy in some local authorities in identifying the full vulnerable group cohort,
                   NEET/not known rates may not be representative for the vulnerable group cohort. The underlying data accompanying this scorecard
                   includes the proportion of each local authority's cohort identified as having one of the vulnerable characteristics (VG_cohort_percentage).")),
                h3("Participation"),
                br("This section shows the proportion of 16 and 17 year olds living in each area who were in education or training
                       at the end of March."),
                br("There is a breakdown of the three main routes that young people choose: full-time education, apprenticeship, and other education or training
                       (this includes part-time education, work based learning and employment with study towards a regulated qualification)."),
                br("It also shows the proportion of 16 and 17 year olds receiving an offer of a place in education and training under the September
                       Guarantee. The September Guarantee is a guarantee of an offer, made by the end of September, of an
                      appropriate place in post-16 education or training for every young person completing compulsory
                      education. This is particularly important as it helps young people make a seamless transition into
                      post-16 learning or employment with training. Local authorities are responsible for leading the 'September Guarantee' process, working with schools and colleges
                       across their area."),
                br("There are some young people who have not yet made a decision about what they want to do next, have other plans, or who cannot be
                       contacted. These young people are at risk of becoming NEET."),
                h3("Contextual - attainment and attendence"),
                br(
                  "This section covers ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/2021-22", "post 16 attainment"), ", ",
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised/2021-22", "GCSE attainment"), " and ",
                  a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england/2021-22", "school attendance"), " of young people living in each area.
                  It also provides the Client Caseload Information System (CCIS) population of 16 and 17 year olds in the local authority. In previous years we have also included the Office for National Statistics (ONS) population estimate for comparison.
                  However, there is delay to the latest ONS population updates so this information is not available in the dashboard this year."
                ),
                br("Surveys show that higher attainment at age 16 is the factor most closely associated with participation and a lower
                       risk of becoming NEET between the ages of 16 and 18."),
                br("Young people who have poor attendance or who are excluded from schools are at greater risk of becoming NEET."),
                # br("Due to the coronavirus (COVID-19) pandemic, all summer 2020 and 2021 exams were cancelled and absence statistics were
                # notably different to previous years. Therefore the contextual information on absence and attainment has not been
                # included in the scorecard this year."),
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
    "Scorecard",

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
                  selected = "Barking and Dagenham"
                ),
                radioGroupButtons(
                  inputId = "acc_colour_scheme", label = "Switch between accessible and hi-vis gauge chart colour schemes",
                  choiceNames = c("Hi-vis", "Accessible"),
                  choiceValues = c(FALSE, TRUE),
                )
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
              ),
              column(
                12,
                tags$b("Different indicators can be viewed by switching between the tabs below.")
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
              title = "NEET and activity not known",
              fluidRow(
                column(
                  width = 12,
                  br(),
                  # tags$b("16-17 year olds at end ", latest_year_end, "(average of December, January and February)"),
                  p(strong("Gauges below show where the LA rate sits within the", actionLink("link_to_tech_notes1", "quintile"), "range of all LAs and regional/England averages.")),
                  h2("NEET and activity not known"),
                  h3("16-17 year olds at end ", latest_year_end, "(average of December, January and February)"),
                  column(width = 2),
                  column(width = 8, plotlyOutput("NEET_nk_gauge", width = "100%", inline = T) %>% withSpinner()),
                  column(width = 2),
                  valueBoxOutput("NEET_nk", width = 12)
                )
              ),
              fluidRow(
                column(
                  6,
                  h2("NEET"),
                  plotlyOutput("NEET_gauge", width = "100%") %>% withSpinner(),
                  valueBoxOutput("NEET", width = 12)
                ),
                column(
                  6,
                  h2("Activity not known"),
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
                # tags$b("16-17 year olds NEET or activity not known at end ", latest_year_end, "(average of December, January and February)"),
                column(
                  6,
                  h2("Vulnerable group"),
                  h3("16-17 year olds NEET or activity not known at end ", latest_year_end, "(average of December, January and February)"),
                  plotlyOutput("vulnerable_plot") %>% withSpinner()
                ),
                column(
                  6,
                  br(),
                  br(),
                  br(),
                  br(),
                  p("A young person is said to be in a vulnerable group if they have any of the following characteristics
                       (taken from IC01 of the NCCIS returns):"),
                  p("110 - Looked after/In care"),
                  p("130 - Refugee/Asylum seeker"),
                  p("140 - Carer-not own child"),
                  p("150 - Disclosed substance misuse"),
                  p("160 - Care leaver"),
                  p("170 - Supervised by YOT (Youth Offending Team)"),
                  p("190 - Parent-not caring for own child"),
                  p("200 - Alternative provision"),
                  p("210 - Mental health flag"),
                  h4(textOutput("vg_cohort"), style = "color:#28A197")
                )
              ),
              fluidRow(
                br(),
                br(),
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
              ),
              fluidRow(
                br(),
                p(strong("Please note that if the local authority has suppressed data to avoid disclosure or their data is not available, no bar will appear on the plot."))
              )
            ),
            tabPanel(
              value = "participation",
              title = "Participation",
              br(),
              # tags$b("16-17 year olds March ", latest_year),
              p(strong("Gauges below show where the LA rate sits within the", actionLink("link_to_tech_notes2", "quintile"), "range of all LAs and regional/England averages.")),
              h2("Participating in education and training"),
              h3("16-17 year olds March ", latest_year),
              fluidRow(
                column(
                  7,
                  plotlyOutput("Participation_gauge", width = "92%") %>% withSpinner(),
                  valueBoxOutput("Participating", width = 12),
                  br(),
                  br(),
                  br(),
                  br(),
                  br()
                ),
                column(
                  5,
                  p(strong(paste0("Type of education or training"))),
                  plotlyOutput("participation_types") %>% withSpinner()
                )
              ),
              fluidRow(
                h2("September Guarantee: % offered an education or training place"),
                column(
                  7,
                  plotlyOutput("Sept_Guar_gauge", width = "92%") %>% withSpinner(),
                  valueBoxOutput("Sept_Guarantee", width = 12),
                  p("In some instances, a local authority may have slightly over 100% offers made.
                    This is due to additional young people being added after the September Guarantee cohort is fixed.")
                ),
              )
            ),
            tabPanel(
              value = "contextual",
              title = "Contextual - attainment and attendance",
              gov_row(
                column(width = 12, br()),
                h2("Post 16 attainment"),
                br("The following figures can be found in the ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/2021-22", "'Level 2 and 3 attainment age 16 to 25: 2021/22'"), " release."),
                br(),
                column(
                  6,
                  br(),
                  br(),
                  p(strong("% 19 year olds achieving level 3")),
                  plotlyOutput("level3_plot") %>% withSpinner(),
                  br()
                ),
                column(
                  6,
                  p(strong("% 19 year olds achieving GCSE 9-4 standard pass in
                      English and maths (or equivalent) between ages 16 and 19,
                      for those who had not achieved this level by 16")),
                  plotlyOutput("L2_EM_GCSE_plot") %>% withSpinner(),
                  br()
                )
              ),
              gov_row(
                column(width = 12, br()),
                br(),
                h2("GCSE attainment"),
                br("The following figures can be found in the ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance-revised/2021-22", "'Key stage 4 performance: 2021/22'"), " release."),
                br(),
                column(
                  6,
                  p(strong("Average attainment 8 score per pupil")),
                  plotlyOutput("Attainment8_plot") %>% withSpinner(),
                  br()
                ),
                column(
                  6,
                  p(strong("% 9-4 standard pass in English and maths GCSEs")),
                  plotlyOutput("EM_pass_plot") %>% withSpinner(),
                  br()
                )
              ),
              gov_row(
                column(width = 12, br()),
                br(),
                h2("School attendance"),
                br("The following state-funded secondary school attendance figures can be found in the ", a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england/2021-22", "'Pupil absence in schools in England: 2021/22'"), " release."),
                br(),
                column(
                  6,
                  p(strong("Overall absence (% of sessions)")),
                  plotlyOutput("overall_abs_plot") %>% withSpinner(),
                  br()
                ),
                column(
                  6,
                  p(strong("Persistent absentees (% of pupils)")),
                  plotlyOutput("Persistent_abs_plot") %>% withSpinner(),
                  br()
                )
              ),
              gov_row(
                column(width = 12, br()),
                h2("16-17 LA population"),
                # p("ONS estimate"),
                column(
                  6,
                  valueBoxOutput("NCCIS_pop", width = 12)
                ),
                # column(
                # 6,
                # valueBoxOutput("NCCIS_pop", width = 12)
                # )
              ),
              # ),
              uiOutput("contextual.bartext")
            )
          )
          # add box to show user input
        )
      )
    )
  )
}
