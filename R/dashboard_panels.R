homepage_panel <- function() {
  gov_row(
    h1("NEET and participation LA scorecard"),
    h2("Introduction"),
    p("The Department for Education (DfE) publishes an estimate each year
                of the proportion of young people not in education, employment or
                training (NEET).  However, evidence shows that there are a range of
                factors that can affect the proportion NEET, and this scorecard aims
                to put the headline figure into context by presenting it alongside a
                range of other related information."),
    p("There is a separate scorecard for each local authority in England
                except the City of London and Isles of Scilly. For these two local authorities, data is not available
                for all indicators and small numbers can result in large changes
                in percentages from one year to the next."),
    p("Each area's performance is compared with that of other local authorities in England.
            The best performing with the lowest NEET/highest participation fall into the
            green quintile, whereas local authorities with the highest NEET/lowest participation fall
            into the red quintile."),
    warning_text(
      inputId = "vatiation_warning",
      text = "Some caution should be taken if using these figures due to the estimates being based on
            management information. There is considerable variation at local authority level in
            how well 16 and 17 year olds are tracked and hence 'activity not known' proportions can impact on the
            estimates of the proportion NEET."
    ),
    # p(strong("The Department for Education's definitive measures for England
    # of participation and not in education, employment or training (NEET) for 16 to 18 year olds are
    # published annually in the national statistics release", a(
    # href = "https://explore-education-statistics.service.gov.uk/find-statistics/participation-in-education-and-training-and-employment",
    # "'Participation in Education, Training and Employment age 16 to 18'."
    # )))

    ## Left panel -------------------------------------------------------

    h2("Scorecard Contents"),
    h3("NEET and activity not known"),
    p("This section shows the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year."),
    p("The proportion not known indicator shows how effective the arrangements are in each area for tracking
                       young people's participation in education and training."),
    p("Whilst local authorities are responsible for tracking, they depend on schools, colleges and other partners
                       who work with young people sharing information with them. There is a risk that young people NEET have not been
                       identified in areas where effective tracking arrangements are not in place."),
    h3("Vulnerable groups NEET"),
    p("This section includes data showing the proportion of 16 and 17 year olds living in each area who were not in education,
                       employment or training (NEET) or their activity was not known (NK) at the end of the year with special education needs
                       and disability (SEND)/education, health and care plan (EHCP), SEN support or those that fall into the vulnerable group."),
    p("A young person is said to be in a vulnerable group if they have any of the following characteristics (taken from IC01 of
                   the NCCIS returns):"),
    tags$ul(
      tags$li("110 - Looked after / In care"),
      tags$li("130 - Refugee / Asylum seeker"),
      tags$li("140 - Carer-not own child"),
      tags$li("150 - Disclosed substance misuse"),
      tags$li("160 - Care leaver"),
      tags$li("170 - Supervised by YOT (Youth Offending Team)"),
      tags$li("190 - Parent-not caring for own child"),
      tags$li("200 - Alternative provision"),
      tags$li("210 - Mental health flag")
    ),
    warning_text(
      inputId = "vulnerablecaution",
      text = paste(
        "Caution should be used in interpreting these figures due to variation in local authority",
        "reporting of these characteristics. The proportion of the 16 and 17 year old cohort",
        "classified as being in the vulnerable group ranges from 0.0 percent to 10.7 percent. Due",
        "to this variation and likely inaccuracy in some local authorities in identifying the",
        "full vulnerable group cohort, NEET / not known rates may not be representative for the",
        "vulnerable group cohort. The underlying data accompanying this scorecard includes the",
        "proportion of each local authority's cohort identified as having one of the vulnerable",
        "characteristics (VG_cohort_percentage)."
      )
    ),
    h3("Participation"),
    p("This section shows the proportion of 16 and 17 year olds living in each area who were in education or training
                       at the end of March."),
    p("There is a breakdown of the three main routes that young people choose: full-time education, apprenticeship, and other education or training
                       (this includes part-time education, work based learning and employment with study towards a regulated qualification)."),
    p("It also shows the proportion of 16 and 17 year olds receiving an offer of a place in education and training under the September
                       Guarantee. The September Guarantee is a guarantee of an offer, made by the end of September, of an
                      appropriate place in post-16 education or training for every young person completing compulsory
                      education. This is particularly important as it helps young people make a seamless transition into
                      post-16 learning or employment with training. Local authorities are responsible for leading the 'September Guarantee' process, working with schools and colleges
                       across their area."),
    p("There are some young people who have not yet made a decision about what they want to do next, have other plans, or who cannot be
                       contacted. These young people are at risk of becoming NEET."),
    h3("Contextual - attainment and attendance"),
    p(
      "This section covers ",
      external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/2023-24", "post 16 attainment"), ", ",
      external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2023-24", "GCSE attainment"), " and ",
      external_link(href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england/2023-24", "school attendance"), " of young people living in each area.
                  It also provides the Client Caseload Information System (CCIS) population of 16 and 17 year olds in the local authority."
    ),
    p("Surveys show that higher attainment at age 16 is the factor most closely associated with participation and a lower
                       risk of becoming NEET between the ages of 16 and 18."),
    p("Young people who have poor attendance or who are excluded from schools are at greater risk of becoming NEET."),
    # br("Due to the coronavirus (COVID-19) pandemic, all summer 2020 and 2021 exams were cancelled and absence statistics were
    # notably different to previous years. Therefore the contextual information on absence and attainment has not been
    # included in the scorecard this year."),
    # tags$div(
    # title = "This section is useful if you want to understand how well different industries retain graduates.",
    # h3(actionLink("link_to_app_content_tab", "App Content"))
    # ),
  )
}


dashboard_panel <- function() {
  # Sidebar with a slider input for number of bins
  gov_row(
    h1(textOutput("data_description")),
    div(
      class = "well",
      style = "min-height: 100%; height: 100%; overflow-y: visible; background-color: #f3f2f1;",
      gov_row(
        width = 12,
        layout_columns(
          col_widths = c(6, 6, 12),
          card(
            style = "background-color: #f3f2f1;",
            selectInput("LA_choice",
              label = p(strong("Choose a local authority")),
              choices = levels(LA_options),
              selected = "Barking and Dagenham"
            ),
            shinyGovstyle::download_button(
              "pdfDownload",
              "Download summary report",
              file_type = "PDF",
              file_size = "173 KB"
            ),
            shinyGovstyle::download_button(
              "download_ud",
              "Download all LA data",
              file_type = "CSV",
              file_size = "58 KB"
            )
          ),
          card(
            style = "background-color: #f3f2f1;",
            radio_button_Input(
              inputId = "acc_colour_scheme",
              label = "Switch between accessible and hi-vis gauge chart colour schemes",
              selected = FALSE,
              choiceNames = c("Hi-vis", "Accessible"),
              choiceValues = c(FALSE, TRUE),
            ),
            tags$p("Different indicators can be viewed by switching between the tabs below.")
          ),
        )
      )
    ),
    br(),
    tabsetPanel(
      id = "tabsetpanel",
      tabPanel(
        value = "neet",
        title = "NEET and activity not known",
        # tags$b("16-17 year olds at end ", latest_year_end, "(average of December, January and February)"),
        h2("NEET and activity not known"),
        p(
          "Gauges below show where the LA rate sits within the",
          actionLink("link_to_tech_notes1", "quintile"), "range of all LAs and regional/England averages."
        ),
        h3("16-17 year olds at end ", latest_year_end, "(average of December, January and February)"),
        layout_columns(
          col_widths = c(2, 8, 2),
          card(),
          card(plotlyOutput("NEET_nk_gauge", width = "100%", inline = T) %>% withSpinner()),
          card()
        ),
        uiOutput("NEET_nk_vb"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(h2("NEET")),
            card_body(
              plotlyOutput("NEET_gauge", width = "100%") %>% withSpinner(),
              uiOutput("NEET", width = 12)
            )
          ),
          card(
            h2("Activity not known"),
            plotlyOutput("Nk_gauge", width = "100%") %>% withSpinner(),
            uiOutput("Not_known", width = 12)
          )
        )
      ),
      tabPanel(
        value = "vulnerable",
        title = "Vulnerable Groups NEET",
        h2("Vulnerable group"),
        h3("16-17 year olds NEET or activity not known at end ", latest_year_end, "(average of December, January and February)"),
        layout_columns(
          col_widths = c(6, 6),
          # tags$b("16-17 year olds NEET or activity not known at end ", latest_year_end, "(average of December, January and February)"),
          card(
            card_body(plotlyOutput("vulnerable_plot") %>% withSpinner())
          ),
          card(
            card_body(
              p("A young person is said to be in a vulnerable group if they have any of the following characteristics
                       (taken from IC01 of the NCCIS returns):"),
              tags$ul(
                tags$li("110 - Looked after/In care"),
                tags$li("130 - Refugee/Asylum seeker"),
                tags$li("140 - Carer-not own child"),
                tags$li("150 - Disclosed substance misuse"),
                tags$li("160 - Care leaver"),
                tags$li("170 - Supervised by YOT (Youth Offending Team)"),
                tags$li("190 - Parent-not caring for own child"),
                tags$li("200 - Alternative provision"),
                tags$li("210 - Mental health flag")
              )
            ),
            strong(textOutput("vg_cohort"), style = "color:#28A197")
          )
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_title(h2("No SEND")),
            card_body(plotlyOutput("No_SEN_plot") %>% withSpinner())
          ),
          card(
            card_title(h2("SEND (EHCP)")),
            card_body(plotlyOutput("EHCP_plot") %>% withSpinner())
          ),
          card(
            card_title(h2("SEN support")),
            card_body(plotlyOutput("SEN_support_plot") %>% withSpinner())
          )
        ),
        warning_text(
          inputId = "suppressed_warning",
          text = "Please note that if the local authority has suppressed data to avoid disclosure or their data is not available, no bar will appear on the plot."
        )
      ),
      tabPanel(
        value = "participation",
        title = "Participation",
        # tags$b("16-17 year olds March ", latest_year),
        h2("Participating in education and training"),
        p(
          "Gauges below show where the LA rate sits within the",
          actionLink("link_to_tech_notes2", "quintile"), "range of all LAs and regional/England averages."
        ),
        h3("16-17 year olds March ", latest_year),
        layout_columns(
          col_widths = c(6, 6),
          card(
            plotlyOutput("Participation_gauge", width = "92%") %>% withSpinner(),
            uiOutput("Participating", width = 12)
          ),
          card(
            p(strong(paste0("Type of education or training"))),
            plotlyOutput("participation_types") %>% withSpinner()
          )
        ),
        h2("September Guarantee: % offered an education or training place"),
        h3("16-17 year olds ", "September", last_year),
        layout_columns(
          col_widths = c(6),
          card(
            plotlyOutput("Sept_Guar_gauge", width = "92%") %>% withSpinner(),
            uiOutput("Sept_Guarantee", width = 12),
            p("In some instances, a local authority may have slightly over 100% offers made.
                    This is due to additional young people being added after the September Guarantee cohort is fixed.")
          )
        )
      ),
      tabPanel(
        value = "contextual",
        title = "Contextual - attainment and attendance",
        h2("Post 16 attainment"),
        p(
          "The following state-funded figures can be found in the ",
          external_link(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/level-2-and-3-attainment-by-young-people-aged-19/2023-24",
            "Level 2 and 3 attainment age 16 to 25, 2022/23 release"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_title(h3(strong("% 19 year olds achieving level 3"))),
            card_body(plotlyOutput("level3_plot") %>% withSpinner())
          ),
          card(
            card_title(h3("% 19 year olds achieving GCSE 9-4 standard pass in
                      English and maths (or equivalent) between ages 16 and 19,
                      for those who had not achieved this level by 16")),
            card_body(plotlyOutput("L2_EM_GCSE_plot") %>% withSpinner())
          )
        ),
        h2("GCSE attainment"),
        p(
          "The following state-funded figures can be found in the ",
          external_link(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/key-stage-4-performance/2023-24",
            "Key stage 4 performance, 2022/23 release"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_title(h3("Average attainment 8 score of all pupils")),
            card_body(plotlyOutput("Attainment8_plot") %>% withSpinner())
          ),
          card(
            card_title(h3("% 9-4 standard pass in English and maths GCSEs")),
            card_body(plotlyOutput("EM_pass_plot") %>% withSpinner())
          )
        ),
        h2("School attendance"),
        p(
          "The following state-funded secondary school attendance figures can be found in the ",
          external_link(
            href = "https://explore-education-statistics.service.gov.uk/find-statistics/pupil-absence-in-schools-in-england/2023-24",
            "Pupil absence in schools in England, 2022/23 release"
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_title(h3("Overall absence rate")),
            card_body(plotlyOutput("overall_abs_plot") %>% withSpinner())
          ),
          card(
            card_title(h3("Percentage of persistent absentees (10% or more missed)")),
            card_body(plotlyOutput("Persistent_abs_plot") %>% withSpinner())
          )
        ),
        h2("16-17 LA population"),
        # p("ONS estimate"),
        layout_columns(
          col_widths = c(6),
          card(
            uiOutput("NCCIS_pop", width = 12)
          )
          # column(
          # 6,
          # uiOutput("NCCIS_pop", width = 12)
          # )
        )
      ),
      # ),
      uiOutput("contextual.bartext")
    )
    # add box to show user input
  )
}
