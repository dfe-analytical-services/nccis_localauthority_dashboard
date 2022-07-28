# ---------------------------------------------------------
# This is the ui file.
# Use it to call elements created in your server file into the app, and define where they are placed.
# Also use this file to define inputs.
#
# Every UI file should contain:
# - A title for the app
# - A call to a CSS file to define the styling
# - An accessibility statement
# - Contact information
#
# Other elements like charts, navigation bars etc. are completely up to you to decide what goes in.
# However, every element should meet accessibility requirements and user needs.
#
# This file uses a slider input, but other inputs are available like date selections, multiple choice dropdowns etc.
# Use the shiny cheatsheet to explore more options: https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Likewise, this template uses the navbar layout.
# We have used this as it meets accessibility requirements, but you are free to use another layout if it does too.
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------

ui <- function(input, output, session) {
  fluidPage(
    useShinyjs(),
    tags$head(includeHTML(("google-analytics.html"))),
    includeCSS("www/dfe_shiny_gov_style.css"),
    useShinydashboard(),
    # use_tota11y(), # accessibility layer for local testing

    # Set metadata for browser ==================================================

    #tags$html(lang = "en"),
    #meta_general(
     #  application_name = "NEET and not known scorecard",
     #  description = "NEET and not known scorecards by local authority in England",
     #  robots = "index,follow",
     # generator = "R-Shiny",
     #  subject = "NEET and not known scorecard",
     #  rating = "General",
     #  referrer = "no-referrer"
    # ),

    # Set title for search engines
    HTML("<title>NEET and not known scorecard</title>"),

    # Navbar ====================================================================

    # This CSS sets the 4th item on the navbar to the right
    tagList(
      tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(4) {
                           float: right;
                           }
                           ")))
    ),
    navbarPage("",
      id = "navbar",

      # Homepage tab ============================================================

      tabPanel(
        "Homepage",
        fluidPage(
          fluidRow(
            column(
              12,
              h1("NEET and not known scorecard"),
              h2("Introduction"),
              p("The Department for Education (DfE) publishes an estimate each year
                of the proportion of young people not in education, employment or 
                training (NEET).  However, evidence shows that there are a range of
                factors that can affect the proportion NEET, and this scorecard aims
                to put the headline figure into context by setting it alongside a 
                range of other related information."),
              br(),
              p("There is a separate scorecard for each local authority in England 
                except the City of London and Isles of Scilly as data are not available
                for all indicators and because small numbers can result in large changes
                in percentages from one year to the next."),
              br()
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
                    h3("NEET and tracking"),
                    br("This section shows the proportion of 16 and 17 year olds living in each area who were not in education, 
                       employment or training (NEET) or their activity was not known (NK) at the end of the year"),
                    br("The proportion not known indicator shows how effective the arrangements are in each area for tracking
                       young people's participation in education and training."),
                    br("Whilst local authorities are responsible for tracking, they depend on schools, colleges and other partners
                       who work with young people sharing information with them. There is a risk that young people NEET have not been
                       identified in areas where effective tracking arrangements are not in place."),
                    h3("LA support"),
                    br("This section shows the proportion of 16 and 17 year olds living in each area who were in education or training
                       at the end of March."),
                    br("There is a breakdown of the three main routes that young people choose: full-time education, apprenticeship, and other education or training
                       (this includes part-time education, work based learning and employment with study towards a regulated qualification."),
                    br("It also shows the proportion of 16 and 17 year olds receiving an offer of a place in education and training under the September 
                       Guarantee. Local authorities are responsible for leading the 'September Guarantee' process, working with schools and colleges
                       across their area."),
                    br("There are some young people who have not yet made a decision about what they want to do next, have other plans, or who cannot be 
                       contacted. These young people are at risk of becoming NEET."),
                    h3("Contextual information"),
                    br("This section covers outcomes, GCSE attainment and school attendance of young people living in each area."),
                    br("Surveys show that higher attainment at age 16 is the factor most closely associated with participation and a lower
                       risk of becoming NEET between the ages of 16 and 18."),
                    br("Young people who have poor attendance or who are excluded from schools are at greater risk of becoming NEET."),
                    br("Due to the coronavirus (COVID-19 pandemic all summer 2020 and 2021 exams were cancelled and absence statistics were
                       notably different to previous years. Therefore the contextual information on absence and attainment has not been 
                       included in the scorecard this year."),
                    h3("Vulnerable groups"),
                    br("This section includes the proportion of 16 and 17 year olds living in each area who were not in education, 
                       employment or training (NEET) or their activity was not known (NK) at the end of the year with special education needs
                       and disability (SEND), SEN support or those that fall into the vulnerable group."),
                    br(" A young person is said to be in a vulnerable group if they have any of the following characteristics 
                       (taken from IC01 of the NCCIS returns): Looked after/In care (110), Parent-caring for own child (120), Refugee/Asylum seeker (130),
                       Carer-not own child (140), Disclosed substance misue (150), Care leaver (160), Supervised by YOT (170), Alternative provision (200),
                       Mental health flag (210)"),
                    #tags$div(
                      #title = "This section is useful if you want to understand how well different industries retain graduates.",
                      #h3(actionLink("link_to_app_content_tab", "App Content"))
                    #),
                    br()
                  )
               )
              ),
            ),

            ## Right panel ------------------------------------------------------

           # column(
             # 6,
             # div(
             #  div(
               #   class = "panel panel-info",
              #    div(
               #     class = "panel-heading",
              #      style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
               #     h2("Background Info")
               #   ),
               #   div(
                #    class = "panel-body",
                 # )
                #)
              #)
            #)
          )
       )
      ),
      
      #scorecard----------------------------------------------------
      
      tabPanel(
        value = "la_scorecards",
        "LA scorecards",

        # Sidebar with a slider input for number of bins
        sidebarLayout(
          sidebarPanel(
            width = 2,
            p("View a local authority scorecard using the drop downs below."),
            p("Switch between different indicators using the tabs on the right."),
            selectInput("LA_choice",
                        label = p(strong("Choose a local authority")),
                        choices = c("LA1","LA2","England"),
                        selected = "England"
            )
          ),
          
          # Show a plot of the generated distribution
          
      # Create the main content-----------------
      mainPanel(
        width = 10,
        valueBoxOutput("box_info", width = 6),
        plotOutput("distPlot"),
        br()
        # add box to show user input
      )
      )
    ),

    # Create the tech notes-----------------
    tabPanel(
      value = "technical_notes",
      title = "Technical notes",
      h2("Technical notes"),
      br("Use this dashboard to view NEET and not known scorecards and contextual information for local authorities in England."),
      br("There is no scorecard for North Northamptonshire and West Northamptonshire as they are new local authorities, following the split from one local authority into two in April 2021. As these two new local authorities are not directly comparable with the pre LGR 2021 Northamptonshire, we were unable to produce complete figures for the majority of individual indictors included in the school places scorecard,
                           however the relevant data for these pre and post LGR 2021 local authorities are included in the England data and can be found in the summary data on explore education statistics."),
      br("Young people are measured according to their academic age; ie their age on 31 August."),
      br("The cohort does not include young adult offenders in custody."),
      br("For further information on participation in education, training and NEET age 16 to 17 by local authority figures please see the methodology document here INSERT LINK."),
      h3("Scorecard content from 2017"),
      br("Changes by DfE from 1 September 2016 reduced the amount of information that local
                    authorities are mandated to collect and record in their Client Caseload Information 
                    Systems and submit to the DfE in monthly extracts. Local authorities are only required
                    to submit extracts of information about young people of academic age 16 and 17, and were
                    no longer required to collect and record information about young people beyond the end of
                    the academic year in which they reach their 18th birthday. As such from 2017 the scorecard
                    reflects information about 16 and 17 year olds only."),
      br("The headline measure in the scorecard combines NEET and not known figures for 16 and 17 year olds. This gives
                       an accurate picture of local authority performance in terms of tracking and supporting 
                       young people and means that low NEET figures cannot be masked by high levels of ‘not knowns’."),
      h3("Quintiles"),
      br("Each area's performance is compared with that of other local authorities in England.  The 1st quintile is always the indicator
      of best performance. Therefore, the 30 local authorities that have the lowest levels of 16-17 year olds NEET and not knowns will fall
      into the first quintile; the next 30 will fall into the second quintile etc.  The other indicators are reported in a similar way but
      those with the highest levels will fall into the first quintile.    Comparisons between local authorities are made using data that has
      not been rounded."),  
      br("The average England percentage may not necessarily fall in the middle of Quintile 3 because the average is calculated by averaging
      all the values; for example if some of the values are particularly large compared with the other values, the average will be larger than
      the middle ranked LA and may fall outside the middle quintile."),
      br("LAs with no value are excluded from the quintile calculation.  Therefore if five LAs have missing data there will be 29 LAs in each
      quintile rather than 30."),
      br(),
      h3("LA direction"),
      br("Each area's performance is compared with the same period of the previous year.  A green arrow denotes those areas whose performance has improved,
      a red arrow where performance is lower and a black arrow where performance has stayed the same compared to the previous year.  Year-on-year comparisons,
      where available,  have been made using data that has not been rounded."),
      br(),
      h3("CCIS"),
      br("Local authorities track young people's participation in education or training using information provided by schools and colleges.  They record this on a local database which is known as the
         Client Caseload Information System (CCIS).  An extract from CCIS is sent to DfE and used to produce statistics about young people's participation which can be found on explore education statistics."),
      br(),
      ),
           # Create the accessibility statement-----------------
      tabPanel(
        "Accessibility",
        h2("Accessibility statement"),
        br("This accessibility statement applies to the **application name**.
            This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind."),
        h3("WCAG 2.1 compliance"),
        br("We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements. ", onclick = "ga('send', 'event', 'click', 'link', 'IKnow', 1)"), "This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool "), ", which did not detect accessibility issues.
             This application also fully passes the accessibility audits checked by the ", a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool"), ". This means that this application:"),
        tags$div(tags$ul(
          tags$li("uses colours that have sufficient contrast"),
          tags$li("allows you to zoom in up to 300% without the text spilling off the screen"),
          tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users")
        )),
        h3("Limitations"),
        br("We recognise that there are still potential issues with accessibility in this application, but we will continue
             to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
            are known issues that we will continue to monitor and improve:"),
        tags$div(tags$ul(
          tags$li("List"),
          tags$li("known"),
          tags$li("limitations, e.g."),
          tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format)")
        )),
        h3("Feedback"),
        br(
          "If you have any feedback on how we could further improve the accessibility of this application, please contact us at",
          a(href = "mailto:email@education.gov.uk", "email@education.gov.uk")
        )
      ), # End of accessibility tab
      # Support links ===========================================================

      tabPanel(
        "Support and feedback",
        support_links() # defined in R/supporting_links.R
      ),
      # Footer ====================================================================

      shinyGovstyle::footer(TRUE)
    ) # End of navBarPage
  )
}
