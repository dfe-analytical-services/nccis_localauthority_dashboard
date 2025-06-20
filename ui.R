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

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#


#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The documentation for this GOVUK components can be found at:
#
#    https://github.com/moj-analytical-services/shinyGovstyle
#

# library(shinya11y)

ui <- function(input, output, session) {
  page_fluid(
    tags$head(
      HTML(paste0("<title>", site_title, "</title>"))
    ),
    tags$head(tags$link(rel = "shortcut icon", href = "dfefavicon.png")),
    tags$head(includeHTML(("google-analytics.html"))),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "dfe_shiny_gov_style.css"
      )
    ),
    tags$html(lang = "en"),
    shinyjs::useShinyjs(),
    dfeshiny::dfe_cookies_script(),
    dfeshiny::cookies_banner_ui(
      name = site_title
    ),
    dfeshiny::custom_disconnect_message(
      publication_name = ees_pub_name,
      publication_link = ees_publication
    ),
    dfeshiny::header(header = site_title),
    gov_main_layout(
      bslib::navset_hidden(
        id = "pages",
        bslib::nav_panel(
          "dashboard",
          ## Main dashboard ---------------------------------------------------
          layout_columns(
            # Override default wrapping breakpoints to avoid text overlap
            col_widths = breakpoints(sm = c(4, 8), md = c(3, 9), lg = c(2, 9)),
            ## Left navigation ------------------------------------------------
            dfe_contents_links(
              links_list = c(
                "Scorecards",
                "User guide"
              )
            ),
            ## Dashboard panels -----------------------------------------------
            bslib::navset_hidden(
              id = "left_nav",
              bslib::nav_panel(
                "scorecards",
                dashboard_panel()
              ),
              bslib::nav_panel(
                "user_guide",
                homepage_panel()
              )
            )
          )
        ),
        ## Footer pages -------------------------------------------------------
        bslib::nav_panel(
          "technical_notes",
          layout_columns(
            col_widths = c(-2, 8, -2),
            # Add in back link
            actionLink(class = "govuk-back-link", style = "margin: 0", "technical_notes_to_dashboard", "Back to dashboard"),
            technical_notes()
          )
        ),
        bslib::nav_panel(
          "support_and_feedback",
          layout_columns(
            col_widths = c(-2, 8, -2),
            # Add in back link
            actionLink(class = "govuk-back-link", style = "margin: 0", "support_to_dashboard", "Back to dashboard"),
            support_panel(
              team_email = "post16.statistics@education.gov.uk",
              publication_name = "Participation in education, training and NEET age 16 to 17 by local authority",
              publication_slug = "participation-in-education-training-and-neet-age-16-to-17-by-local-authority",
              repo_name = "https://github.com/dfe-analytical-services/nccis_localauthority_dashboard",
              form_url = "https://forms.office.com/Pages/ResponsePage.aspx?id=yXfS-grGoU2187O4s0qC-WPVbuM_P-hDu3_r8-AFqmhUOTRGUDFRRjE3U0xSNjJTSTJTUVFFOUtBRC4u"
            )
          )
        ),
        bslib::nav_panel(
          "accessibility_statement",
          layout_columns(
            col_widths = c(-2, 8, -2),
            # Add in back link
            actionLink(class = "govuk-back-link", style = "margin: 0", "accessibility_to_dashboard", "Back to dashboard"),
            a11y_panel()
          )
        ),
        bslib::nav_panel(
          "cookies_statement",
          layout_columns(
            col_widths = c(-2, 8, -2),
            # Add in back link
            actionLink(class = "govuk-back-link", style = "margin: 0", "cookies_to_dashboard", "Back to dashboard"),
            "Cookies",
            cookies_panel_ui(google_analytics_key = google_analytics_key)
          )
        )
      )
    ),
    shinyGovstyle::footer(
      links = c("Technical notes", "Support and feedback", "Accessibility statement", "Cookies statement"),
      full = TRUE
    )
  )
}
