# This file contains scripts which are intended to provide standard styling
# across dashboards. If you want to change anything in this script, please
# talk to the DfE Statistics Development team first.

valueBox <- function(value, subtitle, icon = NULL, color = "aqua", width = 4,
                     href = NULL) {
  validateColor(color)
  if (!is.null(icon)) tagAssert(icon, type = "i")

  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(value, id = "vboxhead"),
      p(subtitle, id = "vboxdetail")
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href)) {
    boxContent <- a(href = href, boxContent)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

validColors <- c("blue", "dark-blue", "green", "orange", "purple", "white")
gss_colour_palette <- c("#12436D", "#28A197", "#801650", "#F46A25") # ,'#3D3D3D','#A285D1')
acces_grad_palette <- c("#cedbcb", "#cdd0b7", "#ccbf9b", "#bb906f", "#8c301b")


validateColor <- function(color) {
  if (color %in% validColors) {
    return(TRUE)
  }

  stop(
    "Invalid color: ", color, ". Valid colors are: ",
    paste(validColors, collapse = ", "), "."
  )
}

icon_up_arrow_neet <- tags$i(
  class = "fa fa-arrow-up",
  style = paste("color:", acces_grad_palette[5])
)

icon_down_arrow_neet <- tags$i(
  class = "fa fa-arrow-down",
  style = paste("color:", gss_colour_palette[2])
)

icon_no_change <- tags$i(
  class = "fa fa-arrows-alt-h",
  style = paste("color:", acces_grad_palette[3])
)

icon_not_available <- tags$i(
  class = "",
  style = paste("color:", acces_grad_palette[3])
)

icon_up_arrow_part <- tags$i(
  class = "fa fa-arrow-up",
  style = paste("color:", gss_colour_palette[2])
)

icon_down_arrow_part <- tags$i(
  class = "fa fa-arrow-down",
  style = paste("color:", acces_grad_palette[5])
)

# icon_change_neet <- function(value) {
# if (value > 0) {
#    icon_up_arrow_neet
# }
# else if (value == 0) {
#   icon_no_change
# }
# else if (value == "z") {
#   icon_not_available
# } else {
#   icon_down_arrow_neet
#  }
# }

icon_change_neet <- function(value) {
  if (value == 0) {
    icon_no_change
  } else if (value == "z") {
    icon_not_available
  } else if (value > 0) {
    icon_up_arrow_neet
  } else {
    return(icon_down_arrow_neet)
  }
}

icon_change_part <- function(value) {
  if (value == 0) {
    icon_no_change
  } else if (value == "z") {
    icon_not_available
  } else if (value > 0) {
    icon_up_arrow_part
  } else {
    return(icon_down_arrow_part)
  }
}

customDisconnectMessage <- function(refresh = "Refresh page",
                                    links = sites_list,
                                    publication_name = ees_pub_name,
                                    publication_link = ees_publication,
                                    team_email = team_email) {
  checkmate::assert_string(refresh)
  htmltools::tagList(
    htmltools::tags$script(
      paste0(
        "$(function() {",
        "  $(document).on('shiny:disconnected', function(event) {",
        "    $('#custom-disconnect-dialog').show();",
        "    $('#ss-overlay').show();",
        "  })",
        "});"
      )
    ),
    htmltools::tags$div(
      id = "custom-disconnect-dialog",
      style = "display: none !important;",
      htmltools::tags$div(
        id = "ss-connect-refresh",
        tags$p("You've lost connection to the dashboard server - please try refreshing the page:"),
        tags$p(tags$a(
          id = "ss-reload-link",
          href = "#", "Refresh page",
          onclick = "window.location.reload(true);"
        )),
        if (length(links) > 1) {
          tags$p(
            "If this persists, you can also view the dashboard at one of our mirror sites:",
            tags$p(
              tags$a(href = links[1], "Site 1"),
              " - ",
              tags$a(href = links[2], "Site 2"),
              if (length(links) == 3) {
                "-"
              },
              if (length(links) == 3) {
                tags$a(href = links[3], "Site 3")
              }
            )
          )
        },
        if (!is.null(publication_name)) {
          tags$p(
            "All the data used in this dashboard can also be viewed or downloaded via the ",
            tags$a(
              href = publication_link,
              publication_name
            ),
            "on Explore Education Statistics."
          )
        },
        tags$p(
          "Please contact",
          tags$a(href = "mailto:post16.statistics@education.gov.uk", "post16.statistics@education.gov.uk"),
          "with details of any problems with this resource."
        )
        #  ),
        # htmltools::tags$p("If this persists, you can view tables and data via the ",htmltools::tags$a(href ='https://explore-education-statistics.service.gov.uk/find-statistics/pupil-attendance-in-schools', "Pupil attendance in schools")," release on Explore Education Statistics and please contact statistics.development@education.gov.uk with details of what you were trying to do.")
      )
    ),
    htmltools::tags$div(id = "ss-overlay", style = "display: none;"),
    htmltools::tags$head(htmltools::tags$style(
      glue::glue(
        .open = "{{", .close = "}}",
        "#custom-disconnect-dialog a {
             display: {{ if (refresh == '') 'none' else 'inline' }} !important;
             color: #1d70b8 !important;
             font-size: 16px !important;
             font-weight: normal !important;
          }"
      )
    ))
  )
}

