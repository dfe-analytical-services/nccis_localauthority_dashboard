# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

  # Simple server stuff goes here ------------------------------------------------------------


  # Define server logic required to draw a histogram
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })

  # Define server logic to create a box

  output$box_info <- renderValueBox({

    # Put value into box to plug into app
    shinydashboard::valueBox(
      # take input number
      input$bins,
      # add subtitle to explain what it's showing
      paste0("Number that user has inputted"),
      color = "blue"
    )
  })

  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navbar", selected = "app_content")
  })



  # Top lines -------------------------
  ## create header on the scorecard so users know which LA it is showing

  output$data_description <- renderText({
    paste0("Data for ", input$LA_choice)
  })


  # NEET and not known rates-----------------
  # NEET and not known
  output$NEET_nk <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    NEET_nk_perc <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(NEET_not_known_perc))

    NEET_nk_change <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(NEET_nk_perc, "%"),
      paste0(
        "16-17 year olds NEET or whose activity is not known, end ", last_year,
        ", ", NEET_nk_change, " percentage point change since last year."
      ),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })
  
  #guage chart with annual change
  
  output$NEET_nk_guage <- renderPlot({
    plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = 1, #this needs to be the annual changes in NEET/nk for selected LA
    title = list(text = "NEET or not known"),
    type = "indicator",
    mode = "gauge+number")
  })
  
  #output$NEET_nk_guage <- NEET_nk_guage %>%
   # layout(margin = list(l=20,r=30))

  # NEET
  output$NEET <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    NEET_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(NEET_perc))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(NEET_percent, "%"),
      paste0("16-17 year olds NEET, end ", last_year),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # Not known
  output$Not_known <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    Not_known_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(Notknown_perc))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Not_known_percent, "%"),
      paste0("16-17 year olds whose activity is not known, end ", last_year),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # LA support -----------------
  # Participating in education and training
  output$Participating <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    participating_perc <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(total_participating_in_education_training_perc))

    fte_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(full_time_education_perc))

    Apprenticeship_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(apprenticeship_perc))

    Other_ed_tr_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(other_education_training_perc))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(participating_perc, "%"),
      paste0(
        "16-17 year olds participating in education and training, March ", latest_year,
        ". Of which ", fte_percent, "% in full-time education, ",
        Apprenticeship_percent, "% on an apprenticeship and ",
        Other_ed_tr_percent, "% in other education and training."
      ),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # September Guarantee
  output$Sept_Guarantee <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    Sept_Guar_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(september_guarantee_offer_made_perc))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Sept_Guar_percent, "%"),
      paste0("16-17 year olds made offer of an education place under September Guarantee ", last_year),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # Contextual information--------------------------------------
  # Outcomes - level 3
  output$level3 <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    level3_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(level_3))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(level3_percent, "%"),
      paste0("19 year olds achieving level 3 ", last_year),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # Outcomes - GCSE
  output$GCSE <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    gcse_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(l2_em_gcse_othL2))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(gcse_percent, "%"),
      paste0("19 year olds achieving GCSE 9-4 standard pass in
             English and maths (or equivalent) between ages 16
             and 19, for those who had not achieved this level by 16 ", last_year),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # School attendance - overall
  output$Overall_abs <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    overall_abs_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(sess_overall_percent))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(overall_abs_percent, "%"),
      paste0("Overall absence (% of sessions)"),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # School attendance - persistent
  output$Persistent_abs <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    persistent_abs_percent <- filter(la_ud, la_name == input$LA_choice) %>%
      pull(as.numeric(sess_overall_percent_pa_10_exact))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(persistent_abs_percent, "%"),
      paste0("Persistent absentees (% of pupils)"),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

  # calculating quintiles
  # https://stackoverflow.com/questions/39693957/calculating-quintile-based-scores-on-r

  # quintile = function(y) {
  #  x = df$val[df$y == y]
  #  qn = quantile(x, probs = (0:5)/5)
  #  result = as.numeric(cut(x, qn, include.lowest = T))
  # }
  
  # Files for download ------------------------------------------------------
  
  #all LAs
  output$download_ud <- downloadHandler(
    filename = function() {
      paste("la_underlying_data",".csv", sep = "")
      },
    content = function(file) {
      write.csv(la_ud, file, row.names = FALSE)
    }
  )
 
 #selected LA pdf
 
  function(input, output, session) {
    # Define font family for charts
    font_choice <- list(
      family = "Arial",
      size = 14
    )
    
    output$pdfDownload <- downloadHandler(
      filename = paste0("dashboard_output.pdf"),
      content = function(file) {
        # Add a loading modal, can probably make this prettier at a later date
        showModal(modalDialog("Preparing PDF report...", footer = NULL))
        on.exit(removeModal())
        
        # List of parameters to pass from shiny to the report
        params <- input_la_choice = input$LA_choice
         
        
        # Render the pdf file from the rmarkdown template
        rmarkdown::render("Summary_scorecard.Rmd",
                          output_file = file,
                          params = params,
                          output_format = "pdf_document",
                          envir = new.env(parent = globalenv())
        )
      }
    )
  }

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
