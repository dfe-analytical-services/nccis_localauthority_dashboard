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
      # add subtitle to explain what it's hsowing
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
  
  #to finalise once we've got the data in - tweaked school places code for now
  #output$NEET_nk <- renderValueBox({
    
    # Take filtered data, search for NEET/nk rate, pull the value and tidy the number up
  # NEET_nk_perc <- live_scorecard_data() %>%
  #    filter(name == "QuanRP") %>%
   #   pull(value)
    
    # Put value into box to plug into app
 #   shinydashboard::valueBox(
  #    paste0(scales::comma(NEET_nk_perc)),
    #  paste0("% 16-17 year olds NEET or whose activity is not known", plan_year),
      # icon = icon("fas fa-signal"),
    #  color = "blue"
   # )
 # })
  
  
  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}

