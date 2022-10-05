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

  # Define font family for charts
  font_choice <- list(
    family = "Arial",
    size = 14
  )
  
  #Filters
  lineLA <- reactive({
    la_ud %>% filter(la_name == input$LA_choice)
  })
  
  England <- reactive({
    la_ud %>% filter(geographic_level == "National")
  })
  
  
  #Participation type data
  
  # reshape the data so it plots neatly!
  participation_data_fte <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name,la_name,Full_time_education_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level=="National" ~ "England",
      geographic_level=="Regional" ~ region_name,
      TRUE ~ la_name
    ),participation_type="FTE")
  
  colnames(participation_data_fte)[colnames(participation_data_fte) == "Full_time_education_percent"] <- "value"
  
  participation_data_fte <- participation_data_fte %>%
    select(la_name,participation_type, value)
  
  participation_data_app <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name,la_name,Apprenticeship_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level=="National" ~ "England",
      geographic_level=="Regional" ~ region_name,
      TRUE ~ la_name
    ),participation_type="Apprenticeship")
  
  colnames(participation_data_app)[colnames(participation_data_app) == "Apprenticeship_percent"] <- "value"
  
  participation_data_app <- participation_data_app %>%
    select(la_name,participation_type, value)
  
  participation_data_other <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name,la_name,Other_education_and_training_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level=="National" ~ "England",
      geographic_level=="Regional" ~ region_name,
      TRUE ~ la_name
    ),participation_type="Other")
  
  colnames(participation_data_other)[colnames(participation_data_other) == "Other_education_and_training_percent"] <- "value"
  
  participation_data_other <- participation_data_other %>%
    select(la_name,participation_type, value)
  
  #pull the types together into one file
  participation_data <- bind_rows(participation_data_fte,participation_data_app,participation_data_other) 
  
  participation_data <-  participation_data %>%
    mutate(value =as.numeric(value))
  
  
  partLA <- reactive({
    participation_data %>% filter(la_name == input$LA_choice)
  })
  
  partEng <- reactive({
    participation_data %>% filter(la_name == "England")
  })
  
  #partRegion <- reactive({
   # participation_data %>% filter(la_name == Regionname)
 # })
  
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


  # NEET and not known tab-----------
 
  ##NEET/NK----------------------------
  ###Guage chart-----------------------
  
  output$NEET_nk_guage <- renderPlotly({
    plot_ly(
    domain = list(x = c(0, 2), y = c(0, 2)),
    value = lineLA() %>% pull(as.numeric(NEET_not_known_percent)), 
    number = list(suffix = "%"),
    title = list(text = "NEET or not known", font =list(size=32)),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(range = list(1.4, 13.8), tickwidth = 1, tickcolor = "darkblue",tickvals=list(1.4,3.6,4.5,5.4,6.7,13.8)), #need to make this to the max % neet/nk
      bar = list(color = "darkblue"),
      bgcolor = "white",
      borderwidth = 1,
      #bordercolor = "gray",
      steps = list(
        list(range = c(1.4, 3.6), color = "limegreen"), #need to make these the quintile boundaries
        list(range = c(3.6, 4.5), color = "yellowgreen"),
        list(range = c(4.5, 5.4), color = "yellow"),
        list(range = c(5.4, 6.7), color = "gold"),
        list(range = c(6.7, 13.8), color = "red")
        ),
      threshold = list(
        line = list(color = "black", width = 4),
        displayvalue = "England",
        thickness = 1,
        value = England() %>% pull(round(as.numeric(NEET_not_known_percent),1)))
    ))
  })
  
  ###Annual change and national,regional comparison box-------
  
  output$NEET_nk <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    NEET_nk_perc <- lineLA() %>%
      pull(as.numeric(NEET_not_known_percent))
    
    NEET_nk_change <- lineLA() %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    
    NEET_nk_perc_Eng <- England() %>%
      pull(as.numeric(NEET_not_known_percent))
    
    NEET_nk_change_Eng <- England() %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    NEET_nk_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(NEET_not_known_percent))
    
    NEET_nk_change_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(input$LA_choice, ": ",NEET_nk_perc, "%, ", change_ed(NEET_nk_change), NEET_nk_change, " ppts"),
      paste0("England: ", NEET_nk_perc_Eng, "%, ", change_ed(NEET_nk_change_Eng), NEET_nk_change_Eng, " ppts.",
              Regionname, ": ", NEET_nk_perc_region, "%, ", change_ed(NEET_nk_change_region), NEET_nk_change_region, " ppts.
              Annual changes are since end ", previous_year_end, ")."),
      color = "blue"
    )
  })
  
  
  ## NEET-------------------------
  
  ###Guage chart-----------------
  
  output$NEET_guage <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 2), y = c(0, 2)),
      value = lineLA() %>% pull(as.numeric(NEET_percent)), 
      number = list(suffix = "%"),
      title = list(text = "NEET", font =list(size=24)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0.8, 6.8), tickwidth = 1, tickcolor = "darkblue",tickvals=list(0.8,1.8,2.3,3.1,3.9,6.8)), #need to make this to the max % neet/nk
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 1,
        #bordercolor = "gray",
        steps = list(
          list(range = c(0.8, 1.8), color = "limegreen"), #need to make these the quintile boundaries
          list(range = c(1.8, 2.3), color = "yellowgreen"),
          list(range = c(2.3, 3.1), color = "yellow"),
          list(range = c(3.1, 3.9), color = "gold"),
          list(range = c(3.9, 6.8), color = "red")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = England() %>% pull(round(as.numeric(NEET_percent),1))) 
      ))
  })

  ###Annual change and national,regional comparison box-------
  
  output$NEET <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    NEET_perc <- lineLA() %>%
      pull(as.numeric(NEET_percent))
    
    NEET_change <- lineLA() %>%
      pull(as.numeric(annualchange_NEET))
    
    NEET_perc_Eng <- England() %>%
      pull(as.numeric(NEET_percent))
    
    NEET_change_Eng <- England() %>%
      pull(as.numeric(annualchange_NEET))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    NEET_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(NEET_percent))
    
    NEET_change_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(annualchange_NEET))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(NEET_perc, "%, ", change_ed(NEET_change), NEET_change, " ppts"),
      paste0("England: ", NEET_perc_Eng, "%, ", change_ed(NEET_change_Eng), NEET_change_Eng, " ppts.     ",
             Regionname, ": ", NEET_perc_region, "%, ", change_ed(NEET_change_region), NEET_change_region, " ppts.
             (Annual changes are since end ", previous_year_end, ")."),
      color = "blue"
    )
  })
  
  
  ##Not known---------------------------

  ###Guage chart--------------------
  
  output$Nk_guage <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 2), y = c(0, 2)),
      value = lineLA() %>% pull(as.numeric(Notknown_percent)), 
      number = list(suffix = "%"),
      title = list(text = "Not known", font =list(size=24)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(0.0, 12.1), tickwidth = 1, tickcolor = "darkblue",tickvals=list(0.0,0.9,1.4,2.1,3.2,12.1)), #need to make this to the max % neet/nk
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 1,
        #bordercolor = "gray",
        steps = list(
          list(range = c(0.0, 0.9), color = "limegreen"), #need to make these the quintile boundaries
          list(range = c(0.9, 1.4), color = "yellowgreen"),
          list(range = c(1.4, 2.1), color = "yellow"),
          list(range = c(2.1, 3.2), color = "gold"),
          list(range = c(3.2, 12.1), color = "red")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = England() %>% pull(round(as.numeric(Notknown_percent),1)))
      ))
  })
  
  ###Annual change and national,regional comparison box-------
  
  output$Not_known <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    Nk_perc <- lineLA() %>%
      pull(as.numeric(Notknown_percent))
    
    Nk_change <- lineLA() %>%
      pull(as.numeric(annualchange_notknown))
    
    Nk_perc_Eng <- England() %>%
      pull(as.numeric(Notknown_percent))
    
    Nk_change_Eng <- England() %>%
      pull(as.numeric(annualchange_notknown))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    Nk_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(Notknown_percent))
    
    Nk_change_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(annualchange_notknown))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Nk_perc, "%, ", change_ed(Nk_change), Nk_change, " ppts"),
      paste0("England: ", Nk_perc_Eng, "%, ", change_ed(Nk_change_Eng), Nk_change_Eng, " ppts.     ",
             Regionname, ": ", Nk_perc_region, "%, ", change_ed(Nk_change_region), Nk_change_region, " ppts.
             (Annual changes are since end ", previous_year_end, ")."),
      color = "blue"
    )
  })
  
  #Vulnerable groups NEET tab---------------------------------
  ##Vulnerable group--------------------------------
  
  ###Guage chart--------------------
  #not doing this year
  #output$Vulnerable_guage <- renderPlotly({
   # plot_ly(
     # domain = list(x = c(0, 2), y = c(0, 2)),
     # value = lineLA() %>% pull(as.numeric(VG_NEET_NK_percentage)), 
     # number = list(suffix = "%"),
     # title = list(text = "NEET or not known", font =list(size=24)),
     # type = "indicator",
      #mode = "gauge+number",
      #gauge = list(
      #  axis = list(range = list(0.0, 12.1), tickwidth = 1, tickcolor = "darkblue",tickvals=list(0.0,0.9,1.4,2.1,3.2,12.1)), #need to make this to the max % neet/nk
      #  bar = list(color = "darkblue"),
      #  bgcolor = "white",
       # borderwidth = 1,
        #bordercolor = "gray",
       # steps = list(
       #   list(range = c(0.0, 0.9), color = "limegreen"), #need to make these the quintile boundaries
       #   list(range = c(0.9, 1.4), color = "yellowgreen"),
       #   list(range = c(1.4, 2.1), color = "yellow"),
       #   list(range = c(2.1, 3.2), color = "gold"),
        #  list(range = c(3.2, 12.1), color = "red")
       # ),
       # threshold = list(
        #  line = list(color = "black", width = 4),
        #  thickness = 1,
       #   value = England() %>% pull(round(as.numeric(VG_NEET_NK_percentage),1)))
      #))
  #})
  
  ###Value box, National,regional comparison-------
  
  output$Vulnerable <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    Vul_perc <- lineLA() %>%
      pull(as.numeric(VG_NEET_NK_percentage))
    
    #Vul_cohort <- lineLA() %>%
      #pull(as.numeric(VG_cohort_DJF_avg))
    
    Vul_cohort_perc <- lineLA() %>%
      pull(as.numeric(VG_cohort_percentage))
    
    Vul_perc_Eng <- England() %>%
      pull(as.numeric(VG_NEET_NK_percentage))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    Vul_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(VG_NEET_NK_percentage))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Vul_perc, "%"),
      paste0("(",  Vul_cohort_perc, "% of LA in the vulnerable group). England: ", Vul_perc_Eng,
             "%. ", Regionname, ": ", Vul_perc_region, "%. "),
      color = "blue"
    )
  })
  
  ##EHCP--------------------------------------------
  ###Value box, National,regional comparison-------
  
  output$EHCP <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    EHCP_perc <- lineLA() %>%
      pull(as.numeric(NEET_NK_EHCP_percent))
    
    EHCP_cohort_perc <- lineLA() %>%
      pull(as.numeric(cohort_EHCP_percent))
    
    EHCP_perc_Eng <- England() %>%
      pull(as.numeric(NEET_NK_EHCP_percent))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    EHCP_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(NEET_NK_EHCP_percent))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(EHCP_perc, "%"),
      paste0("(",  EHCP_cohort_perc, "% of LA with EHCP). England: ", EHCP_perc_Eng,
             "%. ", Regionname, ": ", EHCP_perc_region, "%. "),
      color = "blue"
    )
  })
  
  ##SEN support---------------------------------------
  ###Value box, National,regional comparison-------
  
  output$SEN_support <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    SEN_support_perc <- lineLA() %>%
      pull(as.numeric(NEET_NK_SENDsupport_percent))
    
    SEN_support_cohort_perc <- lineLA() %>%
      pull(as.numeric(cohort_SENDsupport_percent))
    
    SEN_support_perc_Eng <- England() %>%
      pull(as.numeric(NEET_NK_SENDsupport_percent))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    SEN_support_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(NEET_NK_SENDsupport_percent))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(SEN_support_perc, "%"),
      paste0("(",  SEN_support_cohort_perc, "% of LA with SEN support). England: ", SEN_support_perc_Eng, 
             "%. ", Regionname, ": ", SEN_support_perc_region, "%. "),
      color = "blue"
    )
  })
  
  ##No SEN-----------------------------------------
  ###Value box, National,regional comparison-------
  
  output$No_SEN <- renderValueBox({
    
    # Take filtered data, search for rate, pull the value and tidy the number up
    No_SEN_perc <- lineLA() %>%
      pull(as.numeric(NEET_NK_noSEN_percent))
    
    No_SEN_cohort_perc <- lineLA() %>%
      pull(as.numeric(cohort_noSEN_percent))
    
    No_SEN_perc_Eng <- England() %>%
      pull(as.numeric(NEET_NK_noSEN_percent))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    No_SEN_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(NEET_NK_noSEN_percent))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(No_SEN_perc, "%"),
      paste0("(",  No_SEN_cohort_perc, "% of LA with SEN support). England: ", No_SEN_perc_Eng,
             "%. ", Regionname, ": ", No_SEN_perc_region, "%. "),
      color = "blue"
    )
  })
  
  # Participating in education and training------------------
  
  ##Total participating-----------
  
  ###Gauge chart--------------------
  
  output$Participation_guage <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 2), y = c(0, 2)),
      value = lineLA() %>% pull(as.numeric(TOTAL_participating_in_education_and_training_percent)), 
      number = list(suffix = "%"),
      #title = list(text = "Participating in education and training", font =list(size=18)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(87.4, 98.5), tickwidth = 1, tickcolor = "darkblue",tickvals=list(87.4,91.6,92.7,93.9,95.5,98.5)), #need to make this to the max % neet/nk
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 1,
        #bordercolor = "gray",
        steps = list(
          list(range = c(87.4, 91.6), color = "red"), #need to make these the quintile boundaries
          list(range = c(91.6, 92.7), color = "gold"),
          list(range = c(92.7, 93.9), color = "yellow"),
          list(range = c(93.9, 95.5), color = "yellowgreen"),
          list(range = c(95.5, 98.5), color = "limegreen")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = England() %>% pull(round(as.numeric(TOTAL_participating_in_education_and_training_percent),1)))
      ))
  })
  
  ###value box--------------------------------
  output$Participating <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    participating_perc <- lineLA() %>%
      pull(as.numeric(TOTAL_participating_in_education_and_training_percent))
    
    participating_change <- lineLA() %>%
      pull(as.numeric(annual_change_Participation_in_education_training))
    
    participating_perc_Eng <- England() %>%
      pull(as.numeric(TOTAL_participating_in_education_and_training_percent))
    
    participating_change_Eng <- England() %>%
      pull(as.numeric(annual_change_Participation_in_education_training))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    participating_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(TOTAL_participating_in_education_and_training_percent))
    
    participating_change_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(annual_change_Participation_in_education_training))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(participating_perc, "%, ", change_ed(participating_change), participating_change, " ppts"),
      paste0("England: ", participating_perc_Eng, "%, ", change_ed(participating_change_Eng), participating_change_Eng, " ppts.",
             Regionname, ": ", participating_perc_region, "%, ", change_ed(participating_change_region), participating_change_region, " ppts.
              (Annual changes are since March ", last_year, ")."),
      color = "blue"
    )
  })
    
  ##Participation type breakdown plot----------------------------
  # Stacked bar instead of pie here for preference?
  # Easier for users to interpret

  ##from LA place scorecard code    
    output$participation_types <- renderPlotly({
      
      Regionname <- lineLA() %>%
        pull(region_name)
      
      partRegion <- participation_data %>% filter(la_name == Regionname)
      
      participation_types <- bind_rows(partLA(), partRegion, partEng()) %>%
        ggplot(aes(
          y = value, x = "",
          fill = participation_type,
          text = paste(participation_type, ": ", value, "%")
        )) +
        geom_bar(stat= "identity", na.rm=TRUE) +
        #position =position_fill(reverse = TRUE)
        coord_flip() +
        facet_wrap(~la_name, nrow = 3) +
        #geom_text(aes(label = paste0(value, "%")), colour = "#ffffff", size = 4, position = position_fill(reverse = TRUE, vjust = 0.5)) +
        labs(x = "", y = "") +
        guides(fill = guide_legend(title = "")) +
        scale_fill_manual(values = c("#FFB90F", "#3182bd", "#8B8878")) +
        scale_y_continuous(limits=c(0,100)) +
        theme_minimal() +
        labs(x="", y="%") +
        theme(
          legend.position = "top",
          text = element_text(size = 14, family = "Arial"),
          strip.text.x = element_text(size = 16),
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
      
      
      ggplotly(participation_types,
               tooltip = c("text")
      ) %>%
        layout(
          uniformtext = list(minsize = 12, mode = "hide"),
          #xaxis = list(showticklabels = FALSE),
          legend = list(
            orientation = "h",
            y = -0.3, x = 0.33,
            font = font_choice
          ),
          title = list(
            text = "Type of education or training",
            font = list(color = "#ffffff")
          )
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    
  ## September Guarantee------------------------------------
  ###Gauge chart--------------------
  
  output$Sept_Guar_guage <- renderPlotly({
    plot_ly(
      domain = list(x = c(0, 2), y = c(0, 2)),
      value = lineLA() %>% pull(as.numeric(September_guarantee_Offer_made_percent)), 
      number = list(suffix = "%"),
      #title = list(text = "September Guarantee: % offered an education place", font =list(size=18)),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = list(50.8, 99.8), tickwidth = 1, tickcolor = "darkblue",tickvals=list(50.8,93.2,95.1,96.7,97.8,99.8)), #need to make this to the max % neet/nk
        bar = list(color = "darkblue"),
        bgcolor = "white",
        borderwidth = 1,
        #bordercolor = "gray",
        steps = list(
          list(range = c(50.8, 93.2), color = "red"), #need to make these the quintile boundaries
          list(range = c(93.2, 95.1), color = "gold"),
          list(range = c(95.1, 96.7), color = "yellow"),
          list(range = c(96.7, 97.8), color = "yellowgreen"),
          list(range = c(97.8, 99.8), color = "limegreen")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 1,
          value = England() %>% pull(round(as.numeric(September_guarantee_Offer_made_percent),1)))
      ))
  })
  
  
  ###Value box-------------------------------------
  output$Sept_Guarantee <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    Sept_Guar_perc <- lineLA() %>%
      pull(as.numeric(September_guarantee_Offer_made_percent))

    Sept_Guar_change <- lineLA() %>%
      pull(as.numeric(September_guarantee_annual_change_ppts))
    
    Sept_Guar_perc_Eng <- England() %>%
      pull(as.numeric(September_guarantee_Offer_made_percent))
    
    Sept_Guar_change_Eng <- England() %>%
      pull(as.numeric(September_guarantee_annual_change_ppts))
    
    Regionname <- lineLA() %>%
      pull(region_name)
    
    Sept_Guar_perc_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(September_guarantee_Offer_made_percent))
    
    Sept_Guar_change_region <- filter(la_ud, geographic_level=="Regional", region_name==Regionname) %>%
      pull(as.numeric(September_guarantee_annual_change_ppts))
    
    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Sept_Guar_perc, "%, ", change_ed(Sept_Guar_change), Sept_Guar_change, " ppts"),
      paste0("England: ", Sept_Guar_perc_Eng, "%, ", change_ed(Sept_Guar_change_Eng), Sept_Guar_change_Eng, " ppts.",
             Regionname, ": ", Sept_Guar_perc_region, "%, ", change_ed(Sept_Guar_change_region), Sept_Guar_change_region, " ppts.
              (Annual changes are since March ", last_year, ")."),
      color = "blue"
    )
  })

  # Contextual information--------------------------------------
  # Outcomes - level 3
  output$level3 <- renderValueBox({

    # Take filtered data, search for rate, pull the value and tidy the number up
    level3_percent <- lineLA() %>%
      pull(as.numeric(Level_3))

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
    gcse_percent <- lineLA() %>%
      pull(as.numeric(L2_em_GCSE_othL2))

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
    overall_abs_percent <- lineLA() %>%
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
    persistent_abs_percent <- lineLA() %>%
      pull(as.numeric(sess_overall_percent_pa_10_exact))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(persistent_abs_percent, "%"),
      paste0("Persistent absentees (% of pupils)"),
      # icon = icon("fas fa-signal"),
      color = "blue"
    )
  })

 
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
