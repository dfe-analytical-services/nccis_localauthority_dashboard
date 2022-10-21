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

  # Filters
  lineLA <- reactive({
    la_ud %>% filter(la_name == input$LA_choice)
  })

  England <- reactive({
    la_ud %>% filter(geographic_level == "National")
  })


  # Reshaping data for plots-----------------------------------------
  ## Participation type data-----------------------------------------

  # reshape the data so it plots neatly!
  participation_data_fte <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name, la_name, Full_time_education_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level == "National" ~ "England",
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    ), participation_type = "FTE")

  colnames(participation_data_fte)[colnames(participation_data_fte) == "Full_time_education_percent"] <- "value"

  participation_data_fte <- participation_data_fte %>%
    select(la_name, participation_type, value)

  participation_data_app <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name, la_name, Apprenticeship_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level == "National" ~ "England",
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    ), participation_type = "Apprenticeship")

  colnames(participation_data_app)[colnames(participation_data_app) == "Apprenticeship_percent"] <- "value"

  participation_data_app <- participation_data_app %>%
    select(la_name, participation_type, value)

  participation_data_other <- la_ud %>%
    # select only participation types
    select(geographic_level, region_name, la_name, Other_education_and_training_percent) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level == "National" ~ "England",
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    ), participation_type = "Other")

  colnames(participation_data_other)[colnames(participation_data_other) == "Other_education_and_training_percent"] <- "value"

  participation_data_other <- participation_data_other %>%
    select(la_name, participation_type, value)

  # pull the types together into one file
  participation_data <- bind_rows(participation_data_fte, participation_data_app, participation_data_other)

  participation_data <- participation_data %>%
    mutate(value = as.numeric(value))


  partLA <- reactive({
    participation_data %>% filter(la_name == input$LA_choice)
  })

  partEng <- reactive({
    participation_data %>% filter(la_name == "England")
  })

  ## Vulnerable groups data---------------------------------------------
  # reshape the data so it plots neatly!
  vulnerable_data <- la_ud %>%
    # select only vulnerable info
    select(geographic_level, region_name, la_name, NEET_NK_noSEN_percent, NEET_NK_EHCP_percent, NEET_NK_SENDsupport_percent, VG_NEET_NK_percentage) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level == "National" ~ "England",
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    ))


  vulnerable_data <- vulnerable_data %>%
    select(la_name, NEET_NK_noSEN_percent, NEET_NK_EHCP_percent, NEET_NK_SENDsupport_percent, VG_NEET_NK_percentage)

  vulnerable_data <- vulnerable_data %>%
    mutate(
      NEET_NK_noSEN_percent = as.numeric(NEET_NK_noSEN_percent), NEET_NK_EHCP_percent = as.numeric(NEET_NK_EHCP_percent), NEET_NK_SENDsupport_percent = as.numeric(NEET_NK_SENDsupport_percent),
      VG_NEET_NK_percentage = as.numeric(VG_NEET_NK_percentage)
    )

  vulnerableLA <- reactive({
    vulnerable_data %>% filter(la_name == input$LA_choice)
  })

  vulnerableEng <- reactive({
    vulnerable_data %>% filter(la_name == "England")
  })


  ## Contextual data---------------------------------------------
  # reshape the data so it plots neatly!
  contextual_data <- la_ud %>%
    # select only contextual info
    select(geographic_level, region_name, la_name, Level_3, L2_em_GCSE_othL2, avg_att8, pt_l2basics_94, sess_overall_percent, sess_overall_percent_pa_10_exact) %>%
    # Put England and region name into LA name
    mutate(la_name = case_when(
      geographic_level == "National" ~ "England",
      geographic_level == "Regional" ~ region_name,
      TRUE ~ la_name
    ))


  contextual_data <- contextual_data %>%
    select(la_name, Level_3, L2_em_GCSE_othL2, avg_att8, pt_l2basics_94, sess_overall_percent, sess_overall_percent_pa_10_exact)

  contextual_data <- contextual_data %>%
    mutate(
      Level_3 = as.numeric(Level_3), L2_em_GCSE_othL2 = as.numeric(L2_em_GCSE_othL2), avg_att8 = as.numeric(avg_att8),
      pt_l2basics_94 = as.numeric(pt_l2basics_94), sess_overall_percent = as.numeric(sess_overall_percent), sess_overall_percent_pa_10_exact = as.numeric(sess_overall_percent_pa_10_exact)
    )

  contextLA <- reactive({
    contextual_data %>% filter(la_name == input$LA_choice)
  })

  contextEng <- reactive({
    contextual_data %>% filter(la_name == "England")
  })

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

  ## NEET/NK----------------------------
  ### Gauge chart-----------------------

  output$NEET_nk_gauge <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    NEET_nk_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(NEET_not_known_percent))

    gauge_plot(as.numeric(lineLA()$NEET_not_known_percent),
      round(as.numeric(England()$NEET_not_known_percent), 1),
      round(as.numeric(NEET_nk_perc_region), 1),
      range = c(1.4, 13.8),
      intervals = c(1.4, 3.6, 4.5, 5.4, 6.7, 13.8),
      needle_length = 0.9
    )
  })

  ### Annual change and national,regional comparison box-------

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

    NEET_nk_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(NEET_not_known_percent))

    NEET_nk_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(input$LA_choice, ": ", NEET_nk_perc, "%, ", change_ed(NEET_nk_change), NEET_nk_change, " ppts"),
      HTML(paste0(
        Regionname, ": ", NEET_nk_perc_region, "%, ", change_ed(NEET_nk_change_region), NEET_nk_change_region, " ppts.", br(),
        "England: ", NEET_nk_perc_Eng, "%, ", change_ed(NEET_nk_change_Eng), NEET_nk_change_Eng, " ppts. ", br(),
        "Annual changes are since end ", previous_year_end, "."
      )),
      color = "blue"
    )
  })


  ## NEET-------------------------

  ### Guage chart-----------------

  output$NEET_gauge <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    NEET_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(NEET_percent))

    gauge_plot(as.numeric(lineLA()$NEET_percent),
      round(as.numeric(England()$NEET_percent), 1),
      round(as.numeric(NEET_perc_region), 1),
      range = c(0.8, 6.8),
      intervals = c(0.8, 1.8, 2.3, 3.1, 3.9, 6.8),
      needle_length = 0.7
    )
  })

  ### Annual change and national,regional comparison box-------

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

    NEET_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(NEET_percent))

    NEET_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(annualchange_NEET))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(NEET_perc, "%, ", change_ed(NEET_change), NEET_change, " ppts"),
      HTML(paste0(
        Regionname, ": ", NEET_perc_region, "%, ", change_ed(NEET_change_region), NEET_change_region, " ppts.", br(),
        "England: ", NEET_perc_Eng, "%, ", change_ed(NEET_change_Eng), NEET_change_Eng, " ppts.", br(),
        "Annual changes are since end ", previous_year_end, "."
      )),
      color = "blue"
    )
  })


  ## Not known---------------------------

  ### Guage chart--------------------

  output$Nk_gauge <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    Nk_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(Notknown_percent))

    gauge_plot(as.numeric(lineLA()$Notknown_percent),
      round(as.numeric(England()$Notknown_percent), 1),
      round(as.numeric(Nk_perc_region), 1),
      range = c(0.0, 12.1),
      intervals = c(0.0, 0.9, 1.4, 2.1, 3.2, 12.1),
      needle_length = 0.7
    )
  })

  ### Annual change and national,regional comparison box-------

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

    Nk_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(Notknown_percent))

    Nk_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(annualchange_notknown))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Nk_perc, "%, ", change_ed(Nk_change), Nk_change, " ppts"),
      HTML(paste0(
        Regionname, ": ", Nk_perc_region, "%, ", change_ed(Nk_change_region), Nk_change_region, " ppts.", br(),
        "England: ", Nk_perc_Eng, "%, ", change_ed(Nk_change_Eng), Nk_change_Eng, " ppts.", br(),
        "Annual changes are since end ", previous_year_end, "."
      )),
      color = "blue"
    )
  })

  # Vulnerable groups NEET tab---------------------------------

  ## Vulnerable group--------------------------------

  ### Plot------------------------------

  output$vulnerable_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    vulnerable <- plotdata %>%
      ggplot(aes(
        y = VG_NEET_NK_percentage, x = "",
        fill = la_name,
        text = paste(la_name, ": ", VG_NEET_NK_percentage, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(vulnerable,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })


  ## EHCP--------------------------------------------

  ### Plot------------------------------

  output$EHCP_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    EHCP <- plotdata %>%
      ggplot(aes(
        y = NEET_NK_EHCP_percent, x = "",
        fill = la_name,
        text = paste(la_name, ": ", NEET_NK_EHCP_percent, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(EHCP,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## SEN support---------------------------------------

  ### Plot------------------------------

  output$SEN_support_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    SEN_support <- plotdata %>%
      ggplot(aes(
        y = NEET_NK_SENDsupport_percent, x = "",
        fill = la_name,
        text = paste(la_name, ": ", NEET_NK_SENDsupport_percent, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(SEN_support,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })


  ## No SEN-----------------------------------------

  ### Plot------------------------------

  output$No_SEN_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    No_SEN <- plotdata %>%
      ggplot(aes(
        y = NEET_NK_noSEN_percent, x = "",
        fill = la_name,
        text = paste(la_name, ": ", NEET_NK_noSEN_percent, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(No_SEN,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })


  # Participating in education and training------------------

  ## Total participating-----------

  ### Gauge chart--------------------
  output$Participation_gauge <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    participation_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(TOTAL_participating_in_education_and_training_percent))

    gauge_plot(as.numeric(lineLA()$TOTAL_participating_in_education_and_training_percent),
      round(as.numeric(England()$TOTAL_participating_in_education_and_training_percent), 1),
      round(as.numeric(participation_region), 1),
      range = c(87.4, 98.5),
      intervals = c(87.4, 91.6, 92.7, 93.9, 95.5, 98.5),
      needle_length = 0.7,
      reverse_colour = TRUE
    )
  })


  ### value box--------------------------------
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

    participating_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(TOTAL_participating_in_education_and_training_percent))

    participating_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(annual_change_Participation_in_education_training))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(participating_perc, "%, ", change_ed(participating_change), participating_change, " ppts"),
      HTML(paste0(
        Regionname, ": ", participating_perc_region, "%, ", change_ed(participating_change_region), participating_change_region, " ppts.", br(),
        "England: ", participating_perc_Eng, "%, ", change_ed(participating_change_Eng), participating_change_Eng, " ppts.", br(),
        "Annual changes are since March ", last_year, "."
      )),
      color = "blue"
    )
  })

  ## Participation type breakdown plot----------------------------

  output$participation_types <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    partRegion <- participation_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(partLA(), partRegion, partEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = unique(plotdata$la_name))

    participation_types <- plotdata %>%
      ggplot(aes(
        y = value, x = "",
        fill = participation_type,
        text = paste(participation_type, ": ", value, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      # position =position_fill(reverse = TRUE)
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      # geom_text(aes(label = paste0(value, "%")), colour = "#ffffff", size = 4, position = position_fill(reverse = TRUE, vjust = 0.5)) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "top",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    ggplotly(participation_types, tooltip = "text") %>%
      layout(
        uniformtext = list(minsize = 12, mode = "hide"),
        # xaxis = list(showticklabels = FALSE),
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
  ### Gauge chart--------------------

  output$Sept_Guar_gauge <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    Sept_Guar_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(September_guarantee_Offer_made_percent))

    gauge_plot(as.numeric(lineLA()$September_guarantee_Offer_made_percent),
      round(as.numeric(England()$September_guarantee_Offer_made_percent), 1),
      round(as.numeric(Sept_Guar_region), 1),
      range = c(50.8, 99.8),
      intervals = c(50.8, 93.2, 95.1, 96.7, 97.8, 99.8),
      needle_length = 0.7,
      reverse_colour = TRUE
    )
  })

  ### Value box-------------------------------------
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

    Sept_Guar_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(September_guarantee_Offer_made_percent))

    Sept_Guar_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(September_guarantee_annual_change_ppts))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(Sept_Guar_perc, "%, ", change_ed(Sept_Guar_change), Sept_Guar_change, " ppts"),
      HTML(paste0(
        Regionname, ": ", Sept_Guar_perc_region, "%, ", change_ed(Sept_Guar_change_region), Sept_Guar_change_region, " ppts.", br(),
        "England: ", Sept_Guar_perc_Eng, "%, ", change_ed(Sept_Guar_change_Eng), Sept_Guar_change_Eng, " ppts. ", br(),
        "Annual changes are since March ", last_year, "."
      )),
      color = "blue"
    )
  })

  # Contextual information--------------------------------------
  ## Attainment outcomes - level 3------------------------------

  output$level3_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    level_3 <- plotdata %>%
      ggplot(aes(
        y = Level_3, x = "",
        fill = la_name,
        text = paste(la_name, ": ", Level_3, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(level_3,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## Attainment outcomes - L2 EM GCSE------------------------------

  output$L2_EM_GCSE_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    L2_EM_GCSE <- plotdata %>%
      ggplot(aes(
        y = L2_em_GCSE_othL2, x = "",
        fill = la_name,
        text = paste(la_name, ": ", L2_em_GCSE_othL2, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(L2_EM_GCSE,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })


  ## School attendance - overall---------------------------------------

  output$overall_abs_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    overall_abs <- plotdata %>%
      ggplot(aes(
        y = sess_overall_percent, x = "",
        fill = la_name,
        text = paste(la_name, ": ", sess_overall_percent, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(overall_abs,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## School attendance - persistent-------------------------------------

  output$Persistent_abs_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    Persistent_abs <- plotdata %>%
      ggplot(aes(
        y = sess_overall_percent_pa_10_exact, x = "",
        fill = la_name,
        text = paste(la_name, ": ", sess_overall_percent_pa_10_exact, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(Persistent_abs,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## Average attainment 8 score------------------------------------------

  output$Attainment8_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    Attainment8 <- plotdata %>%
      ggplot(aes(
        y = avg_att8, x = "",
        fill = la_name,
        text = paste(la_name, ": ", avg_att8, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "Score") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(Attainment8,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## 9-4 standard pass in English and maths GCSEs--------------------------

  output$EM_pass_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng())
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    EM_pass <- plotdata %>%
      ggplot(aes(
        y = pt_l2basics_94, x = "",
        fill = la_name,
        text = paste(la_name, ": ", pt_l2basics_94, "%")
      )) +
      geom_bar(stat = "identity", na.rm = TRUE) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
      scale_y_continuous(limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "", y = "%") +
      theme(
        legend.position = "none",
        text = element_text(size = 14, family = "Arial"),
        strip.text.x = element_text(size = 14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )


    ggplotly(EM_pass,
      tooltip = c("text")
    ) %>%
      config(displayModeBar = FALSE)
  })

  ## Population-----------------------------------------------------------
  ### ONS-----------------------------------------------------------------

  output$ONS_pop <- renderValueBox({
    ONS_population <- lineLA() %>%
      pull(as.numeric(Age1617_ONS_population))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(ONS_population),
      paste0("ONS estimate"),
      color = "blue"
    )
  })

  ### NCCIS---------------------------------------------------------------

  output$NCCIS_pop <- renderValueBox({
    NCCIS_population <- lineLA() %>%
      pull(as.numeric(Cohort_DJFavg))

    # Put value into box to plug into app
    shinydashboard::valueBox(
      paste0(NCCIS_population),
      paste0("Recorded on CCIS"),
      color = "blue"
    )
  })


  # Files for download ------------------------------------------------------

  # all LAs
  output$download_ud <- downloadHandler(
    filename = function() {
      paste("la_underlying_data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(la_ud, file, row.names = FALSE)
    }
  )

  # selected LA pdf

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
        params <- input_la_choice <- input$LA_choice


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
