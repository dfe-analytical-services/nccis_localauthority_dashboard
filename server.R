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

  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )

  # Sidebar navigation
  observeEvent(
    input$scorecards,
    nav_select("left_nav", "scorecards")
  )

  observeEvent(
    input$user_guide,
    nav_select("left_nav", "user_guide")
  )

  observeEvent(
    input$technical_notes,
    nav_select("left_nav", "technical_notes")
  )

  # Footer navigation
  observeEvent(
    input$support_and_feedback,
    nav_select("pages", "support_and_feedback")
  )

  observeEvent(
    input$accessibility_statement,
    nav_select("pages", "accessibility_statement")
  )

  observeEvent(
    input$cookies_statement,
    nav_select("pages", "cookies_statement")
  )

  ## Back links to main dashboard ---------------------------------------------
  observeEvent(input$support_to_dashboard, nav_select("pages", "dashboard"))
  observeEvent(input$cookies_to_dashboard, nav_select("pages", "dashboard"))
  observeEvent(
    input$accessibility_to_dashboard,
    nav_select("pages", "dashboard")
  )

  ## Links to tech notes
  observeEvent(
    input$link_to_tech_notes1,
    nav_select("left_nav", "technical_notes")
  )

  observeEvent(
    input$link_to_tech_notes2,
    nav_select("left_nav", "technical_notes")
  )


  # Filters
  lineLA <- reactive({
    la_ud %>% filter(la_name == input$LA_choice)
  })

  England <- reactive({
    la_ud %>% filter(geographic_level == "National")
  })

  contextLA <- reactive({
    contextual_data %>% filter(la_name == input$LA_choice)
  })

  partLA <- reactive({
    participation_data %>% filter(la_name == input$LA_choice)
  })

  vulnerableLA <- reactive({
    vulnerable_data %>% filter(la_name == input$LA_choice)
  })


  # Top lines -------------------------
  ## create header on the scorecard so users know which LA it is showing

  output$data_description <- renderText({
    paste0("Scorecard for ", input$LA_choice)
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
      range = c(1.0, 21.5),
      intervals = c(1.0, 3.5, 4.3, 5.5, 6.7, 21.5),
      needle_length = 1.2,
      accessible = input$acc_colour_scheme
    )
  })

  ### Annual change and national,regional comparison box-------
  output$NEET_nk_vb <- renderUI({
    NEET_nk_perc <- lineLA() %>%
      pull(as.numeric(NEET_not_known_percent))
    NEET_nk_change <- lineLA() %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    Regionname <- lineLA() %>%
      pull(region_name)
    NEET_nk_perc_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(NEET_not_known_percent))
    NEET_nk_change_region <- filter(la_ud, geographic_level == "Regional", region_name == Regionname) %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    NEET_nk_perc_Eng <- England() %>%
      pull(as.numeric(NEET_not_known_percent))
    NEET_nk_change_Eng <- England() %>%
      pull(as.numeric(annual_change_ppts_NEET_not_known))
    layout_columns(
      col_widths = c(2, 8, 2),
      "",
      value_box(
        title = "NEET and activity not known",
        value = paste0(NEET_nk_perc, "%, ", change_ed(NEET_nk_change), NEET_nk_change, "ppts"),
        showcase = icon_trend(NEET_nk_change),
        p(paste0(Regionname, ":"), NEET_nk_perc_region, "%,", change_ed(NEET_nk_change_region), NEET_nk_change_region, "ppts"),
        p("England: ", NEET_nk_perc_Eng, "%,", change_ed(NEET_nk_change_Eng), NEET_nk_change_Eng, "ppts"),
        p("Annual changes are since end", previous_year_end),
        theme = "blue"
      ),
      ""
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
      range = c(0.6, 7.6),
      intervals = c(0.6, 2.1, 2.8, 3.8, 4.7, 7.6),
      needle_length = 1.0,
      accessible = input$acc_colour_scheme
    )
  })

  ### Annual change and national,regional comparison box-------

  output$NEET <- renderUI({
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
    value_box(
      title = "NEET",
      value = paste0(NEET_perc, "%, ", change_ed(NEET_change), NEET_change, " ppts"),
      showcase = icon_trend(NEET_change),
      p(paste0(Regionname, ": "), NEET_perc_region, "%,", change_ed(NEET_change_region), NEET_change_region, "ppts"),
      p("England: ", NEET_perc_Eng, "%,", change_ed(NEET_change_Eng), NEET_change_Eng, "ppts"),
      p("Annual changes are since end", previous_year_end),
      theme = "blue"
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
      range = c(0.0, 19.1),
      intervals = c(0.0, 0.5, 0.8, 1.6, 2.7, 19.1),
      needle_length = 1.0,
      accessible = input$acc_colour_scheme
    )
  })

  ### Annual change and national,regional comparison box-------

  output$Not_known <- renderUI({
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
    value_box(
      "Activity not known",
      value = paste0(Nk_perc, "%, ", change_ed(Nk_change), Nk_change, " ppts"),
      showcase = icon_trend(Nk_change),
      p(paste0(Regionname, ":"), Nk_perc_region, "%,", change_ed(Nk_change_region), Nk_change_region, "ppts"),
      p("England:", Nk_perc_Eng, "%,", change_ed(Nk_change_Eng), Nk_change_Eng, "ppts"),
      p("Annual changes are since end", previous_year_end),
      theme = "blue"
    )
  })

  # Vulnerable groups NEET tab---------------------------------

  ## Vulnerable group--------------------------------

  ### Plot------------------------------

  output$vulnerable_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng)
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

  ## create header so users can see the proportion of the cohort in a vulnerable group

  output$vg_cohort <- renderText({
    vgcohort <- lineLA() %>%
      pull(as.numeric(VG_cohort_percentage))

    paste0("Please note, in ", input$LA_choice, " local authority ", vgcohort, "%  of the 16-17 year old cohort were reported in a vulnerable group (see caution on the User guide about possible under-reporting)")
  })


  ## EHCP--------------------------------------------

  ### Plot------------------------------

  output$EHCP_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    vulnerableRegion <- vulnerable_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng)
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

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng)
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

    plotdata <- bind_rows(vulnerableLA(), vulnerableRegion, vulnerableEng)
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
      range = c(82.2, 99.0),
      intervals = c(82.2, 89.7, 91.3, 93.2, 95.4, 99.0),
      needle_length = 0.9,
      reverse_colour = TRUE,
      accessible = input$acc_colour_scheme
    )
  })


  ### value box--------------------------------
  output$Participating <- renderUI({
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
    value_box(
      "Participating in education and training",
      value = paste0(participating_perc, "%, ", change_ed(participating_change), participating_change, " ppts"),
      showcase = icon_trend(participating_change, up = "good"),
      p(paste0(Regionname, ":"), participating_perc_region, "%, ", change_ed(participating_change_region), participating_change_region, "ppts"),
      p("England:", participating_perc_Eng, "%, ", change_ed(participating_change_Eng), participating_change_Eng, " ppts"),
      p("Annual changes are since March", last_year),
      theme = "blue"
    )
  })

  ### Participation type breakdown plot----------------------------

  output$participation_types <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)
    partRegion <- participation_data %>% filter(la_name == Regionname)
    plotdata <- bind_rows(partLA(), partRegion, partEng)
    plotdata$la_name <- factor(plotdata$la_name, levels = unique(plotdata$la_name))
    plotdata$participation_type <- factor(plotdata$participation_type,
      levels = c("Full-time education", "Apprenticeship", "Other")
    )

    participation_types <- plotdata %>%
      ggplot(aes(
        y = value, x = "",
        fill = participation_type,
        text = paste(participation_type, ": ", value, "%")
      )) +
      geom_bar(
        stat = "identity", na.rm = TRUE,
        position = position_stack(reverse = TRUE)
      ) +
      coord_flip() +
      facet_wrap(~la_name, nrow = 3) +
      labs(x = "", y = "") +
      guides(fill = guide_legend(title = "")) +
      scale_fill_manual(values = c("#12436D", "#28A197", "#F46A25")) +
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
      range = c(74.0, 100.0),
      intervals = c(74.0, 93.1, 95.0, 96.8, 98.2, 100.0),
      needle_length = 0.9,
      reverse_colour = TRUE,
      accessible = input$acc_colour_scheme
    )
  })

  ### Value box-------------------------------------
  output$Sept_Guarantee <- renderUI({
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
    value_box(
      "September guarantee",
      value = paste0(Sept_Guar_perc, "%, ", change_ed(Sept_Guar_change), Sept_Guar_change, " ppts"),
      showcase = icon_trend(Sept_Guar_change, up = "good"),
      p(paste0(Regionname, ":"), Sept_Guar_perc_region, "%, ", change_ed(Sept_Guar_change_region), Sept_Guar_change_region, "ppts"),
      p("England:", Sept_Guar_perc_Eng, "%, ", change_ed(Sept_Guar_change_Eng), Sept_Guar_change_Eng, "ppts"),
      p("Annual changes are since September ", previous_year_end),
      theme = "blue"
    )
  })

  # Contextual information--------------------------------------
  ## Attainment outcomes - level 3------------------------------

  output$level3_plot <- renderPlotly({
    Regionname <- lineLA() %>%
      pull(region_name)

    contextRegion <- contextual_data %>% filter(la_name == Regionname)

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
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

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
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

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
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

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
    plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)

    Persistent_abs <- plotdata %>%
      ggplot(aes(
        y = enrolments_pa_10_exact_percent, x = "",
        fill = la_name,
        text = paste(la_name, ": ", enrolments_pa_10_exact_percent, "%")
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

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
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

    plotdata <- bind_rows(contextLA(), contextRegion, contextEng)
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

  # output$ONS_pop <- renderValueBox({
  # ONS_population <- lineLA() %>%
  #  pull(Age1617_ONS_population) %>%
  # as.numeric()

  # Put value into box to plug into app
  # value_box(
  #   format(ONS_population, big.mark = ","),
  #   paste0("ONS estimate - January ", latest_year),
  #   color = "blue"
  #  )
  # })

  ### NCCIS---------------------------------------------------------------

  output$NCCIS_pop <- renderUI({
    NCCIS_population <- lineLA() %>%
      pull(Cohort_DJFavg) %>%
      as.numeric()

    # Put value into box to plug into app
    value_box(
      title = "NCCIS population",
      value = format(NCCIS_population, big.mark = ","),
      showcase = bs_icon("person-fill", size = "2em"),
      p("Recorded on CCIS - end ", last_year, " (average of December, January, February)"),
      theme = "blue"
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


  output$pdfDownload <- downloadHandler(
    filename = function() {
      paste0(gsub(",", "", gsub(" ", "-", input$LA_choice)), "_neet_comparator_scorecard.pdf")
    },
    content = function(file) {
      # Add a loading modal, can probably make this prettier at a later date
      showModal(modalDialog("Preparing PDF report...", footer = NULL))
      on.exit(removeModal())

      # List of parameters to pass from shiny to the report
      params <- list(input_la_choice = input$LA_choice)
      print(params)
      # Render the pdf file from the rmarkdown template
      rmarkdown::render("nccis-la-report.Rmd",
        output_file = file,
        params = params,
        output_format = "pdf_document",
        envir = new.env(parent = globalenv())
      )
    }
  )
}
