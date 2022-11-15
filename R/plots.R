gauge_plot <- function(value, valueEng, valueRegion,
                       range = c(1.4, 13.8),
                       intervals = c(1.4, 3.6, 4.5, 5.4, 6.7, 13.8),
                       needle_length = 1.0,
                       reverse_colour = FALSE,
                       xdomain = c(0, 0.96),
                       fig = plot_ly()) {
  interval_text <- format(intervals)
  mask_range <- (range[2] - range[1]) / 56
  interval_text[abs(intervals - valueEng) < mask_range] <- ""
  interval_text[abs(intervals - valueRegion) < mask_range] <- ""
  quantcols <- c("#cedbcb", "#cdd0b7", "#ccbf9b", "#bb906f", "#8c301b")
  if (reverse_colour) {
    quantcols <- quantcols[5:1]
  }
  domain <- list(x = xdomain, y = c(0, 0.96))
  value_size <- 42
  fig <- fig %>% add_trace(
    domain = domain,
    value = value,
    number = list(suffix = "%", font = list(size = value_size)),
    type = "indicator",
    mode = "gauge+number",
    gauge = list(
      axis = list(
        range = range, tickwidth = 1, tickcolor = "darkblue",
        tickvals = intervals,
        ticktext = interval_text,
        tickfont = list(size = 15),
        tickangle = 36
      ), # need to make this to the max % neet/nk
      bar = list(color = "rgba(0,0,0,0)"),
      bgcolor = "white",
      borderwidth = 1,
      # bordercolor = "gray",
      steps = list(
        list(range = intervals[1:2], color = quantcols[1]), # need to make these the quintile boundaries
        list(range = intervals[2:3], color = quantcols[2]),
        list(range = intervals[3:4], color = quantcols[3]),
        list(range = intervals[4:5], color = quantcols[4]),
        list(range = intervals[5:6], color = quantcols[5]),
        list(range = c(valueEng, valueEng), line = list(color = "black", width = 3)),
        list(range = c(valueRegion, valueRegion), line = list(color = "black", width = 3)),
        list(range = c(valueEng, valueEng), line = list(color = "#12436D", width = 2)),
        list(range = c(valueRegion, valueRegion), line = list(color = "#6BACE6", width = 2))
      )
    )
  )
  fig <- fig %>% add_trace(
    domain = domain,
    value = value,
    number = list(suffix = "%", font = list(size = value_size)),
    type = "indicator",
    mode = "gauge",
    gauge = list(
      axis = list(
        range = range, tickwidth = 2,
        tickcolor = "#1d70b8",
        tickvals = list(valueRegion),
        ticktext = list(" Region "),
        tickfont = list(color = "#1d70b8"),
        ticklen = 1,
        ticks = "outside",
        showticklabels = TRUE,
        tickangle = 36
      ),
      bgcolor = "rgba(0,0,0,0)",
      bar = list(color = "rgba(0,0,0,0)"),
      borderwidth = 1
    )
  )
  fig <- fig %>% add_trace(
    domain = domain,
    value = value,
    number = list(suffix = "%", font = list(size = value_size)),
    type = "indicator",
    mode = "gauge",
    gauge = list(
      axis = list(
        range = range, tickwidth = 2,
        tickcolor = "#12436D",
        tickvals = list(valueEng),
        ticktext = list(" Eng "),
        tickfont = list(color = "#12436D"),
        ticklen = 1,
        ticks = "outside",
        showticklabels = TRUE,
        tickangle = 36
      ),
      bgcolor = "rgba(0,0,0,0)",
      bar = list(color = "rgba(0,0,0,0)"),
      borderwidth = 1
    )
  )
  fig <- fig %>% add_trace(
    domain = domain,
    value = value,
    number = list(suffix = "%", font = list(size = value_size)),
    type = "indicator",
    mode = "gauge",
    gauge = list(
      axis = list(
        range = range, tickwidth = 4, tickcolor = "black",
        tickvals = list(value), ticklen = 160 * needle_length, ticks = "inside", showticklabels = FALSE
      ),
      bgcolor = "rgba(0,0,0,0)",
      bar = list(color = "rgba(0,0,0,0)"),
      borderwidth = 1
    )
  )
  fig <- fig %>% add_trace(
    domain = domain,
    value = value,
    number = list(suffix = "%", font = list(size = value_size)),
    type = "indicator",
    mode = "gauge",
    gauge = list(
      axis = list(
        range = range, tickwidth = 2, tickcolor = "#2073BC",
        tickvals = list(value), ticklen = 160 * needle_length,
        ticks = "inside", showticklabels = FALSE
      ),
      bgcolor = "rgba(0,0,0,0)",
      bar = list(color = "rgba(0,0,0,0)"),
      borderwidth = 1
    )
  )
  return(fig)
}


plot_neetnkgauge <- function(dfla, line_la, line_england) {
  Regionname <- line_la %>% pull(region_name)

  NEET_nk_perc_region <- dfla %>%
    filter(geographic_level == "Regional", region_name == Regionname) %>%
    pull(as.numeric(NEET_not_known_percent))

  gauge_plot(as.numeric(line_la$NEET_not_known_percent),
    round(as.numeric(line_england$NEET_not_known_percent), 1),
    round(as.numeric(NEET_nk_perc_region), 1),
    range = c(1.4, 13.8),
    intervals = c(1.4, 3.6, 4.5, 5.4, 6.7, 13.8),
    needle_length = 0.9
  )
}

plot_neetgauge <- function(dfla, line_la, line_england, xdomain = c(0, 0.96)) {
  Regionname <- line_la %>%
    pull(region_name)

  NEET_perc_region <- dfla %>%
    filter(geographic_level == "Regional", region_name == Regionname) %>%
    pull(as.numeric(NEET_percent))

  gauge_plot(as.numeric(line_la$NEET_percent),
    round(as.numeric(line_england$NEET_percent), 1),
    round(as.numeric(NEET_perc_region), 1),
    range = c(0.8, 6.8),
    intervals = c(0.8, 1.8, 2.3, 3.1, 3.9, 6.8),
    needle_length = 0.7,
    xdomain = xdomain
  )
}

plot_nkgauge <- function(dfla, line_la, line_england,
                         fig = plotly(),
                         xdomain = c(0, 1.9)) {
  Regionname <- line_la %>%
    pull(region_name)

  nk_perc_region <- dfla %>%
    filter(geographic_level == "Regional", region_name == Regionname) %>%
    pull(as.numeric(Notknown_percent))

  gauge_plot(as.numeric(line_la$Notknown_percent),
    round(as.numeric(line_england$Notknown_percent), 1),
    round(as.numeric(nk_perc_region), 1),
    range = c(0.0, 12.1),
    intervals = c(0.0, 0.9, 1.4, 2.1, 3.2, 12.1),
    needle_length = 0.7,
    fig = fig,
    xdomain = xdomain
  )
}


plot_vulnerablebar <- function(dfvulnerable, vulnerable_la, line_la, vulnerable_england,
                               plotcat = "VG_NEET_NK_percentage") {
  figtitles <- data.frame(
    flag = c(
      "VG_NEET_NK_percentage", "NEET_NK_noSEN_percent", "NEET_NK_EHCP_percent",
      "NEET_NK_SENDsupport_percent"
    ),
    figtitle = c("Vulnerable group", "No SEND", "SEND (EHCP)", "SEN support")
  )
  figtitle <- (figtitles %>% filter(flag == plotcat))$figtitle
  Regionname <- line_la %>% pull(region_name)
  vulnerableRegion <- dfvulnerable %>% filter(la_name == Regionname)

  plotdata <- bind_rows(vulnerable_la, vulnerableRegion, vulnerable_england)
  plotdata$la_name <- factor(plotdata$la_name, levels = plotdata$la_name)
  
  plotdata %>%
    ggplot(aes(
      y = .data[[plotcat]], x = "",
      fill = la_name,
      text = paste(la_name, ": ", .data[[plotcat]], "%")
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
      text = element_text(size = 14),
      strip.text.x = element_text(size = 14),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle(figtitle)
}

plot_partgauge <- function(dfla, line_la, line_england) {
  Regionname <- line_la %>% pull(region_name)
  
  part_perc_region <- dfla %>%
    filter(geographic_level == "Regional", region_name == Regionname) %>%
    pull(as.numeric(TOTAL_participating_in_education_and_training_percent))
  
  gauge_plot(as.numeric(line_la$TOTAL_participating_in_education_and_training_percent),
             round(as.numeric(line_england$TOTAL_participating_in_education_and_training_percent), 1),
             round(as.numeric(part_perc_region), 1),
             range = c(87.4, 98.5),
             intervals = c(87.4, 91.6, 92.7, 93.9, 95.5, 98.5),
             needle_length = 0.9,
             reverse_colour = TRUE
  )
}


plot_participationbar <- function(dfparticipation, participation_la, line_la, participation_england) {
  
  Regionname <- line_la %>% pull(region_name)
  
  partRegion <- dfparticipation %>% filter(la_name == Regionname)
  
  plotdata <- bind_rows(participation_la, partRegion, participation_england) 
  plotdata$la_name <- factor(plotdata$la_name, levels = unique(plotdata$la_name))
  
  plotdata %>% 
    ggplot(aes(
      y = value, x = "",
      fill = participation_type,
      text = paste(participation_type, ": ", value, "%")
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
      legend.position = "top",
      text = element_text(size = 14),
      strip.text.x = element_text(size = 14),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}



Sept_Guar_gauge <- function(dfla, line_la, line_england) {
  Regionname <- line_la %>% pull(region_name)
  
  Sept_Guar_region <- dfla %>%
    filter(geographic_level == "Regional", region_name == Regionname) %>%
    pull(as.numeric(September_guarantee_Offer_made_percent))
  
  gauge_plot(as.numeric(line_la$September_guarantee_Offer_made_percent),
             round(as.numeric(line_england$September_guarantee_Offer_made_percent), 1),
             round(as.numeric(Sept_Guar_region), 1),
             range = c(50.8, 99.8),
             intervals = c(50.8, 93.2, 95.1, 96.7, 97.8, 99.8),
             needle_length = 0.9,
             reverse_colour = TRUE
  )
}


plot_contextualbar <- function(dfcontextual, contextual_la, line_la, contextual_england,
                               plotcat = "Level_3") {
  figtitles <- data.frame(
    flag = c(
      "Level_3", "L2_em_GCSE_othL2", "avg_att8",
      "pt_l2basics_94","sess_overall_percent","sess_overall_percent_pa_10_exact"
    ),
    figtitle = c("% 19 year olds achieving level 3", 
                 "% 19 year olds achieving GCSE 9-4 standard pass in English and maths (or equivalent) between ages 16 and 19, for those who had not achieved this level by 16", 
                 "Average attainment 8 score per pupil", 
                 "% 9-4 standard pass in English and maths GCSEs",
                 "Overall absence (% of sessions)",
                 "Persistent absentees (% of pupils)")
  )
  figtitle <- (figtitles %>% filter(flag == plotcat))$figtitle
  Regionname <- line_la %>% pull(region_name)
  contextualRegion <- dfcontextual %>% filter(la_name == Regionname)
  
  bind_rows(contextual_la, contextualRegion, contextual_england) %>%
    ggplot(aes(
      y = .data[[plotcat]], x = "",
      fill = la_name,
      text = paste(la_name, ": ", .data[[plotcat]], "%")
    )) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    coord_flip() +
    facet_wrap(~la_name, nrow = 3) +
    labs(x = "", y = "") +
    guides(fill = guide_legend(title = "")) +
    scale_fill_manual(values = c("#28A197", "#12436D", "#F46A25")) +
    theme_minimal() +
    labs(x = "", y = "%") +
    theme(
      legend.position = "none",
      text = element_text(size = 14),
      strip.text.x = element_text(size = 14),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle(figtitle)
}