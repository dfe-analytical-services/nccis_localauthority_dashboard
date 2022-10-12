gauge_plot <- function(value, valueEng, valueRegion,
                       range = c(1.4, 13.8),
                       intervals = c(1.4, 3.6, 4.5, 5.4, 6.7, 13.8),
                       needle_length = 1.0,
                       reverse_colour = FALSE) {
  interval_text <- format(intervals)
  interval_text[abs(intervals - valueEng) < 0.2] <- ""
  interval_text[abs(intervals - valueRegion) < 0.2] <- ""
  quantcols <- c("#cedbcb", "#cdd0b7", "#ccbf9b", "#bb906f", "#8c301b")
  if(reverse_colour){quantcols <- quantcols[5:1]}
  fig <- plot_ly()
  domain <- list(x = c(0, 0.9), y = c(0, 1.8))
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
        tickfont = list(size = 15)
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
        list(range = c(valueEng, valueEng), line = list(color = "white", width = 2)),
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
        tickcolor = "black",
        tickvals = list(valueRegion, valueEng),
        ticktext = list(" Region ", " Eng "),
        ticklen = 1, ticks = "outside", showticklabels = TRUE
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
