icon_trend <- function(perc_change, up = "bad") {
  if (up == "bad") {
    down_arrow_colour <- "#B1C209"
    up_arrow_colour <- "#d4351c"
  } else {
    up_arrow_colour <- "#B1C209"
    down_arrow_colour <- "#d4351c"
  }
  if (perc_change > 0) {
    return(bs_icon("arrow-up", fill = up_arrow_colour, size = "2em"))
  } else if (perc_change < 0) {
    return(bs_icon("arrow-down", fill = down_arrow_colour, size = "2em"))
  } else {
    return(bs_icon("dash", size = "2em"))
  }
}
