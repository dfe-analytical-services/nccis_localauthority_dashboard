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

icon_up_arrow <- tags$i(
  class = "fa fa-arrow-up",
  style = paste("color:", acces_grad_palette[5])
)

icon_down_arrow <- tags$i(
  class = "fa fa-arrow-down",
  style = paste("color:", gss_colour_palette[2])
)

icon_no_change <- tags$i(
  class = "fa fa-arrows-alt-h",
  style = paste("color:", acces_grad_palette[3])
)

icon_change <- function(value) {
  if (value > 0) {
    icon_up_arrow
  } else if (value == 0) {
    icon_no_change
  } else {
    icon_down_arrow
  }
}
