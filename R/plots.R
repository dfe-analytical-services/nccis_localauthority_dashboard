gauge_plot <- function(value, valueEng){

fig <- plot_ly()  
    
fig <- fig %>% add_trace(
  domain = list(x = c(0, 2), y = c(0, 2)),
  value = value, 
  number = list(suffix = "%"),
  title = list(text = "NEET or not known", font =list(size=32)),
  type = "indicator",
  mode = "gauge+number",
  gauge = list(
    axis = list(range = list(1.4, 13.8), tickwidth = 1, tickcolor = "darkblue",
                tickvals=list(1.4,3.6,4.5,5.4,6.7,13.8)), #need to make this to the max % neet/nk
    bar = list(color='rgba(0,0,0,0)'),
    bgcolor = "white",
    borderwidth = 1,
    #bordercolor = "gray",
    steps = list(
      list(range = c(1.4, 3.6), color = "limegreen"), #need to make these the quintile boundaries
      list(range = c(3.6, 4.5), color = "yellowgreen"),
      list(range = c(4.5, 5.4), color = "yellow"),
      list(range = c(5.4, 6.7), color = "gold"),
      list(range = c(6.7, 13.8), color = "red"),
      list(range = c(2.4, 2.4), line = list(color='blue',width=5))
    ),
    threshold = list(
      line = list(color = "black", width = 4),
      displayvalue = "England",
      thickness = 1,
      value = valueEng
  )
  )
)
fig <- fig %>% add_trace(
  domain = list(x = c(0, 2), y = c(0, 2)),
  value = value+3, 
  number = list(suffix = "%"),
  type = "indicator",
  mode = "gauge",
  gauge = list(
    axis = list(range = list(1.4, 13.8), tickwidth = 6, tickcolor = "black",
                tickvals=list(value),ticklen=180,ticks='inside',showticklabels=FALSE),
    bgcolor = 'rgba(0,0,0,0)',
    bar = list(color='rgba(0,0,0,0)'),
    borderwidth = 3
  )
)
return(fig)
}
