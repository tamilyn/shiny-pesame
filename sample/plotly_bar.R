
  ############################################################
  # generateTicks ----
  # generates sequence for tick marks
  generateTicks <- function(dd) {
    tickMarkers = round(min(0.5, max(abs(c(dd$high, dd$low) - 0.5))), 2)
    seq(-tickMarkers, tickMarkers, length.out = 5)
  }

  generate_bars <- function(dd) {

    if(length(dd) <= 0) {
      print(paste("126: returning empty plot_ly,no rows"))
      return(ggplot())
    }
    
    ticks = generateTicks(dd)
    my.xaxis = list( title = "auc", 
                     autotick = TRUE,
                     ticktextsrc = format(ticks+0.5, digits=3),
                     showticklabels = TRUE,
                     showline = TRUE,
                     linecolor = toRGB("gray"),
                     linewidth = 1,
                     ticktext = format(ticks+0.5, digits=3),
                     tickvals  = ticks,
                     tickmode = "array"
                     )
    
    my.error.bars = list(type = "data", symmetric = FALSE, color = '#333333',arrayminus = ~dd$low-0.5,array = ~dd$high-0.5)
    
    hover.text = paste(dd$Names, " Auc ",round(dd$auc,2)," high: ", round(dd$high,2), "low", round(dd$low,2))
    
    dd$colorxx = map_chr(dd$Enriched, ~ to_color(.)) 
  
    p <- 
      plot_ly( dd, x = ~dd$heights, y = ~dd$Names, type = 'bar' 
              , text = hover.text
              , hoverinfo = "text"
              , marker = list(color = ~dd$colorxx)) %>%
      add_segments( x = ~dd$low - 0.5, xend = ~dd$high - 0.5 
                    , y = ~dd$Names, yend = ~dd$Names
                    , color = "#0000FF" 
                    , line = list(color="black",width=1)
                    , marker = list(color="black", symbol = 142)
                    , showlegend = FALSE  
                    , hoverinfo = "none") %>%
      layout(title = NULL, 
             xaxis = my.xaxis, 
             margin = list(l = 120,t = 50, b = 30, unit = "pt", pad = 2),
             yaxis = list(title=""))
    p
  }
