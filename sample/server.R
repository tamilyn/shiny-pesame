# /Users/tamicrawford/projects/pdash3/sample/server.R

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(rio)
library(RColorBrewer)
library(purrr)
library(futile.logger)
library(stringr)
library(data.table)

source("util.R")
source("ftn2.R")
source("plotly_bar.R")

shinyServer(function(input, output, session) {

  factorizedDataList <- reactive({
     isolate({

     print("ISOLATE FDL")
     data <- create_test_data(input$numCols, input$numRows)
     md <- create_test_metadata(input$numCols)
     new_md <- factorize_data(md)

     newlist <- append(new_md,
         list(mvdata = data, origmetadata = md))

    })
  })

  observeEvent(input$testdataButton, {
     if(input$testdataButton > 0) {
        factorizedDataList()
     }
  })

  theSelectedFactor <- reactive({
    req(input$selectedFactor)

    #print(str_c("selected factor: [", input$selectedFactor, "]", sep=""))

    lst <- factorizedDataList()
    fd <- lst[["metadata"]]

    if(is.null(input$selectedFactor)) {
       print("NO FACTOR")
       return(NULL)
    }

    #split the name at the : take the left part as the factor name
    fn <- strsplit(input$selectedFactor, ":") %>% unlist %>% first

    #print(str_c("Selected factor: ", fn))
    fd[, fn]
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    lst <- factorizedDataList()
    mvdata <- lst[["mvdata"]]
    tft <- t(mvdata)

     # convert data
     bd = suppressWarnings(helper.data_by_auc( tft, 
          theSelectedFactor(), input$adj_method))

    ft <- bd
    sf <- as.numeric(input$signficance_threshold)
    filt = (ft["p.adjust", ] < sf) & !is.na(ft["p.adjust", ])
    t(ft[, filt, drop=F]) 
  })

  
  ############################################################
  # generateTicks ----
  # generates sequence for tick marks
  generateTicks <- function(dd) {
    tickMarkers = round(min(0.5, max(abs(c(dd$high, dd$low) - 0.5))), 2)
    seq(-tickMarkers, tickMarkers, length.out = 5)
  }
  
  #####################################################################                                
  # filteredData ----
  # Apply filter to base data
  # Get data with adjust method applied, then apply p.value filter,
  # transpose result.
  filteredData <- reactive({ 
    b <- baseFilteredData()
    if(is.null(b)) {
      print("NO BASE FILTERED DATA")
      return(NULL)
    }

    if (nrow(b) == 0) { 
      print("127: no data")
      return(NULL) 
    }
    
    dd = as.data.frame(b)
    ticks = generateTicks(dd)
    f = theSelectedFactor()

    dd$Enriched = levels(f)[(dd$auc<.5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })
  

  #####################################################################
  # output$testDataTable ----
  output$testDataTable <- renderTable({
    lst <- factorizedDataList()
    return(lst[["mvdata"]])
  })


  #####################################################################
  # output$metadataTable ----
  output$metadataTable <- renderTable({
    lst <- factorizedDataList()
    return(lst[["origmetadata"]])
  })


  #####################################################################
  # output$metadataTable ----
  output$factorizedMetadataTable <- renderTable({
    
    lst <- factorizedDataList()
    return(lst[["metadata"]])
  })


  #####################################################################
  # output$testDataGenerated ----
  output$testDataGenerated <- reactive({
     if(input$testdataButton > 0) {
       lst <- factorizedDataList()
       return(!is.null(lst))
     }
  })

  outputOptions(output, 'testDataGenerated', suspendWhenHidden=FALSE)


  #####################################################################
  output$citation <- renderText({
        a <- citation("ggplot2")
        b <- a[1]
        msg <- str_c(b$author, ". ", b$title, ".", b$publisher, ". ", 
           b$year, ".")
	msg
  })

  #####################################################################


  ############################################################
  ############################################################
  # Generate plot using ggplot
  # generate_ggplot ----
  generate_ggplot <- function(dd){
   flog.info(paste("generate_ggplot: ", length(dd)))
   if(length(dd) > 0) {
      ticks = generateTicks(dd)
      dd$heights = dd$auc - 0.5
      ggplot(data = dd, aes(y = heights, x = Names)) + 
        theme_bw() + 
        geom_bar(stat = "identity", aes(fill = Enriched)) + 
        geom_errorbar(aes(ymax = high - 0.5, ymin = low - 0.5)) +
        coord_flip() + xlab("")  + ylab("") + 
        scale_y_continuous(breaks = ticks, labels = format(ticks+0.5, digits=3)) 
    } else {
      ggplot()  # empty plot
    }
  }
  
  # Render the plot
  # output$filterPlot ----
  output$filteredPlot <- renderPlot({
    generate_ggplot(filteredData())
  })



  # use plot_ly to display data
  # generatePlot_ly ----
  generatePlot_ly <- function(dd) {
    print(paste("211: generatePlot_ly"))
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
    
    
    dd$color = map_chr(dd$Enriched, to_color)
    print(unique(dd$color))
   
    p <- dd %>% 
      plot_ly( ) %>%
      add_bars( x = ~dd$heights, 
                y = ~dd$Names,  
                #color = ~dd$color, 
                orientation = "h", 
                hoverinfo = "text", text = hover.text
                # , marker = list(color = ~dd$color)
                ) %>%
      #scale_colour_brewer(palette="BuPu", direction=-1) %>%
      #scale_fill_manual(values=c("red", "blue", "green")) +
      add_markers( x = ~dd$low - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE ) %>%   
      add_markers( x = ~dd$high - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE,  
                   hoverinfo = "none" ) %>%
      add_segments( x = ~dd$low - 0.5, xend = ~dd$high - 0.5, y = ~dd$Names, yend = ~dd$Names, color = toRGB("black"), 
                    showlegend = FALSE,  hoverinfo = "none") %>%
      layout(title =NULL, xaxis = my.xaxis, margin = list(l=120,t=50,b=30,unit="pt",pad=2),
           yaxis = list(title=""))
    p #+ scale_colour_brewer(palette="BuPu", direction=-1) 
  }

  # output$filteredPlotly ----
  #     this uses f the factor
  output$filteredPlotly <- renderPlotly({
    dd <- filteredData()
    if(is.null(dd) || length(dd) == 0) {
       print("filteredPlotly: NO DATA")
       return(NULL)
    }

    f = theSelectedFactor()

    dd$heights = dd$auc - 0.5
    dd$auc1 = dd$heights
    dd$Enriched = levels(f)[(dd$auc<.5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    generate_bars(dd)

    generatePlot_ly(dd) 
  })


  ############################################################
  # Informational message
  # output$informationMessage ----
  output$informationMessage <- renderText({
    lst <- factorizedDataList()
    mvdata <- lst[["mvdata"]]

    if(is.null(mvdata)) {
       return("No data loaded")
    }

    numDisplayed <- nrow(filteredData())
    totalRows <- nrow(mvdata)
 
    msg <- paste("Displaying ", numDisplayed , " of ", totalRows,
                 " total rows using ", input$adj_method, 
                 " significance value ",  input$signficance_threshold)
    print(msg)
    msg 
   })

  ## output$factorVariables ----
  output$factorVariables = renderUI({
    lst <- factorizedDataList()
    labels <- lst[["labels"]]
    #print("factorVariables")
    #print(labels)
    selectInput('selectedFactor', 'Select factor', labels )
  })
  
})
