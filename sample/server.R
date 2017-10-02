# /Users/tamicrawford/projects/pdash3/pdash0930/server.R

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(rio)
library(RColorBrewer)
library(purrr)
library(futile.logger)
library(stringr)
library(DT)
library(data.table)
library(Hmisc)

source("util.R")
source("generate.R")
source("generate_test_data.R")
source("ftn2.R")
source("plotly_bar.R")

shinyServer(function(input, output, session) {

  source('pesame.R', local = TRUE)
  source('helper.R', local = TRUE)

  ###-- BEGIN reactive values ----
  availableFactors <- reactiveVal(list())
  unprocessedFactors <- reactiveVal(list())
  allOriginalFactor <- reactiveVal(list())

  metadata <- reactiveVal(NULL)
  mvdata <- reactiveVal(NULL)

  orig_metadata <- reactiveVal(NULL)
  orig_mvdata <- reactiveVal(NULL)
  ###-- END reactive values ----

  theSelectedFactor <- reactive({
    req(input$selectedFactor)

    #print(str_c("selected factor: [", input$selectedFactor, "]", sep=""))

    fd <- metadata()
    if(is.null(input$selectedFactor)) {
       print("NO FACTOR")
       return(NULL)
    }
    #print("theSelected")
    #print(dim(fd))
    #print(rownames(fd))
    #print(colnames(fd))
    fd[, input$selectedFactor]
  })

  get_tft <- reactive({
    mdata <- mvdata()
    if(is.null(mdata)) return(NULL)
    t_tft <- t(mdata)

    rownames(t_tft) <- colnames(mdata)

    #print("DIM of t_tft")
    #print(dim(t_tft))
    #print(rownames(t_tft))
    #print(colnames(t_tft))
    t_tft
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    tft <- get_tft()

    # convert data
    bd = suppressWarnings(helper.data_by_auc( tft,
          theSelectedFactor(), input$adj_method))

    print("DIM of bd")
    print(dim(bd))

    ft <- bd

    #rownames(ft) <- colnames(mdata)
    #colnames(ft) <- colnames(mdata)
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
    #browser()
    print("filteredData")
    print(head(b))

    dd = as.data.frame(b)
    ticks = generateTicks(dd)
    f = theSelectedFactor()

    dd$Enriched = levels(f)[(dd$auc<.5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })


  mymetadata <- reactive({
     metadata()
  })
  #####################################################################
  # output$metadataTable ----
  output$factorizedMetadataDataTable <- renderDataTable({
     req(processTestdataFile(), initFactorVariables(), mymetadata())

     action <- dataTableAjax(session, mymetadata())
     print(str_c("factor metadataDataTable DATA metadata()", nrow(metadata())))
     #data.table(metadata())
     DT::datatable(mymetadata())
  })

  #####################################################################
  output$citation <- renderText({
        a <- citation("ggplot2")
        b <- a[1]
        msg <- str_c(b$author, ". ", b$title, ".", b$publisher, ". ",
           b$year, ".")
	msg
  })

  #####################################################################

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


    #dd$color = map_chr(dd$Enriched, to_color)
    dd$color = "blue" # map_chr(dd$Enriched, to_color)
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
    print("FILTERED PLOTLY")
    dd <- filteredData()
    if(is.null(dd) || length(dd) == 0) {
       print("filteredPlotly: NO DATA")
       return(NULL)
    }

    f = theSelectedFactor()
    dd$Enriched = levels(f)[(dd$auc<.5) + 1]
    #print(str_c("filteredPlotly: selected factor:"))

    #print(head(f,2))
    #print(rownames(dd))
    #print(colnames(dd))

    dd$heights = dd$auc - 0.5
    dd$auc1 = dd$heights
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])

    #generate_bars(dd)

    generatePlot_ly(dd)
  })


  ############################################################
  # Informational message
  # output$informationMessage ----
  output$informationMessage <- renderText({
    mm <- metadata()

    if(is.null(mm)) {
       return("No data loaded")
    }

    if(is.null(filteredData())) {
      numDisplayed <- 0
    } else {
      numDisplayed <- nrow(filteredData())
    }
    mdata <- mvdata()
    totalRows <- nrow(mdata)

    msg <- paste("Displaying ", numDisplayed , " of ", totalRows,
                 " total rows using ", input$adj_method,
                 " significance value ",  input$signficance_threshold)
    print(msg)
    msg
   })

  output$hasFactorVariables = reactive({
    af <- availableFactors()
    return(length(af) > 0) 
  })

  ## output$factorVariables ----
  output$factorVariables = renderUI({
    af <- availableFactors()
    if(length(af) > 0) {
      selectInput('selectedFactor', 'Select factor', af )
    } else {
      h1("No available factors")
    }
  })

 initFactorVariables <- reactive({
   isolate({
     mf = metadata()

     cnames = colnames(mf)
     if(length(cnames) > 1) {
       details = new_factor_details(mf)
       setDetails(details)
     }
   })
 })

 ###
 # create html action buttons for features which need to be split
 splitButtons <- function(idnum) {
  paste0('<div class="btn-group" role="group" aria-label="Basic example">
   <button type="button" class="btn btn-primary xbtn-secondary modify"id=median_',idnum,'>Median</button>
   <button type="button" class="btn btn-primary xbtn-secondary modify"id=mean_',idnum,'>Mean</button>
   </div>')
 }


 ##
 # observeEvent input$lastClick ----
  observeEvent(input$lastClick, {
    lid = input$lastClickId
    #print(paste("observeEvent$lastClick", input$lastClickId ))
    if (lid %like% "median") {
      row_to_split <- as.numeric(gsub("median_","",lid))
      print(str_c("MEDIAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "median")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "mean") {
      row_to_split <- as.numeric(gsub("mean_","",lid))
      print(str_c("MEAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "mean")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "modify") {
      print("MODIFY")
      row_to_modify <- as.numeric(gsub("modify_","",lid))
      #print(str_c("MODIFYING that row", lid, row_to_modify, sep = " "))
      #showModal(modal_modify)
    }
   })
 
 displayDetails <- reactive({
   details <- allOriginalFactor() %>%
     arrange(ready) %>%
     mutate(look = ifelse(ready == FALSE, str_c("<b>",description,"</b>"), 
                          description)) %>%
     mutate(status = ifelse(ready==FALSE, splitButtons(idnum), "Ready")) %>%
     select(look, description, uniqvals, status) 
 })

 output$allFactorsDataTable <- DT::renderDataTable({

   details <- displayDetails()
   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 levels factors)", escape = FALSE)
 })

 #########
 #########
 ## output$factorVariables ----
 output$fp_factorVariables <- renderUI({
   if( is.null(input$testdataFile) || is.null(testdataFileEnv()) ) {
     return(tagList(h1("No data loaded")))
   }

   orig_md <- orig_metadata()
   return(tagList(
      dataTableOutput("allFactorsDataTable"),
      html(describe(orig_md, 
          descript = "Data", 
          exclude.missing = TRUE), scroll = TRUE),
           ##rmarkdown = TRUE

      # each row has action buttons, notify shiny when clicked on
      tags$script("$(document).on('click', '#allFactorsDataTable button',
        function () {
          Shiny.onInputChange('lastClickId',this.id);
          Shiny.onInputChange('lastClick', Math.random())
        });")))
 })


 ##############################################################
 testdataFile <- reactive({
   req(input$testdataFile)
   print("got testdata File ")
 })

 testdataFileEnv <- reactive({
   req(input$testdataFile$name)

   input_file_format <- tools::file_ext(input$testdataFile$name)
   if(input_file_format == "RData") {
     this_env <- new.env(parent <- emptyenv())
     print("323: testdataFile")
     load(input$testdataFile$datapath, envir = this_env)
     return(this_env)
     }
 })

  output$testdataFileEnvLoaded <- reactive({ !is.null(testdataFileEnv()) })

  outputOptions(output, "testdataFileEnvLoaded",  suspendWhenHidden=FALSE)
  outputOptions(output, "hasFactorVariables",  suspendWhenHidden=FALSE)

  processTestdataFile <- reactive({
      req(testdataFileEnv())

      print("processTestdataFile")
      this_env <- testdataFileEnv()
      if(is.null(this_env)) { return(NULL) }

      midpoint = median(this_env$l_md[, 1])
      lastcol = ncol(this_env$l_md) + 1
      #newcol = as.logical(this_env$l_md[ , 1] > midpoint)
      #this_env$l_md <- cbind(this_env$l_md,newcol)

      print("l_md")
      print(head(this_env$l_md,2))

      metadata(this_env$l_md)
      orig_metadata(this_env$l_md)

      mvdata(this_env$l_test)
      orig_mvdata(this_env$l_test)

      initFactorVariables()

      print(str_c("   414 393 l_test ", str_c(dim(this_env$l_test), collapse=",")))
      print(str_c("   414 394 l_md ", str_c(dim(this_env$l_md), collapse=",")))

      return(TRUE)
  })


  output$mvdataDataTable <- renderDataTable({
     #print(str_c("mvdataDataTable DATA ", nrow(mvdata())))
     data.table(mvdata())
  })

  output$metadataDataTable <- renderDataTable({
     #print(str_c("metadataDataTable DATA (orig)", nrow(orig_metadata())))
     data.table(orig_metadata())
  })

  setDetails <- function(details){
       details <- details %>% 
         mutate(idnum = 1:nrow(details))

       rdetails = details %>% dplyr::filter(ready)
       pdetails = details %>% dplyr::filter(!ready)

       af <- rdetails$description
       pf <- pdetails$description

       print(str_c("setDetails: all: ", nrow(details), " available: ", nrow(af),
        " need processing: ", nrow(pf) ))
       print(head(details))

       allOriginalFactor(details)
       availableFactors(af)
       unprocessedFactors(pf)
  }
  
  applyFactorButton <- function(ft, tp) {
    print(str_c("applyFactorButton: ", ft, " tp ", tp))

    orig_md <- orig_metadata()
    md <- metadata()

    which <- orig_md[ , ft]
    midpoint = mean(which)
    if (tp == "median") {
       #print("median")
       midpoint = median(which)
    }
    md[ , ft ] <- orig_md[ , ft] > midpoint
    metadata(md)

    uniqvals <- apply(md, 2, function(x) { length(unique(x)) } )
    details <- data_frame(uniqvals = uniqvals,
        name=colnames(md), description=colnames(md)) %>%
        mutate(ready = ifelse(uniqvals != 2, FALSE, TRUE),
               description = ifelse(ready, name, str_c(name, sep=" ")))

    setDetails(details)
  }

  applyFactor <- reactive({
     isolate({
       ft <- input$fp_pselectedFactor
       tp <- input$fp_processtype

       orig_md <- orig_metadata()
       md <- metadata()

       which <- orig_md[ , ft]
       midpoint = mean(which)
       if (tp == "median") {
         #print("median")
         midpoint = median(which)
       }
       md[ , ft ] <- orig_md[ , ft] > midpoint
       metadata(md)

       uniqvals <- apply(md, 2, function(x) { length(unique(x)) } )
       details <- data_frame(uniqvals = uniqvals,
          name=colnames(md), description=colnames(md)) %>%
          mutate(ready = ifelse(uniqvals != 2, FALSE, TRUE),
                 description = ifelse(ready, name, str_c(name, sep=" ")))

       setDetails(details)
     })
   })

  #-- EVENT input$applySplitButton ----
  observeEvent(input$applySplitButton, {
     print(str_c("applySplitButton: Selected Factor: ",
       input$fp_pselectedFactor, " Process Type: ", input$fp_processtype))

       ft <- input$fp_pselectedFactor
       tp <- input$fp_processtype

       orig_md <- orig_metadata()
       md <- metadata()

       which <- orig_md[ , ft]
       midpoint = mean(which)
       if (tp == "median") {
         print("median")
         midpoint = median(which)
       }
       md[ , ft ] <- orig_md[ , ft] > midpoint
       metadata(md)

       uniqvals <- apply(md, 2, function(x) { length(unique(x)) } )
       details <- data_frame(uniqvals = uniqvals,
          name=colnames(md),  description=colnames(md)) %>%
          mutate(ready = ifelse(uniqvals != 2, FALSE, TRUE),
                 description = ifelse(ready, name, str_c(name, sep=" ")))

       setDetails(details)
  })

  observeEvent(input$resetButton, {
     print(str_c("461: reset "))

     metadata(NULL)
     mvdata(NULL)

     availableFactors(list())
     unprocessedFactors(list())

     orig_metadata(NULL)
     orig_mvdata(NULL)

     print("465 after reset loaded")
  })

  output$loadedFilename <- renderText({
    if (!is.null(input$testdataFile)) {
      print(str_c("454: loadedFilename: ", input$testdataFile$name))
      input$testdataFile$name
    }
  })

  output$dataUploaded <- reactive({
     this_env <- testdataFile()
     if(is.null(this_env)) { return(FALSE)}
     
     print("dataUploaded")
     b <- withProgress(
       processTestdataFile(),
       message = "Processing uploaded data...")

     print("after dataUploaded")
     #b <- processTestdataFile()
     return(b)
  })

  outputOptions(output, 'dataUploaded', suspendWhenHidden=FALSE)

})
