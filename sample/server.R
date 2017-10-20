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
library(openxlsx)
library(readxl)

source("util.R")
source("generate.R")
#source("generate_test_data.R")
source("ftn2.R")
source("plotly_bar.R")

shinyServer(function(input, output, session) {

  source('pesame.R', local = TRUE)
  source('helper.R', local = TRUE)

  excel_datafile <- callModule(excelFile, "excel_datafile")
  excel_metadatafile <- callModule(excelFile, "excel_metadatafile")

  ###-- BEGIN reactive values ----
  availableFactors <- reactiveVal(list())
  allOriginalFactor <- reactiveVal(list())
  factorDetails <- reactiveVal(list())
  computedDetails <- reactiveVal(list())

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
    fd[, input$selectedFactor]
  })

  get_tft <- reactive({

    print("56: get_tft NOT TRANSPOSING ANYMORE")
    mdata <- mvdata()
    if(is.null(mdata)) return(NULL)
    
    t_tft <- mdata
    ##print("62: get_tft")

    rownames(t_tft) <- colnames(mdata)
    print("65: get_tft")
    t_tft
  })

  observeEvent(input$refresh, {
      print("REFRESH")
      processDataFileSheet()
      processTestdataFile()
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    tft <- get_tft()

    # convert data
    bd = suppressWarnings(helper.data_by_auc( tft,
          theSelectedFactor(), input$adj_method))

    ft <- bd
    if(!is.matrix(ft)) {
      #print("converting to matrix")
      #print(dim(ft))
      if(is.null(ft)) {
        #print("83: HEY IT IS NULL")
        return(NULL)
      } 
      ft <- as.matrix(ft)
    }
      #print("78: after converting to matrix")

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
    #print("98: filteredData")
    b <- baseFilteredData()
    #print("108: filteredData")
    if(is.null(b)) {
      print("NO BASE FILTERED DATA")
      return(NULL)
    }
    #print("113: filteredData")

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

    dd$heights = dd$auc - 0.5
    dd$auc1 = dd$heights
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])

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

 # do this once when the data set is initially loaded
 initFactorVariables <- reactive({
   isolate({
     print("initFactorVariables")
     mf = metadata()
     #print(head(mf,2))

     cnames = colnames(mf)
     if(length(cnames) >= 1) {
       details <- new_factor_details(mf) 
       details <- details %>% 
         mutate(idnum = 1:nrow(details))
       factorDetails(details)
       setDetails(details)

       cd <- computeDisplayDetails()
       computedDetails(cd)
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
    #popup with the name, the method, 

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



 ##
 # observeEvent input$lastClick ----
  observeEvent(input$lastGroupClick, {
    lid = input$lastGroupClickId

    #print(paste("observeEvent$lastGroupClick", input$lastGroupClickId ))
    #popup with the name, the method, 


    print("DISABLED FOR NOW until we make the button operate on the right df")
    return(NULL)

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



 
 computeDisplayDetails <- reactive({
   print("370: COMPUTE DISPLA DETAILS (of factors)")
   details <- allOriginalFactor() %>%
     arrange(ready) %>%
     mutate(look = ifelse(ready == FALSE, str_c("<b>",description,"</b>"), 
                          description)) %>%
     mutate(status = ifelse(ready==FALSE, splitButtons(idnum), "Ready")) # %>%
     #select(look, description, uniqvals, status) 
 })

 displayDetails <- reactive({
   d <- computedDetails()
   print("RETURNING display details")
   head(d)
   d
 })


 output$allFactorsDataTable <- DT::renderDataTable({
   details <- displayDetails()
   if(nrow(details) == 0) {
      return(NULL)
   }
   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
 })


 output$groupFactorsDataTable <- DT::renderDataTable({
   details <- displayDetails() %>% filter(type != "numeric")
   if(nrow(details) == 0) {
      return(NULL)
   }
   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
   
 })


 output$continuousFactorsDataTable <- DT::renderDataTable({

   details <- displayDetails() %>% dplyr::filter(type == "numeric")
   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
 })

 #########
 #########
 ## output$factorVariables ----
 output$fp_factorVariables <- renderUI({

   print("fp_factorVariables")
   orig_md <- orig_metadata()

   if(is.null(orig_md) ) {
     return(h1("No factors to show because no meta data loaded"))
   }

   #print("here - data loaded")
   #print(str(orig_md,1))

   details <- displayDetails() 
   return(tagList(
      div(str_c("All variables: ", str_c(details$name, collapse = ", "))),
      tabsetPanel(type="tabs", 
        #tabPanel("Description",
        #  purrr::safely( 
        #    html(describe(orig_md, descript = "Data", 
        #       exclude.missing = TRUE), scroll = TRUE),
        #  p("descriptions unavailable"))),

        tabPanel("Continuous",
          dataTableOutput("continuousFactorsDataTable")),

        tabPanel("Group",
          dataTableOutput("groupFactorsDataTable"),
          p("Grouping features not ready"))),

      # each row has action buttons, notify shiny when clicked on
      tags$script("$(document).on('click', '#allFactorsDataTable button',
        function () {
          Shiny.onInputChange('lastClickId',this.id);
          Shiny.onInputChange('lastClick', Math.random())
        });"),

      tags$script("$(document).on('click', '#continuousFactorsDataTable button',
        function () {
          Shiny.onInputChange('lastClickId',this.id);
          Shiny.onInputChange('lastClick', Math.random())
        });"),

      tags$script("$(document).on('click', '#groupFactorsDataTable button',
        function () {
          Shiny.onInputChange('lastClickId',this.id);
          Shiny.onInputChange('lastClick', Math.random())
        });")

    ))
 })

  ##############################################################
  outputOptions(output, "hasFactorVariables",  suspendWhenHidden = FALSE)

  processDataFileSheet <- reactive({
      print("DATA updated: processDataFileSheet")
      df <- excel_datafile() 

      mvdata(df)
      orig_mvdata(df)
  })

  processTestdataFile <- reactive({
      print("METADATA updated: processTestdataFile")
      df <- excel_metadatafile() 

      # convert any character to factor
      if(is.data.frame(df)) {
        df <- df %>% 
           mutate_if(is.character, as.factor) 
      }

      metadata(df)
      orig_metadata(df) 
      initFactorVariables()

      return(TRUE)
  })

  output$mvdataDataTable <- renderDataTable({
     processDataFileSheet()
     DT::datatable(mvdata())
  })


  # output#metadataUI ----
  output$metadataUI <- renderUI({
     req(processTestdataFile(), initFactorVariables(), orig_metadata())

     h1("metadataUI")
     DT::datatable(orig_metadata())
  })

  output$metadataDataTable <- renderDataTable({
     req(processTestdataFile(), initFactorVariables(), orig_metadata())
     DT::datatable(orig_metadata())
  })

  setDetails <- function(details){
       #details <- details %>% 
       #  mutate(idnum = 1:nrow(details))

       rdetails = details %>% dplyr::filter(ready)
       pdetails = details %>% dplyr::filter(!ready)

       af <- rdetails$description
       pf <- pdetails$description

       print(str_c("setDetails: all: ", nrow(details), 
                " available: ", nrow(rdetails),
        " need processing: ", nrow(pdetails) ))
       print(head(details))

       allOriginalFactor(details)
       availableFactors(af)
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
    #set_tp
    # ft is name, eg Sph, 
    #details$methodapplied[
    orig <- allOriginalFactor()
    details <- data_frame( uniqvals = uniqvals,
                   name=colnames(md), 
        description=colnames(md)) %>%
        #mutate(methodapplied = ifelse(name == ft, str_c("applied ", tp), 
        #         methodapplied)) %>%
        mutate(idnum = 1:ncol(md)) %>%
        mutate(ready = ifelse(uniqvals != 2, FALSE, TRUE),
               description = ifelse(ready, name, str_c(name, sep=" ")))

    if(!is.null(orig)) {
      details$methodapplied = orig$methodapplied
      details <- details %>% 
         mutate(methodapplied = ifelse(name == ft, str_c("applied ", tp), 
                 methodapplied)) 
   }


    mp <- round(midpoint,2)
    cd <- computedDetails()
    cd[ ft, "ready" ] <- TRUE
    cd[ ft, "methodapplied" ] <- tp
    cd[ ft, "description" ] <- 
        str_c("0 = below or at ", tp, "<br/>",
        "1 = above (", mp,")",sep = "")
    computedDetails(cd)
    setDetails(details)
  }

  # ----
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

})
