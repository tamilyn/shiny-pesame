# /Users/tamicrawford/projects/pdash3/pdash0930/server.R

library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(dplyr)
library(purrr)
library(futile.logger)
library(stringr)
library(DT)
library(data.table)
library(Hmisc)
library(openxlsx)
library(readxl)

source("util.R")
source("plotly_bar.R")

flog.appender(appender.file("pesame.log"))

shinyServer(function(input, output, session) {

  source('pesame.R', local = TRUE)
  source('helper.R', local = TRUE)

  flog.info("starting")

  rlist <- callModule(excelFile, "importedFile" )

  # load_data ----
  # show Loading panel until initialized
  load_data <- function() {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }
  load_data()

  ###-- BEGIN reactive values ----
  availableFactors <- reactiveVal(list())
  allOriginalFactor <- reactiveVal(list())
  factorDetails <- reactiveVal(list())
  computedDetails <- reactiveVal(list())

  metadata <- reactiveVal(NULL)
  mvdata <- reactiveVal(NULL)

  orig_metadata <- reactiveVal(NULL)
  ###-- END reactive values ----

  theSelectedFactor <- reactive({
    req(input$selectedFactor)

    flog.info(str_c("selected factor: [", input$selectedFactor, "]", sep=""))

    fd <- metadata()
    fd[, input$selectedFactor]
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    flog.info("97: baseFilteredData")
    tft <- mvdata()

    flog.info(str_c("100: baseFilteredData: selected Factor:",
       theSelectedFactor(),
       "method", input$adj_method, sep = " "))

    sf <- theSelectedFactor()
    f <- as.matrix(t(sf))
    flog.info(str_c("sel factor dim", str_c(dim(sf), collapse=" , ")))
    flog.info(str_c("factor ", str_c(length(f), collapse=" , ")))
    flog.info(str_c("tft dim", str_c(dim(tft), collapse=" , ")))

    bd = helper.data_by_auc( otut = tft, 
       f = f, input$adj_method)

    #bd = suppressWarnings(
    #     helper.data_by_auc( 
    #      otut = tft, f = f, input$adj_method))

    flog.info("got bd")

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
      flog.info("78: after converting to matrix")

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

  ###########################################################
  # filteredData ----
  # Apply filter to base data
  # Get data with adjust method applied, then apply p.value filter,
  # transpose result.
  filteredData <- reactive({
    #print("98: filteredData")
    b <- baseFilteredData()
    #print("108: filteredData")
    if(is.null(b)) {
      flog.info("130: filteredData: NO BASE FILTERED DATA")
      return(NULL)
    }

    if (nrow(b) == 0) {
      flog.info("127: no data")
      return(NULL)
    }

    dd = as.data.frame(b)
    ticks = generateTicks(dd)
    f = theSelectedFactor()

    dd$Enriched = levels(f)[(dd$auc<.5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })

  ###########################################################
  # output$metadataTable ----
  output$factorizedMetadataDataTable <- renderDataTable({
     req(metadata())

     #action <- dataTableAjax(session, metadata())
     print(str_c("factor metadataDataTable DATA metadata()", nrow(metadata())))
     DT::datatable(metadata())
  })

  ###########################################################
  output$citation <- renderText({
        a <- citation("ggplot2")
        b <- a[1]
        msg <- str_c(b$author, ". ", b$title, ".", b$publisher, ". ",
           b$year, ".")
	msg
  })

  ###########################################################

  # use plot_ly to display data
  # generatePlot_ly ----
  generatePlot_ly <- function(dd) {
    flog.info(paste("211: generatePlot_ly"))
    if(length(dd) <= 0) {
      flog.error(paste("126: returning empty plot_ly,no rows"))
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
    flog.info("FILTERED PLOTLY")
    dd <- filteredData()
    if(is.null(dd) || length(dd) == 0) {
       flog.info("filteredPlotly: NO DATA")
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

    totalRows <- nrow(mvdata())

    msg <- paste("Displaying ", numDisplayed , " of ", totalRows,
                 " total rows using ", input$adj_method,
                 " significance value ",  input$signficance_threshold)
    flog.info(msg)
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
      flog.info(str_c("MEDIAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "median")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "mean") {
      row_to_split <- as.numeric(gsub("mean_","",lid))
      flog.info(str_c("MEAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "mean")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "modify") {
      flog.info("MODIFY")
      row_to_modify <- as.numeric(gsub("modify_","",lid))
      #print(str_c("MODIFYING that row", lid, row_to_modify, sep = " "))
      #showModal(modal_modify)
    }
   })



 ##
 # observeEvent input$lastClick ----
  observeEvent(input$lastGroupClick, {
    lid = input$lastGroupClickId

    flog.info("lastGroupClick DISABLED FOR NOW until we make the button operate on the right df")
    print("DISABLED FOR NOW until we make the button operate on the right df")
    return(NULL)

    if (lid %like% "median") {
      row_to_split <- as.numeric(gsub("median_","",lid))
      flog.info(str_c("MEDIAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "median")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "mean") {
      row_to_split <- as.numeric(gsub("mean_","",lid))
      flog.info(str_c("MEAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "mean")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (lid %like% "modify") {
      flog.info("MODIFY")
      row_to_modify <- as.numeric(gsub("modify_","",lid))
      #print(str_c("MODIFYING that row", lid, row_to_modify, sep = " "))
      #showModal(modal_modify)
    }
   })


 
 computeDisplayDetails <- reactive({
   flog.info("370: computeDisplayDetails: (of factors)")
   details <- allOriginalFactor() %>%
     arrange(ready) %>%
     mutate(look = ifelse(ready == FALSE, str_c("<b>",description,"</b>"), 
                          description)) %>%
     mutate(status = ifelse(ready==FALSE, splitButtons(idnum), "Ready")) # %>%
     #select(look, description, uniqvals, status) 
 })

 displayDetails <- reactive({
   d <- computedDetails()
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

   flog.info("fp_factorVariables")
   orig_md <- orig_metadata()

   if(is.null(orig_md) ) {
     return(h1("No factors to show because no meta data loaded"))
   }

   details <- displayDetails() 
   return(tagList(
      div(str_c("All variables: ", str_c(details$name, collapse = ", "))),
      tabsetPanel(type="tabs", 

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

  output$mvdataDataTable <- renderDataTable({
     req(mvdata())
     DT::datatable(mvdata())
  })


  # output#metadataUI ----
  output$metadataUI <- renderUI({
     req(orig_metadata())

     h1("metadataUI")
     DT::datatable(orig_metadata())
  })

  output$metadataDataTable <- renderDataTable({
     req(orig_metadata())
     DT::datatable(orig_metadata())
  })

  setDetails <- function(details){
       #details <- details %>% 
       #  mutate(idnum = 1:nrow(details))

       rdetails = details %>% dplyr::filter(ready)
       pdetails = details %>% dplyr::filter(!ready)

       af <- rdetails$description
       pf <- pdetails$description

       flog.info(str_c("setDetails: all: ", nrow(details), 
                " available: ", nrow(rdetails),
        " need processing: ", nrow(pdetails) ))
       flog.info(head(details))

       allOriginalFactor(details)
       availableFactors(af)
  }
  
  applyFactorButton <- function(ft, tp) {
    flog.info(str_c("applyFactorButton: ", ft, " tp ", tp))

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

  #-- EVENT input$setDataButton ----
  observeEvent(input$setDataButton, {
    df <- rlist()$dataframe()
    flog.info(str_c("SET DATA nrows: ", nrow(df)))

    mvdata(df)
  })

  #-- EVENT input$setMetadataButton ----
  observeEvent(input$setMetadataButton, {
    df <- rlist()$dataframe()
    flog.info(str_c("SET META DATA nrow: ", nrow(df)))

    # convert any character to factor
    if(!is.data.frame(df)) {
      flog.info("setMetadataButton: converting to data frame")
      df <- as.data.frame(df)
    }

    if(is.data.frame(df)) {
      df <- df %>% 
         mutate_if(is.character, as.factor) 
    }

    metadata(df)
    orig_metadata(df) 

    if(ncol(df) >= 1) {
       details <- new_factor_details(df) 
       details <- details %>% 
         mutate(idnum = 1:nrow(details))

       factorDetails(details)
       setDetails(details)

       cd <- computeDisplayDetails()
       computedDetails(cd)
     }
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
