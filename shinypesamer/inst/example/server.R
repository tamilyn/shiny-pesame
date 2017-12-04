#
#
flog.appender(appender.file("z.log"))

source("util.R")
source("plotly_bar.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  source("helper.R")
  source("pesame.R")

  # finished loading
  fi <- reactiveVal(NULL)
  fileOptions <- reactiveVal(list())
  mainData <- reactiveVal(list())
  metaData <- reactiveVal(list())

  flog.info("starting server")
  rlist <- callModule(fileImporterFile, "inputFile", fi, fileOptions )

  # load_data ----
  # show Loading panel until initialized
  load_data <- function() {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")

    flog.info("finished loading data")
  }

  load_data()
  
  #-- EVENT input$setDataButton ----
  observeEvent(input$setDataButton, {
    r <- rlist()
    if(is.null(rlist())) {
      print("no data loaded")
      return(NULL)
    }

    df <- rlist()$dataframe()
    flog.info(str_c("set data: ", str_c(names(fileOptions()), collapse = ", ")))
    flog.info(str_c("SET DATA nrows: ", nrow(df)))

    mainData(fileOptions())
    mvdata(df)
  })


##################
 

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
    req(input$selectedFactor, metadata())

    flog.info(str_c("selected factor: [", input$selectedFactor, "]", sep=""))

    fd <- metadata()
    if(!input$selectedFactor %in% names(fd)) {
      flog.info("theSelectedFactor not available in this dataset.")
      return(NULL)
    }
    
    fdata <- fd[, input$selectedFactor]
    fdata <- unlist(fdata)
    fdata
  })

  otut_for_processing <- reactive({
    tft <- mvdata()
    if(is.null(tft)) {
      return(NULL)
    }
    return(as.matrix(tft))
  })

  # baseFilteredData ----
  # filter data by significance threshold and transpose
  baseFilteredData <- reactive({
    req(input$signficance_threshold)

    sf <- theSelectedFactor()
    if(length(sf) == 0) {
      flog.info("98: baseFilteredData: no factor data")
      return(NULL)
    }

    flog.info(str_c("64: baseFilteredData: selected Factor:",
       length(sf), "method", input$adj_method, sep = " "))

    otut <- otut_for_processing()
    flog.info(str_c("72: baseFilteredData", 
       "otut dim", str_c(dim(otut), collapse=" , "), 
       sep =  " "))

    ft = tryCatch({
        suppressWarnings(helper.data_by_auc( otut = otut, fdata = sf, input$adj_method)) 
        }, warning = function(w) {
         flog.warn(str_c("110 baseFilteredData helper warning ", w))
         NULL
        }, error = function(w) {
         flog.error(str_c("113 baseFilteredData helper error ", w))
         #browser()
         NULL
        })

    if(is.null(ft)) {
      # need to display an error message
      flog.info("118: baseFilteredData NULL")
      return(NULL)
    }
    thres <- input$signficance_threshold
    flog.info(str_c("78: after calling data_by_auc",
       "ft dim", str_c(dim(ft), collapse=" , "), 
       "significance threshold: ", thres,
       sep =  " "))

    if(thres != "All") {
      thres <- as.numeric(input$signficance_threshold)
      filt = (ft["p.adjust", ] < thres) & !is.na(ft["p.adjust", ])
      result <- t(ft[, filt, drop=F])
    } else {
      result <- t(ft)
    }
    result
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
  # converting to data frame and augmenting
  filteredData <- reactive({
    req(baseFilteredData())
    b <- baseFilteredData()
    if (nrow(b) == 0) {
      flog.info("127: filteredData: no data")
      return(NULL)
    }

    dd = as.data.frame(b)
    ticks = generateTicks(dd)
    sf = theSelectedFactor()

    dd$Enriched = levels(sf)[(dd$auc<.5) + 1]
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])
    dd
  })

  ###########################################################
  # output$metadataTable ----
  output$computedDataTable <- renderDataTable({
     req(filteredData())

     flog.info(str_c("filteredData ", nrow(filteredData())))
     DT::datatable(filteredData())
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
        msg <- str_c(b$author, ". ", b$title, ".", b$publisher, ". ", b$year, ".")
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
    req(input$signficance_threshold, input$selectedFactor)

    dd <- filteredData()
    flog.info(str_c("FILTERED PLOTLY: dd length ==> null? ", is.null(dd), " length", length(dd)))
    if(is.null(dd) || length(dd) == 0) {
       flog.info("filteredPlotly: NO DATA")
       return(NULL)
    }

    sf = theSelectedFactor()
    dd$Enriched = levels(sf)[(dd$auc<.5) + 1]

    dd$heights = dd$auc - 0.5
    dd$auc1 = dd$heights
    dd$Names = factor(rownames(dd), levels=rownames(dd)[order(dd$auc)])

    generatePlot_ly(dd)
  })



  check_data_dims <- reactive({
    otut <- otut_for_processing()
    fdata <- theSelectedFactor()
    if( nrow(otut) != length(fdata) ) {
      msg = str_c("Factor length (", length(fdata), 
        ") must be same as number of rows (", nrow(otut), ")" )
      return(msg)
    } 
    return(NULL)
  })

  statusMessage <- reactive({
    d <- mvdata()
    if(is.null(d)) {
      return(NULL)
    }
   
    totalRows <- nrow(mvdata())

    fd <- baseFilteredData()
    if(is.null(fd)) {
      numDisplayed <- 0
      m <- check_data_dims()
      if(is.null(m)) {
      return("No data (after apply significance threshold)")
      } else {
        return(m)
      }
    } else {
      numDisplayed <- nrow(fd)
    }

    cd <- computedDetails()
    flog.info(str_c("status message cd:", nrow(cd)))

    totalRows <- nrow(mvdata())
    msg <- paste("Displaying ", numDisplayed , " of ", totalRows,
                 " total rows using ", input$adj_method,
                 " significance value ",  input$signficance_threshold,
              " cols ", nrow(cd))
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

    print(paste("observeEvent$lastClick", input$lastClickId ))
    flog.info(paste("observeEvent$lastClick", input$lastClickId ))
    #popup with name, method, 
 
    if (str_detect(lid, "median")) {
      row_to_split <- as.numeric(gsub("median_","",lid))
      flog.info(str_c("MEDIAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "median")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (str_detect(lid, "mean")) {
      row_to_split <- as.numeric(gsub("mean_","",lid))
      flog.info(str_c("MEAN row", lid, "row", row_to_split, sep = " ") )
      applyFactorButton(row_to_split, "mean")
      #vals$Data <- vals$Data[-row_to_delete]
    } else if (str_detect(lid, "modify")) {
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
   })


 computeDisplayDetails <- reactive({
   flog.info("370: computeDisplayDetails: (of factors)")

   details <- allOriginalFactor() %>%
     arrange(ready) %>%
     mutate(look = ifelse(ready == FALSE, str_c("<b>",description,"</b>"), 
                          description)) %>%
     mutate(status = ifelse(ready==FALSE, splitButtons(idnum), "Ready")) %>%
     select(look, description, type, uniqvals, name, status ) 
 })

 output$allFactorsDataTable <- DT::renderDataTable({
   details <- computedDetails()
   if(nrow(details) == 0) {
      return(NULL)
   }

   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
 })


 output$groupFactorsDataTable <- DT::renderDataTable({
   dtl <- computedDetails()

   flog.info("GROUP FACTOR DISPLAY DETAILS")

   details <- computedDetails() %>% filter(type != "numeric")
   if(nrow(details) == 0) {
      return(NULL)
   }

   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
   
 })


 output$continuousFactorsDataTable <- DT::renderDataTable({

   details <- computedDetails() %>% dplyr::filter(type == "numeric")
   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
 })

 describeDataFile <- function(f1) {
   if(is.null(f1)) {
    f1msg <- "No file"
   } else if (is.null(names(f1))) {
    f1msg <- "No file"
   } else {
    fn <- "no file"
    ss <- "no sheet"
    if("filename" %in% names(f1)) {
       fn <- f1[["filename"]]
    }
    if("selectedSheet" %in% names(f1)) {
       ss <- str_c("Sheet: ", f1[["selectedSheet"]])
    }

    if("selectedSheet" %in% names(f1)) {
      f1msg <- str_c(fn, " ", ss)
    } else {
      f1msg <- str_c(fn)
    }
   }
   f1msg
 }

 describeDataFiles <- reactive({
   f1 <- mainData()
   describeDataFile(f1)
 })


 describeMetaDataFiles <- reactive({
   f1 <- metaData()
   describeDataFile(f1)
 })

 describeFactors <- reactive({
   af <- availableFactors()
   if(is.null(af) || length(af) == 0) {
     return("No factors")
   }
   return(str_c("availabe factors: ", length(af)))
 })

 output$describeDataFiles <- renderText({ describeDataFiles() })
 output$describeMetaDataFiles <- renderText({ describeMetaDataFiles() })
 output$describeFactors <- renderText({ describeFactors() })
 output$statusMessage <- renderText({ statusMessage() })

 #########
 #########
 ## output$factorVariables ----
 output$fp_factorVariables <- renderUI({

   flog.info("fp_factorVariables")
   orig_md <- orig_metadata()

   if(is.null(orig_md)) {
     return(h1("No factors to show because no meta data loaded"))
   }

   details <- computedDetails() 
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

  setDetails <- function(details) {
       rdetails = details %>% dplyr::filter(ready)
       pdetails = details %>% dplyr::filter(!ready)

       flog.info(str_c("setDetails: all: ", nrow(details), 
                " available: ", nrow(rdetails),
            " need processing: ", nrow(pdetails) ))

       allOriginalFactor(details)
       availableFactors(rdetails$description)
  }
  
  applyFactorButton <- function(ft, tp) {
    flog.info(str_c("applyFactorButton: ", ft, " tp ", tp))

    orig_md <- orig_metadata()
    md <- metadata()

    which <- orig_md[ , ft] %>% unlist
    if (tp == "median") {
       midpoint = median(which)
    } else {
       midpoint = mean(which)
    }
    md[ , ft ] <- orig_md[ , ft] > midpoint
    metadata(md)

#####

    uniqvals <- apply(md, 2, function(x) { length(unique(x)) } )
    orig <- allOriginalFactor()
    details <- data_frame( uniqvals = uniqvals,
                   name=colnames(md), 
        description=colnames(md)) %>%
        mutate(idnum = 1:ncol(md)) %>%
        mutate(ready = ifelse(uniqvals != 2, FALSE, TRUE),
               description = ifelse(ready, name, str_c(name, sep=" ")))

    if(!is.null(orig)) {
      details$methodapplied = orig$methodapplied
      details <- details %>% 
         mutate(methodapplied = ifelse(name == ft, str_c("applied ", tp), methodapplied),
                ready = ifelse(name == ft,  TRUE, ready))
   }


    mp <- round(midpoint,2)
    cd <- computedDetails()
    cd[ ft, "ready" ] <- TRUE
    cd[ ft, "methodapplied" ] <- tp
    cd[ ft, "description" ] <- 
        str_c("0 = below or at ", tp, "<br/>",
        "1 = above (", mp,")",sep = "")

    flog.info("UPDATING computedDetails")

    computedDetails(cd)
    setDetails(details)
  }

  #-- EVENT input$setDataButton ----
  observeEvent(input$setDataButtonOLD, {
    df <- rlist()$dataframe()
    flog.info(str_c("SET DATA nrows: ", nrow(df)))

    mvdata(df)
  })

  #-- EVENT input$resetButton ----
  observeEvent(input$resetButton, {

    flog.info("reset")

    mvdata(NULL)
    metadata(NULL)
    orig_metadata(NULL) 
    
    availableFactors(list())
    allOriginalFactor(list())
    factorDetails(list())
    computedDetails(list())

    mainData(list())
    metaData(list())
  })

  #-- EVENT input$setMetadataButton ----
  observeEvent(input$setMetadataButton, {
    df <- rlist()$dataframe()
    flog.info(str_c("SET META DATA nrow: ", nrow(df)))

    metaData(fileOptions())

    ######### NEED TO SET THE FACTORS WHEN THE METADATA IS CHANGED

    # convert any character to factor
    if(!is.data.frame(df)) {
      flog.info("setMetadataButton: converting to data frame")
      df <- as.data.frame(df)
    }

    if(is.data.frame(df)) {
      df <- df %>% 
         mutate_if(is.character, as.factor) 
    }

    nmd <- suppressWarnings(apply(df,2,as.numeric))
    zz <- apply(nmd,2,is.numeric) 
    zznames <- names(zz)[zz]
    if(length(zznames) > 0) {
      xdf <- df %>% mutate_at(zznames, as.numeric) 
      df <- xdf
    }

    flog.info("setting metadata() and orig_metadata()")
    # save original converted to numeric
    metadata(df)
    orig_metadata(df) 
    
    availableFactors(list())
    allOriginalFactor(list())
    factorDetails(list())
    computedDetails(list())

    if(ncol(df) >= 1) {
       flog.info("ok got at least one potential factor")
       details <- new_factor_details(df) 
       details <- details %>% 
         mutate(idnum = 1:nrow(details))

       factorDetails(details)
       setDetails(details)

       cd <- computeDisplayDetails()
       flog.info("now setting computed display detils")
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
       #browser()
       if (tp == "median") {
         midpoint = median(which)
       } else {
         midpoint = mean(which)
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


  observe({

    r <- rlist()

    if(is.null(rlist())) {
      shinyjs::hide("setDataButton")
      shinyjs::hide("setMetadataButton")
      shinyjs::hide("resetButton")
    } else  {
      shinyjs::show("setDataButton")
      shinyjs::show("setMetadataButton")
      shinyjs::show("resetButton")
    }

    if(length(availableFactors()) > 0) { 
      shinyjs::show("analyze_has_data")
    } else {
      shinyjs::hide("analyze_no_data")
    }
  })

})
