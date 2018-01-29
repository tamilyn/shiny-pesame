#
#
flog.appender(appender.file("z.log"))

source("newutil.R")
source("plotly_bar.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  source("helper.R")
  source("pesame.R")

  mainData <- reactiveVal(list())
  metaData <- reactiveVal(list())

  fi <- reactiveVal(NULL)
  fileOptions <- reactiveVal(list())

  rlist <- callModule(fileImporterFile, "inputFile", fi, fileOptions )

  # load_data ----
  # show Loading panel until initialized
  load_data <- function() {
    shinyjs::hide("loading_page")
    shinyjs::show("main_content")
  }

  load_data()

  #-- EVENT input$setDataButton ----
  observeEvent(input$setDataButton, {
    r <- rlist()
    if(is.null(rlist())) {
      return(NULL)
    }

    df <- rlist()$dataframe()
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

    # probably a call for pull
    # other <- fd %>% pull(input$selectedFactor)
    # other does not have names, otherswise the values look the same
    # not sure those names are needed
    fdata <- fd[, input$selectedFactor]
    fdata <- unlist(fdata)
    #browser()
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

    otut <- otut_for_processing()
    flog.info(str_c("72: baseFilteredData",
       "otut dim", str_c(dim(otut), collapse=" , "), sep =  " "))

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
     DT::datatable(filteredData())
  })

  ###########################################################
  # output$metadataTable ----
  output$factorizedMetadataDataTable <- renderDataTable({
     req(metadata())

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
    #print(unique(dd$color))

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

    totalRows <- nrow(mvdata())
    msg <- paste("Displaying ", numDisplayed , " of ", totalRows,
                 " total rows using ", input$adj_method,
                 " significance value ",  input$signficance_threshold)
    flog.info(msg)
    msg
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


 ### categoricalSplitButtons 
 ### on Assign, should popup a window 
 categoricalSplitButtons <- function(idnum) {
  paste0('<div class="btn-group" role="group" aria-label="Basic example">
   <button type="button" class="btn btn-primary xbtn-secondary modify"id=group_',idnum,'>Group</button>
   </div>')
 }

 categoricalSplitButtonsAssign <- function(idnum) {
  paste0('<div class="btn-group" role="group" aria-label="Basic example">
   <button type="button" class="btn btn-primary xbtn-secondary modify"id=assign_',idnum,'>Assign</button>
   </div>')
 }

  output$row_assign <- DT::renderDataTable({
    selected_row = as.numeric(gsub("assign_","",input$lastClickId))
    flog.info(str_c("row assign SELECTED: ", selected_row))
    
    details <- computedDetails() %>% dplyr::filter(idnum == selected_row)
    uv <- details %>% pull(unique_values) %>% str_split(",") %>% unlist
    df <- data_frame(variable=uv,label=uv)

    datatable(df, options = list(pageLength = 25),
     caption = "Assign Labels", escape = FALSE)

#    old_row=vals$Data[selected_row]
#    row_change=list()
#    for (i in colnames(old_row)) {
#      if (is.numeric(vals$Data[[i]])) {
#          row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
#      } else
#        row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
#      }
#    row_change = as.data.table(row_change)
#    setnames(row_change,colnames(old_row))
#    DT = rbind(old_row,row_change)
#    rownames(DT) <- c("Current values","New values")
#    DT
#    },escape=F,options=list(dom='t',ordering=F),selection="none"
    })

  # Modification Modal Dialog ----
  modal_assign_categorical <- modalDialog(
    fluidPage(
      h3(strong("Assign Groups"), align = "center"),
      hr(),
      dataTableOutput('row_assign'),
      actionButton("save_changes","Save changes") #,
      #tags$script(HTML("$(document).on('click', '#save_changes',
      #   function () {
      #      var list_value=[]
      #      for (i = 0; i < $( '.new_input' ).length; i++) {
      #          list_value.push($( '.new_input' )[i].value)
      #      }
      #      Shiny.onInputChange('newValue', list_value)
      #  });"))
    ),
    size="l")


  # Modification Modal Dialog ----
  modal_group_categorical <- modalDialog(
    fluidPage(
      h3(strong("Group Categorical"), align = "center"),
      hr() #,
      #dataTableOutput('row_modif'),
      #actionButton("save_changes","Save changes"),
      #tags$script(HTML("$(document).on('click', '#save_changes',
      #   function () {
      #      var list_value=[]
      #      for (i = 0; i < $( '.new_input' ).length; i++) {
      #          list_value.push($( '.new_input' )[i].value)
      #      }
      #      Shiny.onInputChange('newValue', list_value)
      #  });"))
    ),
    size="l")

 ### observeEvent input$lastClick ----
  observeEvent(input$lastClick, {
    lid = input$lastClickId

    if (str_detect(lid, "median")) {
      row_to_split <- as.numeric(gsub("median_","",lid))
      applyFactorButton(row_to_split, "median")
    } else if (str_detect(lid, "mean")) {
      row_to_split <- as.numeric(gsub("mean_","",lid))
      applyFactorButton(row_to_split, "mean")
    } else if (str_detect(lid, "assign_")) {
      row_to_modify <- as.numeric(gsub("assign_","",lid))
      showModal(modal_assign_categorical)
    } else if (str_detect(lid, "group_")) {
      row_to_modify <- as.numeric(gsub("group_","",lid))
      showModal(modal_group_categorical)
    } else {
      flog.error(str_c("input$lastClick: OTHER: ", lid))
    }
   })

 getFactorTableDetails <- function(is_continuous) {
   if(is_continuous) {
      details <- computedDetails() %>% dplyr::filter(type == "numeric")
   } else {
      details <- computedDetails() %>% dplyr::filter(type != "numeric")
   }

   details <- details %>% 
           select(name, unique_values, method_applied, ready, description,status)

   datatable(details, options = list(pageLength = 25),
     caption = "Data Set Features (analysis can only be performed on 2 level factors)", escape = FALSE)
 }

 ### output$groupFactorsDataTable 
 output$groupFactorsDataTable <- DT::renderDataTable({
   getFactorTableDetails(FALSE)
 })

 ### output$continuousFactorsDataTable 
 output$continuousFactorsDataTable <- DT::renderDataTable({
   getFactorTableDetails(TRUE)
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
      f1msg <- str_c(fn, " ", f1$selectedSheet())
    } else {
      f1msg <- str_c(fn)
    }

    #if("startRow" %in% names(f1)) {
    #  f1msg <- str_c(f1msg, " start row", f1$startRow() )
    #}

    #if("endRow" %in% names(f1)) {
    #  f1msg <- str_c(f1msg, " end row ", f1$endRow() )
    #}

    #if("samplesAreRows" %in% names(f1)) {
    #  f1msg <- str_c(f1msg, " Samples are Rows ", f1$samplesAreRows() )
    #}
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
   validate( need(orig_metadata(), 
                  "No factors to show because no meta data loaded"))

   return(tagList(
      tabsetPanel(type="tabs",
        tabPanel("Continuous",
          dataTableOutput("continuousFactorsDataTable")),

        tabPanel("Group",
          dataTableOutput("groupFactorsDataTable"))),

      # each row has action buttons, notify shiny when clicked on
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

  output$mvdataDataTable <- renderDataTable({
     req(mvdata())
     DT::datatable(mvdata())
  })

  ### output$metadataDataTable 
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
       availableFactors(rdetails$name)
  }

  applyFactorButton <- function(ft, tp) {
    flog.info(str_c("applyFactorButton: ", ft, " tp ", tp))

    # 1. update the data 
    # 2. update the factor table

    # orig_md - original meta data
    # md - orig_md with factorized data (values converted to TRUE/FALSE)
    orig_md <- orig_metadata()
    md <- metadata()

    selected_factor <- orig_md %>% pull(ft) 
    if (tp == "median") {
       midpoint = median(selected_factor, na.rm = TRUE)
    } else {
       midpoint = mean(selected_factor, na.rm = TRUE)
    }
    md[ , ft ] <- orig_md[ , ft] > midpoint
    metadata(md)

    orig <- allOriginalFactor()

    this_row <- orig[ft, ]
    this_row$method_applied = str_c("applied ", tp) 
    this_row$ready = TRUE
    orig[ft, ] <- this_row

    details <- orig

    mp <- round(midpoint,2)
    cd <- computedDetails()

    details[ ft, "ready" ] <- TRUE

    cd[ ft, "ready" ] <- TRUE
    cd[ ft, "method_applied" ] <- tp
    cd[ ft, "description" ] <-
        str_c("0 = below or at ", tp, "<br/>",
        "1 = above (", mp,")",sep = "")

    flog.info("UPDATING computedDetails")

    computedDetails(cd)
    setDetails(details)
  }

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

  getButtons <- function(type,idnum,ready) {
    print(str_c("getButtons: ", type, " ", idnum, " ", ready))
    flog.info(str_c("getButtons: ", type, " ", idnum, " ", ready))
    if(type == "numeric") {
      splitButtons(idnum) 
    } else {
      categoricalSplitButtons(idnum, ready) 
    }
  }

  getButtonsForRow <- function(r ) {
          print(r)
          browser()
    print(str_c("R ", r, r$type, r$idnum, r$ready, sep=" "))
    flog.info(str_c("R ", r, r$type, r$idnum, r$ready, sep=" "))
    return("ROW")
  }

  #-- EVENT input$setMetadataButton ----
  observeEvent(input$setMetadataButton, {
    df <- rlist()$dataframe()

    metaData(fileOptions())
    if(!is.data.frame(df)) {
      df <- as.data.frame(df)
    }

    if(nrow(df) == 0){
      return
    }

    metadata(df)
    orig_metadata(df)

    # reset everything to empty
    availableFactors(list())
    allOriginalFactor(list())
    factorDetails(list())
    computedDetails(list())

    if(ncol(df) >= 1) {
       factors_df <- identify_factors(df)

       factorDetails(factors_df)
       setDetails(factors_df)

       cd <- factors_df
       cd <- factors_df %>%
          mutate(status = 
                 ifelse(type == "numeric", 
                            splitButtons(idnum), 
                            ifelse(ready, 
                            categoricalSplitButtonsAssign(idnum), 
                            categoricalSplitButtons(idnum) 
                            ))) 
       computedDetails(cd)
       setDetails(factors_df)
    }
  })

  #-- EVENT input$applySplitButton ----
  observeEvent(input$applySplitButton, {

       ft <- input$fp_pselectedFactor
       tp <- input$fp_processtype

       orig_md <- orig_metadata()
       md <- metadata()

       which <- orig_md[ , ft]
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
