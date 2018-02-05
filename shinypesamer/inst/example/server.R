#
#
flog.appender(appender.file("z.log"))

source("newutil.R")

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

    flog.info(str_c("selected factor:[", input$selectedFactor, "]"))

    fd <- metadata()
    if(!input$selectedFactor %in% names(fd)) {
      flog.info("theSelectedFactor not available in this dataset.")
      return(NULL)
    }

    # probably a call for pull
    # other <- fd %>% pull(input$selectedFactor)
    # other does not have names, otherswise the values look the same
    # not sure those names are needed
    fd %>% pull(input$selectedFactor)
    #fdata <- fd[, input$selectedFactor]
    #fdata <- unlist(fdata)
    #fdata
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
    flog.info(str_c("93: baseFilteredData: sf[", sf, "]"))
    if(length(sf) == 0) {
      flog.info("98: baseFilteredData: no factor data")
      #browser()
      return(NULL)
    }

    otut <- otut_for_processing()
    if(is.null(otut)) {
      flog.info("102: baseFilteredData: no otut")
      return(NULL)
    }

    flog.info(str_c("72: baseFilteredData otut null?", is.null(otut),
                    "dim", str_c(dim(otut), collapse=" , "), sep =  " "))
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

    dd$Enriched = levels(sf)[(dd$auc < .5) + 1]
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


    #nn <- list(level0="pink", level1="lightblue")
    dd$color <- map_chr(dd$auc, ~ ifelse(. < 0.5, "pink", "lightblue"))

    p <- dd %>%
      plot_ly( ) %>%
      add_bars( x = ~dd$heights,
                y = ~dd$Names,
                # need to specify the legend labels
                color = ~dd$color,
                #name = "Enriched",
                orientation = "h",
                hoverinfo = "text",
                text = hover.text,
                showlegend = FALSE
                # , marker = list(color = ~dd$color)
                ) %>%
      add_markers( x = ~dd$low - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE ) %>%
      add_markers( x = ~dd$high - 0.5, y = ~dd$Names, marker = list(symbol=142, color="black"), showlegend = FALSE,
                   hoverinfo = "none" ) %>%
      add_segments( x = ~dd$low - 0.5, xend = ~dd$high - 0.5, y = ~dd$Names, yend = ~dd$Names, color = toRGB("black"),
                    showlegend = FALSE,  hoverinfo = "none") %>%
      layout(title =NULL, xaxis = my.xaxis, margin = list(l=120,t=50,b=30,unit="pt",pad=2),
           yaxis = list(title=""))

      ## add text with the labels

      #browser()
    p
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

  ###-- panel at bottom of Analyze panel
  output$dataTabPanel = renderUI({
   validate( need(!is.null(otut_for_processing) &&
                  !is.null(input$selectedFactor),
                  "No data - Data and meta data must be loaded"))

    tabsetPanel(type="tabs",
      tabPanel("Plot", plotlyOutput("filteredPlotly")),
      tabPanel("Table", dataTableOutput("computedDataTable")))
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

  observeEvent(input$applyMean, {
     applyFactorButton(input$selectedViewFactor, "mean")
  })

  observeEvent(input$applyMedian, {
     applyFactorButton(input$selectedViewFactor, "median")
  })

 ### output$factorsDataTable
 output$factorsDataTable <- function() {
   library(kableExtra)
   library(skimr)

   details <- computedDetails()
   orig_fd <- orig_metadata()

   my_skim <- skim(orig_fd)
   my_skim_1 <- my_skim  %>%
     dplyr::filter(stat == "hist") %>%
     select(variable,Histogram=formatted)

   details2 <- details %>%
     left_join(my_skim_1, by=c("name"="variable")) %>%
     knitr::kable("html") %>%
     kable_styling("striped", full_width = FALSE)
 }

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
   return(str_c("available factors: ", length(af)))
 })

 output$describeDataFiles <- renderText({ describeDataFiles() })
 output$describeMetaDataFiles <- renderText({ describeMetaDataFiles() })
 output$describeFactors <- renderText({ describeFactors() })
 output$statusMessage <- renderText({ statusMessage() })

 output$fp_selectFactorVariables <- renderUI({
    af <- computedDetails()
    if(!is.null(af)) {
       af <- af %>% pull(name)
       selectInput('selectedViewFactor', 'Factor', af )
    }
 })

 selectedFactorValues <- reactive({
      orig_metadata() %>% pull(input$selectedViewFactor) %>% unique
 })

 output$selectedFactorDescription <- renderUI({
   if(is.null(input$selectedViewFactor)) {
     return(NULL)
   }

   selected <- input$selectedViewFactor
   af <- computedDetails()
   glimpse(af)

   this_factor <- af %>% filter(name == selected)
   if(this_factor$type == "numeric") {
      return(NULL)
   }

   # based on type of factor
   tagList(
     textInput("level0Label", "Level 0 Label"),
     textInput("level1Label", "Level 1 Label"),
     div(actionButton("applyFactorGroupButton","Apply")))
 })

 output$selectFactorButtons <- renderUI({
   validate( need(input$selectedViewFactor,
                  "No factors to show because no meta data loaded"))

    selected <- input$selectedViewFactor

    details <- allOriginalFactor()
    kind <- details %>%
            dplyr::filter(name == input$selectedViewFactor) %>%
            dplyr::pull(type)

    uniq_vals <- orig_metadata() %>% dplyr::pull(selected) %>% unique

    if(kind=="numeric") {
     tagList(
        actionButton("applyMean", "Mean"),
        actionButton("applyMedian", "Median"))
    } else {
      tagList(
        #need to initialize with the current settings
        checkboxGroupInput("level0checkboxes",
          "Select Level 0 Values", selectedFactorValues()))
    }
 })

 #########
 #########
 ## output$factorVariables ----
 output$fp_factorVariables <- renderUI({
   validate( need(orig_metadata(),
                  "No factors to show because no meta data loaded"))

   return(tableOutput("factorsDataTable"))
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
       allOriginalFactor(details)
       availableFactors(rdetails$name)
  }

  applyFactorButtonGroup <- function(ft, level0) {
    flog.info(str_c("applyFactorButtonGroup: [", ft, "] level0 ", level0))
    # 1. update the data
    # 2. update the factor table

    # orig_md - original meta data
    # md - orig_md with factorized data (values converted to TRUE/FALSE)
    orig_md <- orig_metadata()
    md <- metadata()

    selected_factor <- orig_md %>% pull(ft)
    new_factor <- map_lgl(selected_factor, ~ !(. %in% level0))

    md[ , ft ] <- new_factor
    metadata(md)

    details <- allOriginalFactor()

    new_descript <- str_c("Factor group applied")

    tp <- "factorgroup"

    n_df <- data_frame(name=ft,
                       new_ready=TRUE,
                       new_method_applied="factorgroup",
                       des=new_descript)

    k_df <- details %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
            mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
            mutate(description = ifelse(is.na(des), description, des)) %>%
            mutate(method_applied =
              ifelse(is.na(new_method_applied),
                    method_applied, new_method_applied)) %>%
            select(-new_ready, -des, -new_method_applied)

    details <- z_df

    cd <- computedDetails() %>%
            mutate(ready, ifelse(name==ft,TRUE,ready)) %>%
            mutate(method_applied, ifelse(name==ft,tp,method_applied))  %>%
            mutate(description, ifelse(name==ft,new_descript,description))


    k_df <- computedDetails() %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
      mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
      mutate(description = ifelse(is.na(des), description, des)) %>%
      mutate(method_applied =
               ifelse(is.na(new_method_applied),
                      method_applied, new_method_applied)) %>%
      select(-new_ready, -des, -new_method_applied)

    cd <- z_df

    flog.info("UPDATING GROUP computedDetails")
    #browser()
    computedDetails(cd)
    setDetails(details)
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

    details <- allOriginalFactor()

    mp <- round(midpoint,2)
    new_descript <- str_c("0 = below or at ", tp,
        " 1 = above (", mp, ")",sep = "")

    n_df <- data_frame(name=ft,
                       new_ready=TRUE,
                       new_method_applied=tp,
                       des=new_descript)

    k_df <- details %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
            mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
            mutate(description = ifelse(is.na(des), description, des)) %>%
            mutate(method_applied =
              ifelse(is.na(new_method_applied),
                    method_applied, new_method_applied)) %>%
            select(-new_ready, -des, -new_method_applied)

    details <- z_df

    cd <- computedDetails() %>%
            mutate(ready, ifelse(name==ft,TRUE,ready)) %>%
            mutate(method_applied, ifelse(name==ft,tp,method_applied))  %>%
            mutate(description, ifelse(name==ft,new_descript,description))


    k_df <- computedDetails() %>% left_join(n_df, by=c("name"="name"))

    z_df <- k_df %>%
      mutate(ready = ifelse(is.na(new_ready), ready, new_ready)) %>%
      mutate(description = ifelse(is.na(des), description, des)) %>%
      mutate(method_applied =
               ifelse(is.na(new_method_applied),
                      method_applied, new_method_applied)) %>%
      select(-new_ready, -des, -new_method_applied)

    cd <- z_df

    flog.info("UPDATING computedDetails")
    #browser()
    computedDetails(cd)
    setDetails(details)
  }

  observeEvent(input$applyFactorGroupButton, {
    #need to apply this
    applyFactorButtonGroup(input$selectedViewFactor, input$level0checkboxes)
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

       computedDetails(factors_df)
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
