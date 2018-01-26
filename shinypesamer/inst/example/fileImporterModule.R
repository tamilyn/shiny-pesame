library(readxl)
library(dplyr) 
library(stringr) 
library(readr) 
library(DT)

# module UI ----
importFileUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      tagList(
        fileInput(ns("file"), 'File'),
        uiOutput(ns("controls")),
        actionButton(ns("loadFile"), 'Load'))),
   mainPanel(
      verticalLayout(
        tags$span(id="error_message", class="error",
                  textOutput(ns("error"))),
        textOutput(ns("description")),
        dataTableOutput(ns("mytable")))))
}


#####
# module server functions ----
fileImporterFile <- function(input, output, session, fi, fileOptions ) {

  v = reactiveValues(data = NULL)

  fileVal <- fi
  fileOptions <- fileOptions

  userFile <- reactive({
    # If no file is selected, do nothing
    validate(need(input$file, message = FALSE))
    ext <- tools::file_ext(input$file$name)

    fileVal(input$file$name)
    flog.info(str_c("userFile:", ext, input$file$name, sep = " "))
    input$file
  })

  startRow <- reactive({ input[[ "startRow" ]] })
  endRow <- reactive({ input[[ "endRow" ]] })
  startCol <- reactive({ input[[ "startCol" ]] })
  endCol <- reactive({ input[[ "endCol" ]] })

  samplesAreRows <- reactive({ input[[ "samplesAreRows" ]] })
  selectedSheet <- reactive({ input[[ "selectedSheet" ]] })

  file <- reactive({input[[ "file" ]] })

  ## display table
  output$mytable <- renderDataTable({
     validate( need(v$data, "vvv data validate: No data"))

     #DT::datatable(v$data, options = list(pageLength = 5),
     #   caption = "Data Set Features", escape = FALSE)
     v$data
  })

  ##--
  output$controls <- renderUI({
    validate( need(userFile(), "output$controls: No data loaded"))

    ns <- session$ns
    file <- userFile()
    ext <- extension()

    if(ext == "csv") {
      return (tagList( 
        checkboxInput(ns("row_names"), "Use row names"),
        checkboxInput(ns("col_names"), "Use column names"),
        checkboxInput(ns("samplesAreRows"), 
                      "Samples are rows", value = TRUE)))

    }

    sheets <- availableSheets()
    tagList(
      if( !is.null(sheets))  
        tagList(
          selectInput(ns("selectedSheet"), "Select Sheet",
             sheets, multiple = FALSE),
          checkboxInput(ns("col_names"), "Use column names"),
          checkboxInput(ns("row_names"), "Use row names"),
          checkboxInput(ns("samplesAreRows"), "Samples are rows", value = TRUE),
          fluidRow( column(6,
            numericInput(ns("startRow"), "Start Row:", 1, min = 1, max = 9900)),
            column(6,
            numericInput(ns("startCol"), "Start Column:", 1, min = 1, max = 1000))),

          fluidRow( column(6,
            numericInput(ns("endRow"), "End Row:", 1, min = 1, max = 9900)),
            column(6,
            numericInput(ns("endCol"), "EndColumn:", 1, min = 1, max = 1000)))))
  })

  readCsvData <- function() {
      v$err = NULL
      if( !("col_names" %in% names(input))) {
         d <- suppressMessages(read_csv(userFile()$datapath,
                                        progress = FALSE))
      } else {
         d <- suppressMessages(
             read_csv(userFile()$datapath, 
               col_names = input$col_names,
               progress = FALSE))
      }

      if("row_names" %in% names(input)) {

      # if row_names selected, row names must be unique with no NA
      if(input$row_names) {
         rnames <- unlist(d[, 1])
         has_na <- sum(is.na(rnames))
         df <- d %>% dplyr::select(-1)

         if (has_na == 0 && length(unique(rnames))  == length(rnames)) {
           flog.info(str_c("using row names, they are unique"))
           rownames(df) <- rnames
           d <- df
         } else {
           flog.error(str_c("176 can't use row names, is.na and/or not unique"))
           v$err = "Warning: Can't use row names, not unique"
         }
      }
      if("samplesAreRows" %in% names(input)) {
        trans <- input$samplesAreRows
      } else {
        trans = FALSE
      }
      # if samples are rows, then transpose
      if(trans) {
         d <- tbl_df(t(d))
      }
      }
      return(d)
  }

  rawDataForSheet <- reactive({
    #this will be based on the type of extension
    #as will data from sheet (which should be preppedData
    if(is.null(userFile())) return (NULL)

    ext <- extension()
    col_names = FALSE
    if("col_names" %in% names(input)) col_names = input$col_names
    row_names = FALSE
    if("row_names" %in% names(input)) row_names = input$row_names

    if(ext == "xlsx") {
       sheet = input$selectedSheet
       sheets = NULL
       if(is.null(sheet)) { 
          #sheet = 1
          #sheets <- readxl::excel_sheets(userFile()$datapath)
       }

       if(is.null(sheet)) {
         d <- readxl::read_excel(userFile()$datapath, 
               col_names = col_names) 
       } else { 
         d <- readxl::read_excel(userFile()$datapath,
           sheet = sheet, col_names = col_names ) 
       }

         if(row_names) {
           #shift columns
           first_col <- d[,1]
           #make sure unique number of values is same as number of rows,
           #duplicates not allowed

           df <- data.frame(d)
           rnames <- unlist(first_col)
           if (length(unique(rnames))  == length(rnames)) {
               #rownames(df) <- unlist(first_col)
               df <- df[ , 2:ncol(df)]
               d <- df
           } else {
               flog.error(str_c("can't use row names, not unique"))
               warning("can't use row names, not unique")
               d
           }
         }
    } else if(ext == "csv") {
     flog.info("253 RAW DATA FOR SHEET")

      d <- read_csv(userFile()$datapath, col_names = col_names ) # input$col_names)
      if(row_names) {
         rnames <- unlist(d[, 1])
         has_na <- sum(is.na(rnames))
         df <- data.frame(d[ , 2:ncol(d)])
         if (has_na == 0 && length(unique(rnames))  == length(rnames)) {
           flog.info(str_c("using row names, they are unique"))
           rownames(df) <- rnames
           d <- df
         } else {
           flog.error(str_c("195 can't use row names, is.na and/or not unique"))
           warning("can't use row names, not unique")
           d
         }
      }
      flog.info(str_c("checking for samples are rows"))

      # if samples are rows, then transpose
      if("samplesAreRows" %in% names(input)) {
         flog.info(str_c("YES checking for samples are rows ", input$samplesAreRows ))
         d <- t(d)
         flog.info(str_c("YES checking for samples are rows ",
                         str_c(str(d,1), collapse = " ")))
         return(d)
      }
      flog.info(str_c("NO checking for samples are rows", input$samplesAreRows))
      flog.info(str_c("RETURNING checking for samples are rows ",
                         str_c(str(d,1), collapse = " ")))
      return(d)


    } else {
      d <- data_frame(NODATA=1:4)
    }

    msg <- sprintf("dataForSheet loading %s sheet %s", userFile()$name,
        input$selectedSheet)
    cat(msg,"\n")
    d
  })


  applyRowColumnFilters <- function(d) {

    startRow <- as.integer(input$startRow)
    startCol <- as.integer(input$startCol)

    endRow <- as.integer(input$endRow)
    endCol <- as.integer(input$endCol)

    nd <- nrow(d)

    flog.info(str_c("START applyRowColumnFilters: ", 
      nd, startRow, startCol, endRow, endCol,
      "DIM(d)", str_c(dim(d), collapse  = ", "), sep = " "))

    if ( (endRow > startRow) && (endRow <= nrow(d)) ) {
      lastRow <- endRow 
    } else {
      lastRow <- nrow(d)
    }
   
    if( (startRow != 1) || (lastRow != nrow(d)) ) {
       d <- d[startRow:lastRow, ]
    }

    if ( endCol > startCol && endCol <= ncol(d) ) {
      lastCol <- endCol 
    } else {
      lastCol <- ncol(d)
    }

    if( (startCol != 1 || lastCol != ncol(d)) ) {
       d <- d[, startCol:endCol]
    }

    flog.info(str_c("END applyRowColumnFilters: ", 
      startRow, startCol, endRow, endCol,
      "DIM(d)", str_c(dim(d), collapse  = ", "), sep = " "))

    d
  }

  dataForExcel <- reactive({
    
    d <- rawDataForSheet()

    # these options only available once a file has been loaded
    trans <- FALSE
    if("samplesAreRows" %in% names(input)) {
      trans <- input$samplesAreRows
      if(trans != TRUE) {
        d <- t(d)
      }
      d <- tryCatch({
         applyRowColumnFilters(d) 
      }, warning = function(w) {
         flog.error(str_c("WARNING: ", w))
         d
     }, error = function(err) { 
         flog.error(str_c("ERROR: ", err ))
         d
     })
    }
    return(list(dataframe_data = d, startRow = startRow, startCol = startCol, samplesAreRows = trans))
  })

  availableSheets <- reactive({

    if(is.null(userFile())) return(NULL)
    ext <- extension()
    if(ext != "xlsx") return(NULL)

    sheets <- readxl::excel_sheets(userFile()$datapath)
    v$sheets = sheets
    flog.info(str_c("215: availableSheets ", str_c(sheets, collapse=", ")))
    return(sheets)
  })

  output$availableSheets <- reactive({
    availableSheets()
  })

  dataframe <- reactive({
    v$data
  })

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  loadDataFile <- function() {
    if(extension() == "xlsx") {
      dlist <- dataForExcel()
      v$data <- dlist[["dataframe_data"]]
    } else if(extension() == "csv") {
      d <- readCsvData()
      v$data <- d 
    } else {
      print("data frame NOT XLSX or csv")
    }
  }

  observeEvent(input$loadFile, { loadDataFile() })
  observeEvent(input$selectedSheet, { loadDataFile() })
  observeEvent(input$file, { loadDataFile() })
  observeEvent(input$samplesAreRows, { loadDataFile() })
  observeEvent(input$col_names, { loadDataFile() })
  observeEvent(input$selectedSheet, { loadDataFile() })
  observeEvent(input$row_names, { loadDataFile() })

  extension <- reactive({
    if(!is.null(userFile())) {
      print(str_c("ext FILE NAME ", userFile()$name))
      ext <- tools::file_ext(userFile()$name)
      return(ext)
    }
    return(NULL)
  })

  optionsForExtension <- function(ext) {
    if(ext == "xlsx") {
       str_c("Sheet", input$selectedSheet, 
              "Samples Are Rows", input$samplesAreRows,
              "Start Row", input$startRow,
              "Start Col", input$startCol,
              sep = " "
              )
    } else {
      return("")
    }
  }

  description <- reactive({
    validate( need(fileVal(), "validate description: No data loaded"))
    return(str_c("File loaded: ", fileVal()))
  })

  output$error <- renderText({
    v$err 
  })

  output$description <- reactive({
    description()
  })

  output$filename <- reactive({
    validate( need(userFile(), "validate description: No filename"))
    str_c("File", userFile()$name, "Type", extension(),
       "Sheet", input$selectedSheet, sep = " " )
  })

  output$extension <- reactive({
    if(!is.null(userFile()$file)) {
      return(tools::file_ext(userFile()$name))
    }
    return(NULL)
  })

  datalist <- reactive({
    ext <- extension()
 
    if(is.null(fileVal())) {
      return(NULL)
    }

    if(ext == "xlsx") {
       excel_options = list("availableSheets" = availableSheets,
               "selectedSheet" = selectedSheet)

       dx <- list("dataframe" = dataframe, 
                 "filename" = fileVal(),
                 "startRow" = startRow,
                 "endRow" = endRow,
                 "samplesAreRows" = samplesAreRows,
                 "description" = description)
       d <- append(dx, excel_options)
       fileOptions(d)
    } else {
      d <- list("dataframe" = dataframe, 
                "filename" = fileVal(),
                "startRow" = startRow,
                "endRow" = endRow,
                "samplesAreRows" = samplesAreRows,
                "description" = description)
       fileOptions(d)
    }
    d
  })
  return(datalist)
}
