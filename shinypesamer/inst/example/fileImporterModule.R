library(readxl)
library(dplyr) 
library(stringr) 
library(readr) 

# module UI ----

# depending on file type (csv, xlsx)
importFileUI <- function(id) {
  ns <- NS(id)

  tagList(h1("import file ui"), 
	  uiOutput(ns("controls")))
}

# Display table 
importDisplayFileUI <- function(id){
  ns <- NS(id)
  tagList(h1("import display filetable"), 
	  uiOutput(ns("importDataTable")))
}

#####
# module server functions ----
fileImporterFile <- function(input, output, session, fi, fileOptions ) {
  
  ns <- session$ns
  fileVal <- fi
  fileOptions <- fileOptions

  userFile <- reactive({
    # If no file is selected, do nothing
    # validate(need(input$file, message = FALSE))

    file <- input$file 
    if(is.null(file)) {
       return(NULL)
    }

    ext <- tools::file_ext(file$name)
    #print(str_c("SETTING FILENAME TO ", file$name))
    fileVal(file$name)
    flog.info(str_c("userFile:", ext, file$name, sep = " "))
    #print(str_c("35: userFile:", ext, file$name, sep = " "))
    file
  })

  #Excel: a list of data frames with sheets 
  #CSV: a list of data frames (?)
  #RData: a list of variables in the data frame (?)
  #Returns a list, contents depends on the type of file

  startRow <- reactive({ input[[ "startRow" ]] })
  endRow <- reactive({ input[[ "endRow" ]] })
  startCol <- reactive({ input[[ "startCol" ]] })
  endCol <- reactive({ input[[ "endCol" ]] })

  samplesAreRows <- reactive({ input[[ "samplesAreRows" ]] })
  selectedSheet <- reactive({ input[[ "selectedSheet" ]] })
  file <- reactive({input[[ "file" ]] })

  ## use renderUI to display table
  output$table <- renderDataTable({
     #ns <- session$ns
     # dependencies - depending on type
     flog.info("output$table")
     d <- dataframe()
     flog.info("output$table", nrow(d))
     DT::datatable(d, options = list(pageLength = 5),
        caption = "Data Set Features", escape = FALSE)
 })

#  mytable <- reactive({
#     ext <- extension()
#     return(ext == "xlsx" || ext == "csv" ? "table")
#        tbl <- dataTableOutput(ns("table"))
#  }
  output$importDataTable <- renderUI({
    flog.info("importDataTable: renderUI")
    ns <- session$ns
    tbl <- ""
    if(!is.null(userFile())) {
      ext <- extension()
      if( ext == "xlsx") {
        tbl <- dataTableOutput(ns("table"))
      } else if( ext == "csv") {
	dataTableOutput(ns("table"))
      }
    }

    verticalLayout(
      textOutput(ns("description")),
      h4("before tbl"),
      dataTableOutput(ns("table")),
      h4("after tbl"))
  })

  ##--
  rdataControls <- reactive({
     tagList(
     "RDATA CONTROLS",
     p("list variables in the envionment?"))
  })

  rdsControls <- reactive({
     tagList(
     "RDS",
     p("list the variables in the envionment?"))
  })

  csvControls <- reactive({
    tagList(
      fileInput(ns("file"), 'File'),
      checkboxInput(ns("row_names"), "Use row names"),
      checkboxInput(ns("col_names"), "Use column names"),
      checkboxInput(ns("samplesAreRows"), "Samples are rows", value = TRUE))
  })

  output$controls <- renderUI({
    ns <- session$ns
    file <- userFile()
    ext <- extension()

    if( !is.null(userFile()) ) {
      #if(ext == "xlsx") return(excelControls())
      if(ext == "RData") return(rdataControls())

      if(ext == "rds") return(rdsControls())
      if(ext == "csv") return(csvControls())
    }

    if(!is.null(userFile())) {
      print(str_c("controls FILE NAME ", userFile()$name))
      ext <- tools::file_ext(userFile()$name)
    }

    sheets <- availableSheets()

    tagList(
      fileInput(ns("file"), 'File'),

      if( !is.null(sheets))  
        tagList(
          checkboxInput(ns("col_names"), "Use column names"),
          checkboxInput(ns("row_names"), "Use row names"),
          selectInput(ns("selectedSheet"), "Select Sheet",
             sheets, multiple = FALSE),
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

  readCsvData <- reactive({
      d <- read_csv(userFile()$datapath, col_names = input$col_names)

      # if row_names is selected, row names must be unique with no NA
      if(input$row_names) {
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
      if("samplesAreRows" %in% names(input)&& input$samplesAreRows) {
         flog.info(str_c("YES checking for samples are rows ", input$samplesAreRows ))
         d <- t(d)
         flog.info(str_c("===>  transpolosed", d))
         return(d)
      }
      flog.info(str_c("NO checking for samples are rows", input$samplesAreRows))
      flog.info(str_c("RETURNING checking for samples are rows ",
                         str_c(str(d,1), collapse = " ")))
      return(d)
  })


  rawDataForSheet <- reactive({
    #this will be based on the type of extension
    #as will data from sheet (which should be preppedData
    if(is.null(userFile())) return (NULL)

    ext <- extension()

    if(ext == "xlsx") {
       sheet = input$selectedSheet
       sheets = NULL
       if(is.null(sheet)) { 
          #sheet = 1
          #sheets <- readxl::excel_sheets(userFile()$datapath)
       }

       if(is.null(sheet)) {
         d <- readxl::read_excel(userFile()$datapath, 
               col_names = input$col_names) 
       } else { 
         d <- readxl::read_excel(userFile()$datapath,
           sheet = sheet, col_names = input$col_names) 
       }

         if(input$row_names) {
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

      d <- read_csv(userFile()$datapath, col_names = input$col_names)
      if(input$row_names) {
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


    } else if(ext == "rdata") {
      d <- data_frame(A=1:4)
    } else if(ext == "RData") {
      d <- data_frame(BB=1:4)
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
    flog.info(str_c("215: availableSheets ", str_c(sheets, collapse=", ")))
    return(sheets)
  })

  output$availableSheets <- reactive({
    availableSheets()
  })

  dataframe <- reactive({
    ext <- extension()
    print(str_c("dataframe() DATA FRAME EXIST IS ", ext))
    if(is.null(ext)) return(NULL)

    if(extension() == "xlsx") {
      dlist <- dataForExcel()
      dlist[["dataframe_data"]]
    } else if(extension() == "csv") {
      readCsvData()
    } else {
      print("data frame NOT XLSX or csv")
      return(NULL)
    }
  })

  # observe toggle of samplesAreRows...

  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

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
    if(!is.null(fileVal())) {
      return(str_c("File loaded: ", fileVal()))
    }

    if(is.null(userFile()$file)){
      return("No file loaded")
    }

    req(userFile())
    ext <- extension()
    oext <- optionsForExtension(ext)
    str_c("File", userFile()$name, "Type", extension(), oext, sep = " " )
  })

  output$description <- reactive({
    description()
  })

  output$filename <- reactive({
    req(userFile())

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
               selectedSheet = selectedSheet())

       # how about adding start row, start col
       dx <- list("dataframe" = dataframe, 
                 "filename" = fileVal(),
                 "description" = description)
       d <- append(dx, excel_options)
       fileOptions(d)
    } else {
      d <- list("dataframe" = dataframe, 
                "filename" = fileVal(),
                "description" = description)
       flog.info("389: SETTING FILE OPTIONS")
       fileOptions(d)
    }
    flog.info("392: SETTING FILE OPTIONS")
    d
  })
  return(datalist)
}
