library(readxl)
library(dplyr)
library(stringr)
library(readr)

# module UI ----
importFileUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("controls")))
}

importDisplayFileUI <- function(id){
  ns <- NS(id)
  tagList(uiOutput(ns("importDataTable")))
}

#####
# module server functions ----
excelFile <- function(input, output, session) {
  ns <- session$ns

  userFile <- reactive({
    # If no file is selected, do nothing
    validate(need(input$file, message = FALSE))
    input$file
  })

  startRow <- reactive({input[[ "startRow" ]] })
  startCol <- reactive({input[[ "startCol" ]] })
  transpose <- reactive({input[[ "transpose" ]] })

  ## use renderUI to display table
  output$table <- renderDataTable(dataframe())

  output$importDataTable <- renderUI({

    tbl <- "no data loaded"
    if(!is.null(input$file)) {
      ext <- extension()
      if( ext == "xlsx") {
        tbl <- dataTableOutput(ns("table"))
      }
      if( ext == "csv") {
        tbl <- dataTableOutput(ns("table"))
      }
    }

    verticalLayout(
      textOutput(ns("description")),
      tbl)
  })

  ##--
  rdataControls <- reactive({
     tagList(
     "RDATA CONTROLS",
     p("list the variables in the envionment?"))
  })


  rdsControls <- reactive({
     tagList(
     "RDS",
     p("list the variables in the envionment?"))
  })

  output$controls <- renderUI({

    file <- input$file
    ext <- extension()

  if(!is.null(input$file)) {
    #if(ext == "xlsx") return(excelControls())
    if(ext == "RData") return(rdataControls())
    if(ext == "rds") return(rdsControls())
    #if(ext == "csv") return(csvControls())
    }

    if(!is.null(input$file)) {
      print(str_c("FILE NAME ", input$file$name))
      ext <- tools::file_ext(input$file$name)
    }

    sheets <- availableSheets()

    tagList(
      fileInput(ns("file"), 'File'),

      if( !is.null(sheets))  
           checkboxInput(ns("col_names"), "Use col names"),
      if( !is.null(sheets))  
           selectInput(ns("selectedSheet"), "Select Sheet",
             sheets, multiple = FALSE),
      if( !is.null(sheets))  
           checkboxInput(ns("transpose"), "Transpose", value = FALSE),
      
      if( !is.null(sheets))  
      fluidRow(
      column(6,
       numericInput(ns("startRow"), "Start Row:", 1, min = 1, max = 9900)),
      column(6,
       numericInput(ns("startCol"), "Start Column:", 1, min = 1, max = 100))))
  })

  rawDataForSheet <- reactive({
    #this will be based on the type of extension
    #as will data from sheet (which should be preppedData

    if(is.null(input$file)) return (NULL)

    ext <- extension()

    if(ext == "xlsx") {
       d <- readxl::read_excel(userFile()$datapath,
         sheet = input$selectedSheet,
         col_names = input$col_names)
    } else if(ext == "csv") {
      d <- read_csv(userFile()$datapath)
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

  dataForSheet <- reactive({
    d <- rawDataForSheet()

    # these options are only available once a file has
    # been loaded
    if("transpose" %in% names(input)) {
      startCol <- as.integer(input$startCol)
      startRow <- as.integer(input$startRow)
      trans <- input$transpose
      if(trans == TRUE) {
        d <- t(d)
      }

      if(startRow > 1 && startRow < nrow(d)) {
         d <- d[ startRow:nrow(d), ]
      }

      if(startCol > 1 && startCol < ncol(d) ) {
         d <- d[ , startCol:ncol(d)]
      }
    }
    d
  })

  availableSheets <- reactive({
    if(is.null(input$file)) return(NULL)
    ext <- extension()
    if(ext != "xlsx") return(NULL)

    sheets <- readxl::excel_sheets(userFile()$datapath)
    return(sheets)
  })

  output$availableSheets <- reactive({
    availableSheets()
  })

  dataframe <- reactive({
    ext <- extension()
    #print(str_c("DATA FRAME EXIST IS ", ext))
    if(is.null(ext)) return(NULL)

    if(extension() == "xlsx") {
      dataForSheet()
    } else if(extension() == "csv") {
      rawDataForSheet()
    } else {
      print("data frame NOT XLSX or csv")
    }
  })


  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })

  extension <- reactive({
    if(!is.null(input$file)) {
      print(str_c("FILE NAME ", input$file$name))
      ext <- tools::file_ext(input$file$name)
      return(ext)
    }
    return(NULL)
  })

  optionsForExtension <- function(ext) {
    if(ext == "xlsx") {
       str_c("Sheet", input$selectedSheet, 
              "Transpose", input$transpose,
              "Start Row", input$startRow,
              "Start Col", input$startCol,
              sep = " "
              )
    } else {
      return("")
    }
  }

  description <- reactive({
    if(is.null(input$file)){
      return("No file loaded")
    }

    req(input$file)
    ext <- extension()
    oext <- optionsForExtension(ext)
    str_c("File", input$file$name, 
       "Type", extension(), oext, sep = " " )
  })

  output$description <- reactive({
    description()
  })

  output$filename <- reactive({
    req(input$file)

    str_c("File", input$file$name, "Type", extension(),
       "Sheet", input$selectedSheet, sep = " " )
    
  })

  output$extension <- reactive({
    if(!is.null(input$file)) {
      return(tools::file_ext(input$file$name))
    }
    return(NULL)
  })

  selectedSheet <- reactive({
     req(input$file)
     input$selectedSheet
  })

  datalist <- reactive({
    list("dataframe" = dataframe,
         "description" = description)
  })

  return(datalist)

}
