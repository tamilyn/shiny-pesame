library(readxl)

excelFileUI <- function(id, label = "Excel File") {
  ns <- NS(id)

  verticalLayout( div(class = "excelModuleUI",
    fileInput(ns("file"), label),
    checkboxInput(ns("col_names"), "Use col names"),
    uiOutput(ns("controls"))))
}

# module server functions
excelFile <- function(input, output, session) {
  userFile <- reactive({
    # If no file is selected, do nothing
    validate(need(input$file, message = FALSE))
    input$file
  })

  output$controls <- renderUI({
    ns <- session$ns
    tagList(
      selectInput(ns("selectedSheet"), "Available Sheets", 
              availableSheets(), multiple = FALSE),
      checkboxInput(ns("transpose"), "Transpose"),
      fluidRow(
      column(6,
       numericInput(ns("startRow"), "Start Row:", 1, min = 1, max = 9900)),
      column(6,
       numericInput(ns("startCol"), "Start Column:", 1, min = 1, max = 100))))
  })

  rawDataForSheet <- reactive({
    d <- readxl::read_excel(userFile()$datapath, sheet = input$selectedSheet, 
         col_names = input$col_names)
    msg <- sprintf("dataForSheet loading %s sheet %s", userFile()$name,
        input$selectedSheet)
    cat(msg,"\n")
    d
  })

  dataForSheet <- reactive({
    d <- rawDataForSheet()
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
    d
  })

  availableSheets <- reactive({
    sheets <- readxl::excel_sheets(userFile()$datapath)
    return(sheets)
  })

  output$availableSheets <- reactive({
    availableSheets()
  })

  dataframe <- reactive({
    dataForSheet()
  })

  return(dataframe)
}
