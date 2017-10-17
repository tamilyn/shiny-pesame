library(readxl)

excelFileUI <- function(id, label = "Excel File") {
  ns <- NS(id)

  tagList(
    div( class = "excelModuleUI",
       fluidRow( fileInput(ns("file"), label)),
       fluidRow( checkboxInput(ns("col_names"), "Use col names")),
       fluidRow( uiOutput(ns("controls"))),

    fluidRow(
     numericInput(ns("startRow"), "Start Row:", 1, min = 1, max = 9900),
     numericInput(ns("startCol"), "Start Column:", 1, min = 1, max = 100)),

    fluidRow(checkboxInput(ns("transpose"), "Transpose"))))
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
    selectInput(ns("selectedSheet"), "Available Sheets", availableSheets(), multiple = FALSE)
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

    #print(paste("start row", startRow, " start Col", startCol, " trans ",
    #  input$transpose))

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
    print("availableSheets: got sheets")
    return(sheets)
  })

  output$availableSheets <- reactive({
    availableSheets()
  })

  dataframe <- reactive({
    msg <- sprintf("loading %s ", userFile()$name)
    cat(msg,"\n")

    d <- dataForSheet()
    msg <- sprintf("after loading %s ", userFile()$name)
    cat(msg,"\n")

    d
  })

  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg,"\n")
  })

  return(dataframe)
}
