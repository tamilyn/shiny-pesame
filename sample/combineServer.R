library(shiny)
library(rio)

combineserver <- function(input, output) {
  
  today = Sys.Date()
  now = date()
  print( paste( "shiny server started: ", now))
  
  
  ################################################################
  # Loads a file
  ################################################################
  load_file <- function(input_file) {
    input_file_format <- tools::file_ext(input_file$name)
    if(input_file_format == "xlsx") {
      d <- rio::import(file = input_file$datapath
                       ,format = input_file_format
                       ,readxl = FALSE
      )
      print(paste("rownames: ", rownames(d)))
    } else {
      d <- rio::import(file = input_file$datapath
                       ,format = input_file_format
      )
    }
    
    nc <- length(colnames(d))
    nr <- length(rownames(d))
    print(paste("there are ", nc, " cols and ", nr, " rows"))
    d
  }
  
  ##############################################################
  # Get the main data by importing a file
  # The first column is used as the row name (and and then removed)
  ##############################################################
  mvdata <- reactive({
    
    if (is.null(input$datasetFile$datapath)) {
      return(NULL)
    }
    
    d <- load_file(input$datasetFile)
    rownames(d) <- d[,1]
    d <- d[,-1]
  })
  
  # read csv or text
  ##############################################################
  # Get the metadata by importing a file
  # The first column is used as the row name (and and then removed)
  ##############################################################
  metadata <- reactive( {
    
    if (is.null(input$metadataFile$datapath)) {
      return(NULL)
    }
    
    #k <- read.csv2(input$metadataFile$datapath,header=TRUE)
    k <- load_file(input$metadataFile)
    
    nc <- length(colnames(k))
    nr <- length(rownames(k))
    print(paste("METADATA there are ", nc, " cols and ", nr, " rows"))
    rownames(k) <- k[,1]
    k
  })
  
  combineFiles <- function( outputName = "combined") {
    
    dd <- mvdata()
    mm <- metadata()
    
    if(is.null((dd)) || is.null(mm)) {
      print("missing one of the inputs")
      # popup a notification
      return(NULL)
    }
    
    if ( all(rownames(mm) == colnames(dd)) ) {
      oname <- paste(trim(outputName),".RData",sep="")
      print(paste("VALIDATED: colnames match row names, saving to ",oname))
      cc <- save(dd,mm,file=oname)
    } else {
      print(paste("combine FAILED: colnames do not match row names: ", 
                  rownames(mm), " data col = ", colnames(dd)))
      nc <- length(colnames(dd))
      nr <- length(rownames(mm))
      
      print(paste("there are ", nc, " cols and ", nr, " rows"))
      
      msg <- paste("combine FAILED: colnames do not match row names: ", 
            rownames(mm), " data col = ", colnames(dd))
      showModal(modalDialog(
        title = "ERROR - Important message",
        msg
      ))
    }
  }
  
  
  # When update button is pressed, combine the files
  observeEvent(input$updateButton, {
    if (is.null(input$outputName))
      return(NULL)

    combineFiles(  input$outputName )
  })
  
  output$mvdataDataTable <- renderDataTable({mvdata()})
  output$metadataDataTable <- renderDataTable({metadata()})
}
