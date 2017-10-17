# excel browser 
#

library(shiny)
library(DT)
library(data.table)

shinyServer( function(input, output, session ) {

  source("excelFileModule.R")  
  excelfile <- callModule(excelFile, "excelfile")
  file2 <- callModule(excelFile, "file2")

  output$statusMessage <- renderText(  {
    msg <- "Hello Status " 
    msg
  })

  output$excelTable <- renderDataTable({ 
    print("36: excelTable")
    f <- excelfile() 
    f
  })

  output$excelTable2 <- renderDataTable({ 
      f <- file2()
      print(head(f,2))
      DT::datatable(f)  
  })

})
