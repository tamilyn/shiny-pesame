#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("csvFileModule.R")
source("excelFileModule.R")

# Define UI for application that draws a histogram
fluidPage(

   tags$head(
    tags$title('Excel Module'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'dash.css')),
   
   # Application title
   titlePanel("Excel Module Demo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        #csvFileInput("datafile", "user data (.csv format)"),
        excelFileUI("excelfile", "Excel file"),
        excelFileUI("file2", "Second excel file (optional)")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("statusMessage"),
        tabsetPanel(type = "pills",
          #dataTableOutput("table"),
          tabPanel("Excel1", id = "excel1", class = "excelmodule",
            dataTableOutput("excelTable")),
          tabPanel("Excel2", id="excel2", class = "excelmodule",
            dataTableOutput("excelTable2"))
      ))
   )
)

