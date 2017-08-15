# /Users/tamicrawford/projects/pdash3/sample/ui.R
library(shiny)
library(shinydashboard)
library(plotly)
library(markdown)
library(RColorBrewer)

source("global.R")

# Header ------------------------------------------------------------
header <- dashboardHeader(
  title = "PESAME"
)


# Analyze ------------------------------------------------------------
analyze_panel <- verticalLayout(

  fluidRow( 
    column(4, selectInput("adj_method", 
		label = "Comparison Method",
                choices = method.options, 
		selectize = FALSE, 
		size = 1,
                selected = method.options.default)),
    column(4, selectInput(inputId = "signficance_threshold", 
		label="Significance Threshold", 
                selectize = FALSE, 
		size = 1, 
		choices = significance.options, 
                selected = significance.options.default )) ,
    column(4,  uiOutput("factorVariables") )), 

  h1(verbatimTextOutput("informationMessage")), 

  tabsetPanel(type="tabs",
     tabPanel("Plotly", plotlyOutput("filteredPlotly")),
     tabPanel("Plot", plotOutput("filteredPlot")),
     tabPanel( "Citation", tags$div(
       h1("Citation Information"),
         pre( textOutput("citation") )))))

# Generate Test Data Panel -------------------------------------------

generate_panel <- 
  mainPanel(
    selectInput("dataSource", "Data Source:",
                c("Generate Random Data" = "random",
                  "Load combined rdata, otut, f" = "legacy",
                  "Load separate csv data and metadata" = "combine")),
    
    conditionalPanel(condition = "input.dataSource == 'random'", 
        fluidRow( includeMarkdown("generate_test_data.md") ),
                     
        fluidRow( column(4, numericInput("numCols", label = "Num columns", 
                                                      value = 20)),
                               column(4, numericInput("numRows", label = "Num rows", 
                                                      value = 100)),
                               column(4, actionButton("testdataButton", 
                                                      "Generate test data"))),
                     
    conditionalPanel("output.testDataGenerated", 
      tabsetPanel(
        tabPanel("Data", tableOutput("testDataTable")),
        tabPanel("Metadata", tableOutput("metadataTable")),
        tabPanel("Factorized Metadata", tableOutput("factorizedMetadataTable")))
    )))

# Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Load Data", tabName = "generate_panel", icon = icon("cog")),
    menuItem("Analyze", tabName = "analyze_panel", icon = icon("area-chart"))
  ))

# Body ----
body <- dashboardBody(
  tags$head(
    tags$title('PESAME Dashboard'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'dash.css')),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(wellPanel( includeMarkdown("overview.md")))),
    tabItem(tabName = "generate_panel", generate_panel ),
    #tabItem(tabName = "select_panel",  select_panel ),
    #tabItem(tabName = "combine_panel", combine_panel ),
    tabItem(tabName = "analyze_panel", analyze_panel )
  ))


# Dashboard ----------------------------------------------------------
dashboardPage(
  header,
  sidebar,
  body)
