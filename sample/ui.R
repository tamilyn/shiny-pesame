# /Users/tamicrawford/projects/pdash3/sample/ui.R
library(shiny)
library(shinydashboard)
library(plotly)
library(markdown)
library(RColorBrewer)
library(DT)

source("global.R")

# Header ------------------------------------------------------------
header <- dashboardHeader(
  title = "PESAME"
)

# Analyze ------------------------------------------------------------
analyze_panel <- verticalLayout(

  conditionalPanel("!output.hasFactorVariables",
     h1("No available factors")),

  conditionalPanel("output.hasFactorVariables",
  fluidRow(
    column(4, selectInput("adj_method",
		label = "Comparison Method", choices = method.options,
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
     #tabPanel("Plot", plotOutput("filteredPlot")),
     tabPanel( "Citation", tags$div(
       h1("Citation Information"),
         pre( textOutput("citation") ))))))

# factor_panel ----
factor_panel <- 
  mainPanel( uiOutput("fp_factorVariables"))

# Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Load Data", tabName = "generate_panel", icon = icon("cog")),
    menuItem("Process Factors", tabName = "factor_panel", icon = icon("cog")),
    menuItem("Analyze", tabName = "analyze_panel", icon = icon("area-chart"))
  ))


# combine panel ----
load_panel <-
    mainPanel(
      fluidRow(fileInput('testdataFile', 'RData file')),

      fluidRow( 
        conditionalPanel("!output.testdataFileEnvLoaded",
           p("Load data and wait for it to load.")),

        conditionalPanel("output.testdataFileEnvLoaded",
        tabsetPanel(type = "tabs",
           tabPanel("Data", DT::dataTableOutput("mvdataDataTable")),
           tabPanel("Metadata", DT::dataTableOutput("metadataDataTable")),
           tabPanel("Factorized Metadata", 
                   DT::dataTableOutput("factorizedMetadataDataTable"))))))

# Body ----
body <- dashboardBody(
  tags$head(
    tags$title('PESAME Dashboard'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'dash.css')),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(wellPanel( includeMarkdown("overview.md")))),
    tabItem(tabName = "generate_panel", load_panel ),
    tabItem(tabName = "factor_panel",  factor_panel ),
    tabItem(tabName = "analyze_panel", analyze_panel )
  ))

# Dashboard ----------------------------------------------------------
dashboardPage(
  header,
  sidebar,
  body)
