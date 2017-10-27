# /Users/tamicrawford/projects/pdash3/sample/ui.R
library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(markdown)
library(RColorBrewer)
library(DT)


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
     tabPanel("Data", p("not ready")), 
     #tabPanel("Plot", plotOutput("filteredPlot")),
     tabPanel( "Citation", tags$div(
       h1("Citation Information"),
         pre( textOutput("citation") ))))))

# factor_panel ----
factor_panel <- uiOutput("fp_factorVariables")

# Sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Load Data", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("View Inputs", tabName = "load_metadata_panel", icon = icon("cog")),
    menuItem("Process Factors", tabName = "factor_panel", icon = icon("cog")),
    menuItem("Analyze", tabName = "analyze_panel", icon = icon("area-chart"))
  ))

load_metadata_panel <-
   tabsetPanel(type = "tabs", 
               id = "inputDataTabsetPanel",
      tabPanel("Data", 
         DT::dataTableOutput("mvDataTable")),
      tabPanel("Metadata", 
         DT::dataTableOutput("metadataDataTable")),
      tabPanel("Discretized", 
        DT::dataTableOutput("factorizedMetadataDataTable")))

# Body ----
body <- dashboardBody(

  useShinyjs(),

  tags$head(
    tags$title('PESAME Dashboard'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'dash.css')),

  div(id = "loading_page", h1("Loading...")),

  hidden(div(id = "main_content",
   tabItems( 
    tabItem(tabName = "dashboard",
      tagList(
      wellPanel( includeMarkdown("overview.md")),
      sidebarLayout(
        sidebarPanel(
          importFileUI('importedFile'), 
          actionButton("setDataButton", "Set Data"),
          actionButton("setMetadataButton", "Set Metadata")),
        mainPanel(
          importDisplayFileUI('importedFile'))))), 

    #tabItem(tabName = "load_data_panel", load_data_panel ),
    tabItem(tabName = "load_metadata_panel", load_metadata_panel ),
    tabItem(tabName = "factor_panel",  factor_panel ),
    tabItem(tabName = "analyze_panel", analyze_panel )
  ))))

# Dashboard ----
dashboardPage(
  header,
  sidebar,
  body)
