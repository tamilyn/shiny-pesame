# /Users/tamicrawford/projects/bstest/example
#
library(shiny)
library(bsplus)

source("analyze.R")

top_part <- tags$nav(class="navbar navbar-default navbar-fixed-top",
  div(class="container",
    tags$ul(class="nav navbar",
      tags$li(class="navbar-text navbar-brand", "PESAME"),
      tags$li(class="navbar-text", "Data", tags$br(),
                tags$b(textOutput("describeDataFiles"))),

      tags$li(class="navbar-text", "Metadata", tags$br(),
                tags$b(textOutput("describeMetaDataFiles"))),
      tags$li(class="navbar-text", "Factors",
		tags$br(), tags$b(textOutput("describeFactors"))),
      tags$li(class="navbar-text navbar-right",
           tags$b(textOutput("statusMessage")) ))
     ))


load_data_content_side <-
   div("Select data and meta data source",
      hidden(actionButton(class="btn-block", "setDataButton", "Set Data")),
      hidden(actionButton(class="btn-block", "setMetadataButton", "Set Metadata")),
      hidden(actionButton(class="btn-block", "resetButton", "Reset")))

load_data_content_main <-
  tagList(

   div(id = "loading_page", h1("Loading...")),

   hidden(
    div(id = "main_content",
      sidebarLayout(
        sidebarPanel(importFileUI('inputFile')),
        mainPanel(importDisplayFileUI('inputFile'))))))



# factor_panel ----
factor_panel <- uiOutput("fp_factorVariables")

modal_equation <-
  bs_modal(
    id = "modal_equation",
    title = "Equations",
    body = "<h1>HELLO TAMI</h1>", 
    size = "medium"
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # theme = "mytheme.css",

# Application title

  #theme = shinytheme("sandstone"),
  useShinyjs(),
  modal_equation,

  tags$head(
    tags$title('PESAME'),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css')),

  top_part,

  div(
    bs_accordion_sidebar(id = "sections") %>%
      bs_append( title_side = "Load data",
                 content_side = load_data_content_side,
                 content_main = load_data_content_main) %>%
      bs_append( title_side = "Factors",
                 content_side = "Select factors",
                 content_main = factor_panel ) %>%
      bs_append( title_side = "Analyze",
                 content_side = "View graphs",
                 content_main = analyze_panel),

    use_bs_tooltip(),
    use_bs_popover(),
    use_bs_accordion_sidebar())
))