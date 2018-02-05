
library(shiny)
library(bsplus)

analyze_panel <- tagList(
  div(id="analyze_no_data", h1("No available factors")),
  hidden(div(id="analyze_has_data", 
    fluidRow(
      column(4, selectInput("adj_method",
		label = "Comparison Method", choices = method.options,
		selectize = FALSE,
		size = 1,
      selected = method.options.default)),
      column(4, selectInput(inputId = "signficance_threshold",
		label= "Significance Threshold",
                selectize = FALSE,
		size = 1,
		choices = significance.options,
                selected = significance.options.default )) ,
      column(4,  uiOutput("factorVariables") )),

    uiOutput("dataTabPanel"))))


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
      tags$li(class="navbar-text", tags$b(textOutput("statusMessage"))))))


load_data_content_side <-
   div("Select data and meta data source",
      hidden(actionButton(class="btn-block", "setDataButton", "Set Data")),
      hidden(actionButton(class="btn-block", "setMetadataButton", "Set Metadata")),
      hidden(actionButton(class="btn-block", "resetButton", "Reset")))

citation_side <-
   div("")

citation_main <-
  div(textOutput("citation"))

load_data_content_main <-
  tagList(

   div(id = "loading_page", h1("Loading...")),
   hidden(
    div(id = "main_content", importFileUI('inputFile'))))



# factor_panel ----
factor_panel <- tagList(
  fluidRow(column(4, uiOutput("fp_selectFactorVariables")),
           column(4, uiOutput("selectFactorButtons")),
           column(4, uiOutput("selectedFactorDescription" ))),
  uiOutput("fp_factorVariables"))

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
    bs_accordion_sidebar(id = "sections",
                         spec_side = c(width = 2, offset = 0),
                         spec_main = c(width = 10, offset = 0)) %>%
      bs_append( title_side = "Load data",
                 content_side = load_data_content_side,
                 content_main = load_data_content_main) %>%
      bs_append( title_side = "Factors",
                 content_side = "Select factors",
                 content_main = factor_panel ) %>%
      bs_append( title_side = "Analyze",
                 content_side = "View graphs",
                 content_main = analyze_panel) %>%
      bs_append( title_side = "Citation",
                 content_side = citation_side,
                 content_main = citation_main),

    use_bs_tooltip(),
    use_bs_popover(),
    use_bs_accordion_sidebar())
))
