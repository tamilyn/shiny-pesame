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

    tabsetPanel(type="tabs",
      tabPanel("Plot", plotlyOutput("filteredPlotly")),
      tabPanel("Data", dataTableOutput("computedDataTable")) 
    ))))

