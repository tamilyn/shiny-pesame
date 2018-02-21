source("install-packages.R")

library(shiny)
library(shinyjs)
library(bsplus)
library(dplyr)
library(plotly)
library(futile.logger)
library(DT)
library(stringr)
#library(shinypesamer)

source("fileImporterModule.R")

## fileFormats ----
fileFormats = c('text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain','.csv','.tsv',
                '.xlsx', '.xls')


## significance.options ----
significance.options <- list(
  "All"           = "All",
  "0.05"          = 0.05,
  "0.01"          = 0.01,
  "0.001"         = 0.001,
  "0.0001"        = 0.0001,
  "0.00001"       = 0.00001,
  "0.000001"      = 0.000001,
  "0.0000001"     = 0.0000001,
  "0.00000001"    = 0.00000001,
  "0.000000001"   = 0.000000001)


significance.options.default = significance.options[3]

#----
method.options.all = list("holm" = "holm",
     "hochberg" = "hochberg",
     "hommel" = "hommel",
     "bonferroni" = "bonferroni",
     "BH" = "BH",
     "BY" = "BY",
     "fdr" = "fdr",
     "none" = "none")

method.options = list( "bonferroni" = "bonferroni",
                      "fdr" = "fdr",
                      "none" = "none")

method.options.default = "fdr"

#----
input.options = list( "Excel" = "excel",
                      "CSV" = "csv",
                      "Rdata" = "rdata")

input.options.default = "excel"


