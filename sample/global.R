library(RColorBrewer)

source("excelFileModule.R")

## fileFormats ---- 
fileFormats = c('text/csv',
                'text/comma-separated-values', 
                'text/tab-separated-values', 
                'text/plain','.csv','.tsv',
                '.xlsx', '.xls')


## fileFormats ---- 
rdsFileFormats = c( '.Rds' )

trim <- function(x) gsub("^\\s+|\\s+$","",x)

## significance.options ----
significance.options <- list(
  "5.5"           = 5.5,
  "0.5"           = 0.5,
  "0.2"           = 0.2,
  "0.1"           = 0.1,
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

method.options = list( "bonferroni" = "bonferroni","fdr" = "fdr")

method.options.default = "fdr"


#----
input.options = list( "Excel" = "excel", 
                      "CSV" = "csv", 
                      "Rdata" = "rdata")

input.options.default = "excel"
  
#----
bar_colors = brewer.pal(5, "Set2")

to_color <- function(q) {
    if(q == "AD") {
      r = bar_colors[1]
    } else {
      r = bar_colors[2]
    }
    return(r)
}


