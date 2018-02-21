# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) {
	    print(paste("Installing ", new.pkg))
	    tryCatch(
         install.packages(new.pkg, dependencies = TRUE),
      warning = function(e) {
			   print(paste("WARNING INSTALLLING ", new.pkg))},
      error = function(e) {
		     print(paste("ERROR INSTALLLING ", new.pkg))}
      )
      sapply(pkg, require, character.only = TRUE)
    }
}

#Usage example
#would like to read the list of packages from a file
#packages<-c("ggplot2", "Hmisc", "dplyr")

packages <- c("devtools",
              "packrat",
              "DT",
              "lubridate",
"Hmisc",
"RColorBrewer",
"colorspace",
"data.table",
"dplyr",
"futile.logger",
"markdown",
"openxlsx",
"plotly",
"purrr",
"readr",
"readxl",
"shiny",
"shinydashboard",
"shinyjs",
"devtools",
"stringr",
"lubridate",
"skimr",
"kableExtra")
check.packages(packages)

if(!("bsplus" %in% installed.packages())){
  devtools::install_github("tamilyn/bsplus")
}
