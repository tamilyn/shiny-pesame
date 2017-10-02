#
#
#
library(dplyr)
library(purrr)

source("generate.R")
source("util.R")



td <- create_test_data()
source("f.R")

prepped_list <- factorize_data(td)




