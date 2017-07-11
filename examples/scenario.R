source("helper.R")
source("pesame.R")

library(stringr)

# load data
data <- read.csv2("../testdata/mvdata_g.csv", header = TRUE)
metadata <- read.csv2("../testdata/metadata_g.csv", header = TRUE)

#fix data - set rowname from the first column and remove that column
rownames(data) = data[, 1]
data <- data[, -1]

# select a factor and get the factor data
factor_name = "g"
factor_data  = as.factor(metadata[ , factor_name])
factor_data = metadata[ , factor_name]

# select adjustment method
adj_method = "fdr"

# create base data 
t_data = t(data)

print(dim(t_data))
print(rownames(t_data))
print(colnames(t_data))
h1 = helper.data_by_auc(t_data, factor_data, adj_method)
print("done")
