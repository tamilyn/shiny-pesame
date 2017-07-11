library(stringr)
library(phyloseq)

source("helper.R")
source("pesame.R")

bb = readRDS("../testdata/soilrep2.Rds")

data = bb@otu_table
cc = as.matrix(sample_data(bb))

# select a factor and get factor data
factor_name = "clipped"
factor_data = as.factor(cc[  , "clipped"])

adj_method = "fdr"

t_data = t(data)

print(dim(t_data))
print(head(rownames(t_data)))
print(head(colnames(t_data)))
h1 = helper.data_by_auc(t_data, factor_data, adj_method)
print("done")

