library(dplyr)
library(readxl)

# convert lipid spreadsheet to a pesame algorithm
devtools::load_all("~/projects/lipid/lipidr")
source("generate_test_data.R")

excel_df <- read_excel("SphCer_98818_.xlsx")
df1 <- prep_sheet(excel_df, "Summary") 

# get metadata
data_rows <- c(2,5:44)
just_data_rows <- 5:44

#column names, lipids
lipids <- df1[2, 2:19]
ids <- as.matrix(df1[5:44, 1])

# add a couple of boolean columns
df2 <- df1[just_data_rows, 17:19] %>%
   mutate(17 = as.numeric(17), 18 = as.numeric(18))
colnames(df2) <- df1[2, 17:19]

midpoint2 = df2 %>% 
midpoint3 = median(df2[, 3])
df3 <- df2 %>% mutate(f2_b=as.logical(l_md[ , 2] > midpoint2),
                      f3_b=as.logical(l_md[ , 3] > midpoint3))

l_md <- as.matrix(df1[data_rows, 17:19])
colnames(l_md) <- l_md[1, ]
l_md <- l_md[ 2:nrow(l_md), ]
rownames(l_md) <- ids
l_md <- apply(l_md, 2, as.numeric)

midpoint = median(l_md[, 1])
lastcol = ncol(tl_md) + 1
newcol = as.logical(l_md[ , 1] > midpoint)
this_env$l_md <- cbind(this_env$l_md,newcol)
print(head(this_env$l_md,2))


l_test <- as.matrix(df1[data_rows, 2:16])
l_test <- l_test[ 2:nrow(l_test),  ]
rownames(l_test) <- ids
colnames(l_test) <- lipids[1, 2:16]
l_test <- apply(l_test, 2, as.numeric)

l_test <- t(l_test)

saveRDS(l_test, "l_test.rds")
saveRDS(l_md, "l_md.rds")
save(l_test, l_md, file = "ldata.RData")

library(readr)
write.csv(l_test, "l_test.csv")
write.csv(l_md, "l_md.csv")





test_td1 = create_test_data(4,40)
test_md1 = as.matrix(create_test_metadata(4))

metadata <- readRDS("t1_metadata.rds")
p_md <- metadata %>% select(-1)
p_md <- as.matrix(p_md)
rownames(p_md) <- metadata$userid



metadata <- metadata %>% select(-1)
rownames(metadata)

dim(as.matrix(metadata))
mmm = as.matrix(metadata)

rownames(mmm) <- metadata
colnames(mmm)

rownames(md1)

# we want the rownames of the test data to be var1, var2.. 1:40
# of metadata ==> rows are sample1, sample2, cols are f,g,h  
# (probably the userid)
