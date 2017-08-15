


create_test_data <- function(numcols = 20, numrows = 100) {
  mvdata = matrix(runif(numrows*numcols), ncol = numcols, byrow=T)
  colnames(mvdata) = paste("sample", 1:numcols, sep = "")
  rownames(mvdata) = paste("var", 1:numrows, sep = "")

  #write.table(mvdata, file="mvdata.txt", sep = "\t")
  #write.csv2(mvdata, file="mvdata.csv")

  metadata = data.frame(f = factor(runif(numcols) < 0.5), 
                        g = factor(runif(numcols) < 0.5), 
                        h = runif(numcols))

  rownames(metadata) = paste("sample", 1:numcols, sep="")

  #write.table(metadata, file="metadata.txt", sep="\t")
  #write.csv2(metadata, file="metadata.csv")

  return(list("mvdata" = mvdata, "metadata" = metadata))
}
