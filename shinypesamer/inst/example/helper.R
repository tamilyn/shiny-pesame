library(futile.logger)
library(stringr)

helper.setpar = function() {
  par(oma = c(1, 4, 1, 2))  # outside margins, bot,l,top,right
  par(mar = c(1, 1, 1, 1))  # margins
  par(las = 1)   #label axis always perpendicular to the axis
  par(mgp=c(5,1,0))
}

# make method a parameter
# get aux by applying the method, order by aux
helper.data_by_auc = function(otut, fdata, method = 'fdr') {
  if( nrow(otut) != length(fdata) ) {
    msg = str_c("Factor length (", length(fdata), ") must be same as number of rows (", 
                nrow(otut), ")" )
    print(msg)
    flog.error(msg)
    stop(msg)
    return(NULL)
  }
  
  fdata <- as.factor(fdata)
  flog.info(str_c("22: helper, length(fdata)", length(fdata),
       str_c(dim(otut), collapse=", ")))

  res = table.wilcox.auc(otut, fdata)
  res = rbind(res, 
         p.adjust = p.adjust(res["p.value", ], 
         method = method))

  ordered <- order(res["auc",])
  res <- res[, ordered, drop = FALSE]
}
