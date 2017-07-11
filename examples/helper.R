
helper.setpar = function() {
  par(oma = c(1, 4, 1, 2))  # outside margins, bot,l,top,right
  par(mar = c(1, 1, 1, 1))  # margins
  par(las = 1)   #label axis always perpendicular to the axis
  par(mgp=c(5,1,0))
}

#make the method a parameter
#  get aux by applying the method, order by aux
helper.data_by_auc = function(otut,f, method='fdr') {
  res = table.wilcox.auc(otut, f)
  res = rbind(res, p.adjust = p.adjust(res["p.value",], method=method))
  ordered <- order(res["auc",])
  res <- res[,ordered,drop=F]
}

