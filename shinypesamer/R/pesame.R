## 2013 Copyright Alexander V. Alekseyenko
library(Hmisc)
library(colorspace)

table.wilcox.auc = function(x, fdata, alpha=0.05){
  x <- as.matrix(x)
  rnames = names(wilcox.auc(x[, 1], fdata))
  flog.info("table.wilcox.auc: rownames", 
    str_c(rnames, collapse = ", "), sep = " ")

  wt = apply(x, 2, function(yy) wilcox.auc(yy, fdata))
  wt
}

wilcox.auc = function(x, fdata, alpha = 0.05){
  # make sure two items are same length, stop otherwise
  if(length(x) != length(fdata)) {
     msg <- str_c("wilcox.auc - mismatch", length(x), nrow(x), 
         "factor = ", length(fdata), sep = " ")
     flog.info(msg)
     stop(msg)
     return(NULL)
  }

  w = rcorr.cens(x, fdata)
  
  auc = w['C Index']
  se = w['S.D.']/2 # Dxy to AUC transformation
  low = auc + qnorm(alpha)*se
  high = auc + qnorm(1-alpha)*se
  p.value = wilcox.test(x~fdata)$p.value
  ns = tapply(fdata,fdata,length)
  res = c("n1" = as.numeric(ns[1]), 
          "n2" = as.numeric(ns[2]), 
          "auc" = as.numeric(auc), 
          "se" = as.numeric(se), 
          "low" = as.numeric(low), 
          "high" = as.numeric(high), 
          "p.value" = p.value)
  res
}

wilcox.auc.barplot = function(wt, f, ticks = 5){
  oo = order(wt["auc",])
  wt = wt[,oo, drop=F]
  lims = round(min(0.5, max(abs(c(wt["high",], wt["low",])-0.5))), 2)
  bar.lengths = wt["auc",,drop=F]-0.5
  cols = c("red", "green")[(bar.lengths>0.0)+1]
  ys = barplot(c(bar.lengths), horiz=T, axes=F, xlim=c(-lims, lims), col=cols, xlab="AUC", names.arg = colnames(bar.lengths))
  ticks = seq(-lims, lims, length.out=ticks)
  labels = format(ticks+0.5, digits=3)
  axis(1, at= ticks, labels=labels)
  legend("topleft", bty="n", legend=levels(f)[1], text.col='black', horiz=T)
  legend("topright", bty="n", legend=levels(f)[2], text.col='black', horiz=T)
  segments(wt["low",] - 0.5, ys, wt["high",] - 0.5, ys, col="gray")
  abline(v=0, lwd=5)
}

auc.se = function(theta, n, p=0.5){
  q1 = theta/(2-theta)
  q2 = 2*theta^2/(1+theta)
  na = n*p
  nn = n*(1-p)
  sqrt((theta * (1-theta) + (na-1)*(q1-theta^2) + (nn-1)*(q2-theta^2))/(nn*na))
}
# auc.se(.7, 10)

# Predictive Effect Size Analysis in Multivariate Ensembles (PESAME)

# x = matrix(runif(20*10), nrow=20, ncol=10)
# y = matrix(runif(19*10), nrow=19, ncol=10)
# f = factor(c(rep(0, dim(x)[1]), rep(1, dim(y)[1])))
# xx = rbind(x,y)

#rnames = colnames(wilcox.auc(xx[,1], f))
#wt = apply(xx, 2, function(z) wilcox.auc(z, f))
#rownames(wt) = rnames
#wilcox.auc.barplot(wt, f)

