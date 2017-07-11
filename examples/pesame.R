## 2013 Copyright Alexander V. Alekseyenko
library(Hmisc)
library(colorspace)

table.wilcox.auc = function(xx, f, alpha=0.05){
  rnames = colnames(wilcox.auc(xx[,1], f))
  wt = apply(xx, 2, function(z) wilcox.auc(z, f))
  rownames(wt) = rnames
  wt
}

wilcox.auc = function(x, f, alpha=0.05){
  w = rcorr.cens(x, f)
  auc = w['C Index']
  se = w['S.D.']/2 # Dxy to AUC transformation
  low = auc+qnorm(alpha)*se
  high = auc+qnorm(1-alpha)*se
  p.value = wilcox.test(x~f)$p.value
  ns = tapply(f,f,length)
  res=rbind(n1=ns[1], n2=ns[2], auc=auc, se=se, low=low, high=high, p.value=p.value)
  colnames(res) = ""
  t(res)
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


