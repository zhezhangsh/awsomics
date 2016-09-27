PlotPValue <- function(pv, col='#6666FFDD', interval=0.01, xlab='', ylab='', title='') {
  
  pv <- pv[!is.na(pv)];
  pv <- pv[pv>=0 & pv <=1];
  
  title <- title[1];
  xlab  <- xlab[1];
  ylab  <- ylab[1];
  
  if (is.na(title) | title=='') par(mar=c(5,5,2,2)) else par(mar(c(5,5,3,2)));
  if (is.na(xlab) | xlab=='') xlab <- 'P value';
  if (is.na(ylab) | ylab=='') ylab <- 'Number of genes';
  
  if (interval <= 0 | interval > 1) interval <- 0.01;
  
  hist(pv, br=1/interval, border=NA, col=col, main=title, xlab=xlab, ylab=ylab, cex.lab=2);
  abline(h=interval*length(pv), lty=2, col='#333333');
  box();
}