# Plot several data series as lines
PlotSeries<-function(d, se=NA, labs=c('', ''), title='', draw.legend=TRUE) {
  par(mar=c(min(12, max(nchar(colnames(d)))),5,2,2)); 
  col<-rainbow(nrow(d));
  if (identical(dim(d), dim(se))) {
    mn<-min(d-se, na.rm=TRUE);
    mx<-max(d+se, na.rm=TRUE);
  } else {
    mn<-min(d, na.rm=TRUE);
    mx<-max(d, na.rm=TRUE);
  }
  plot(0, type='n', xlim=c(1, ncol(d)), ylim=c(mn, mx), main=title, cex.lab=2, xlab=labs[1], ylab=labs[2], xaxt='n')
  abline(h=0);
  axis(1, at=1:ncol(d), colnames(d), las=3, cex.axis=min(2, 10/max(nchar(colnames(d))))); 
  if (draw.legend) legend(par()$usr[1], mx, lty=1, bty='n', col=col, text.col='darkgrey', legend=rownames(d));
 
  if (identical(dim(d), dim(se))) {
    hi<-as.vector(d)+se;
    lo<-as.vector(d)-se;
    xx<-rep(1:ncol(d), each=nrow(d));
    segments(xx, lo, xx, hi);
    segments(xx-0.05, lo, xx+0.05, lo);
    segments(xx-0.05, hi, xx+0.05, hi)
  }
  
  for (i in 1:nrow(d)) lines(d[i, ], col=col[i], lwd=2);
  for (i in 1:nrow(d)) points(d[i, ], col=col[i], pch=19);
}
