## Plots to show pairwise comparison

PlotPairDiffType<-function() c("Scatter Plot", "MA Plot", "Box Plot", "Bar Plot", "Density")

PlotPairDiff<-function(d, type, subset=list(), highlight=c()) {
  type=tolower(type)[1];
  tp<-tolower(PlotPairDiffType());
  subset<-subset[subset %in% rownames(d)];
  highlight<-highlight[highlight %in% rownames(d)];
  
  plotted<-list();
  if (type == tp[1]) plotted<-PlotPairDiffScatter(d, subset, highlight) else 
    if (type == tp[2]) plotted<-PlotPairDiffMa(d, subset, highlight)      else 
      if (type == tp[3]) plotted<-PlotPairDiffBox(d, subset, highlight)     else 
        if (type == tp[4]) plotted<-PlotPairDiffBar(d, subset, highlight)    else 
          if (type == tp[5]) plotted<-PlotPairDiffDens(d, subset, highlight)    else 
          plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Unknown plot type:', type)); 
  
  plotted;
}

PlotPairDiffScatter<-function(d, subset=list(), highlight=c()) { print(dim(d));
  par(mar=c(5,5,2,2));
  
  lims<-c(mn<-min(d, na.rm=TRUE), mx<-max(d, na.rm=TRUE));
  cex<-max(min(4, 4/log10(nrow(d))), 0.25);
  corr<-round(cor(d[,1], d[, 2], use='pair'), 4);
  main<-paste('Corr = ', corr, '; N = ', nrow(d), sep='');
  plot(d[, 1], d[, 2], pch=19, cex=cex, cex.main=1.5, cex.lab=1.5, col='#88888888', xlim=lims, ylim=lims, main=main, xlab=colnames(d)[1], ylab=colnames(d)[2]);
  abline(0, 1, lty=2, lwd=1.5, col='#6666FF88');
  
  highlight<-highlight[highlight %in% rownames(d)];
  if (length(highlight) > 0) points(d[highlight, 1], d[highlight, 2], cex=1.5*cex, pch=13, lwd=2, col='black');
}

PlotPairDiffMa<-function(d, subset=list(), highlight=c()) {
  plot(0, pch=19, cex=4);
}

PlotPairDiffBox<-function(d, subset=list(), highlight=c()) {
  plot(1, pch=20, cex=4)
}

PlotPairDiffBar<-function(d, subset=list(), highlight=c()) {
  plot(1, pch=17, cex=4)
}

PlotPairDiffDens<-function(d, subset=list(), highlight=c()) {
  plot(1, pch=18, cex=4)
}