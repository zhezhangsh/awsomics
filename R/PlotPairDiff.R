## Plots to show pairwise comparison

PlotPairDiffTypes<-function() c("Scatter Plot", "MA Plot", "Box Plot", "Bar Plot", "Density Plot")

PlotPairDiff<-function(d, type, color=GetColorTypes()[1], subset=list(), highlight=c()) {
  type=tolower(type)[1];
  tp<-tolower(PlotPairDiffTypes());
#   subset<-subset[subset %in% rownames(d)];
#   highlight<-highlight[highlight %in% rownames(d)];
  
  plotted<-list();
  if (type == tp[1]) plotted<-PlotPairDiffScatter(d, subset, highlight, color) else 
    if (type == tp[2]) plotted<-PlotPairDiffMa(d, subset, highlight, color)      else 
      if (type == tp[3]) plotted<-PlotPairDiffBox(d, subset, highlight, color)     else 
        if (type == tp[4]) plotted<-PlotPairDiffBar(d, subset, highlight, color)    else 
          if (type == tp[5]) plotted<-PlotPairDiffDens(d, subset, highlight, color)    else 
            plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Unknown plot type:', type)); 
  
  out<-list(plot.type=type);
  append(out, plotted);
}

#############################################################################################
# Scatter plot
PlotPairDiffScatter<-function(d, subset=list(), highlight=c(), color=GetColorTypes()[1]) { 
  par(mar=c(5,5,2,2));
  
  lims<-c(mn<-min(d, na.rm=TRUE), mx<-max(d, na.rm=TRUE));
  cex<-max(min(4, 4/log10(nrow(d))), 0.25);
  corr<-round(cor(d[,1], d[, 2], use='pair'), 4);
  main<-paste('Corr = ', corr, '; N = ', nrow(d), sep='');
  plot(d[, 1], d[, 2], pch=19, cex=cex, cex.main=1.5, cex.lab=1.5, col='#88888888', xlim=lims, ylim=lims, main=main, xlab=colnames(d)[1], ylab=colnames(d)[2]);
  abline(0, 1, lty=2, lwd=2, col='#6666FF88');
  
  ###############################################################################################################################
  highlight<-highlight[highlight %in% rownames(d)];
  subset<-lapply(subset, function(s) s[s %in% rownames(d)]);  
  col<-GetColors(length(highlight)+length(subset), color[1]);
  names(col)<-c(highlight, names(subset)); 
  if (length(highlight) > 0) points(d[highlight, 1], d[highlight, 2], cex=2*cex, pch=13, lwd=2, col=col[highlight]);
  if (length(subset) > 0) for (i in 1:length(subset)) points(d[subset[[i]], 1], d[subset[[i]], 2], cex=1.25*cex, pch=8, col=col[length(highlight)+i]);
                                                           
  list(corr=cor, highlight.color=col);                                 
}

PlotPairDiffMa<-function(d, subset=list(), highlight=c(), color=GetColorTypes()[1]) {

  corr<-round(cor(d[,1], d[, 2], use='pair'), 4);
  main<-paste('Corr = ', corr, '; N = ', nrow(d), sep='');
  
  dff<-d[,2]-d[,1];
  d[,1]<-d[,1]/2+d[,2]/2;
  d[,2]<-dff;
  
  par(mar=c(15,5,2,2));
  mx<-max(abs(dff), na.rm=TRUE);
  lims<-c(-1*mx, mx);
  cex<-max(min(4, 4/log10(nrow(d))), 0.25);
  xlab<-'Mean average (A)';
  ylab<-paste(colnames(d)[2], ' vs. ', colnames(d)[1], ' (M)', sep='');
  
  plot(d[, 1], d[, 2], pch=19, cex=cex, cex.main=1.5, cex.lab=1.5, col='#88888888', ylim=lims, main=main, xlab=xlab, ylab=ylab);
  abline(h=0, lty=1, lwd=1, col='black');
  lines(stats::lowess(d[,1], d[,2]), lwd=1.5, lty=2, col='#6666FF88');
  
  ###############################################################################################################################
  highlight<-highlight[highlight %in% rownames(d)];
  subset<-lapply(subset, function(s) s[s %in% rownames(d)]);  
  col<-GetColors(length(highlight)+length(subset), color[1]);
  names(col)<-c(highlight, names(subset)); 
  if (length(highlight) > 0) points(d[highlight, 1], d[highlight, 2], cex=2*cex, pch=13, lwd=2, col=col[highlight]);
  if (length(subset) > 0) for (i in 1:length(subset)) points(d[subset[[i]], 1], d[subset[[i]], 2], cex=1.25*cex, pch=8, col=col[length(highlight)+i]);
  
  list(corr=cor, highlight.color=col);  
}

PlotPairDiffBox<-function(d, subset=list(), highlight=c(), color=GetColorTypes()[1]) {

  dff<-d[,2]-d[,1];
  names(dff)<-rownames(d);
  
  col<-GetColors(length(highlight)+length(subset), color[1]);
  names(col)<-c(highlight, names(subset)); 
  
  if (length(subset) > 0) {
    subset<-lapply(subset, function(s) s[s %in% names(dff)]);
    subset<-subset[sapply(subset, length)>0];
  }
  
  if (length(subset) > 0) {
    c<-col[names(subset)];
    s<-lapply(subset, function(s) dff[s]);
    dff<-dff[!(names(dff) %in% unlist(subset, use.names=FALSE))];
  } else {
    c<-c();
    s<-list();
  }
  
  if (length(dff) > 0) {
    c<-c('All others'='#A9A9A9', c);
    s<-append(list('All others'=dff), s);
  }

  names(s)<-names(c)<-sub('[:;] ', '\n', names(s));
  
  ################################################################################
  box<-PlotBoxFromList(s, ylab=paste(colnames(d)[2], 'vs.', colnames(d)[1]) , c);#
  ################################################################################
  
  highlight<-highlight[highlight %in% names(dff)];
  if (length(highlight) > 0) points(rep(1, length(highlight)), dff[highlight], cex=2, pch=13, lwd=2, col=col[highlight]);
  
  list(boxplot=box, highlight.color=col);  
}

PlotPairDiffBar<-function(d, subset=list(), highlight=c(), color=GetColorTypes()[1]) {
  dff<-d[,2]-d[,1];
  names(dff)<-rownames(d);
  
  col<-GetColors(length(highlight)+length(subset), color[1]);
  names(col)<-c(highlight, names(subset)); 
  
  if (length(subset) > 0) {
    subset<-lapply(subset, function(s) s[s %in% names(dff)]);
    subset<-subset[sapply(subset, length)>0];
  }
  
  if (length(subset) > 0) {
    c<-col[names(subset)];
    s<-lapply(subset, function(s) dff[s]);
    dff<-dff[!(names(dff) %in% unlist(subset, use.names=FALSE))];
  } else {
    c<-c();
    s<-list();
  }
  
  if (length(dff) > 0) {
    c<-c('All others'='#A9A9A9', c);
    s<-append(list('All others'=dff), s);
  }
  
  names(s)<-names(c)<-sub('[:;] ', '\n', names(s));

  ################################################################################
  bar<-PlotBarFromList(s, ylab=paste(colnames(d)[2], 'vs.', colnames(d)[1]) , c);#
  ################################################################################
  
  highlight<-highlight[highlight %in% names(dff)];
  if (length(highlight) > 0) points(rep(bar$xaxis.pos[1], length(highlight)), dff[highlight], cex=2, pch=13, lwd=2, col=col[highlight]);
  
  list(boxplot=bar, highlight.color=col);  
  
}

PlotPairDiffDens<-function(d, subset=list(), highlight=c(), color=GetColorTypes()[1]) {
  dff<-d[,2]-d[,1];
  names(dff)<-rownames(d);
  
  col<-GetColors(length(highlight)+length(subset), color[1]);
  names(col)<-c(highlight, names(subset)); 
  
  if (length(subset) > 0) {
    subset<-lapply(subset, function(s) s[s %in% names(dff)]);
    subset<-subset[sapply(subset, length)>0];
  }
  
  if (length(subset) > 0) {
    c<-col[names(subset)];
    s<-lapply(subset, function(s) dff[s]);
    dff<-dff[!(names(dff) %in% unlist(subset, use.names=FALSE))];
  } else {
    c<-c();
    s<-list();
  }
  
  if (length(dff) > 0) {
    c<-c('All others'='#A9A9A9', c);
    s<-append(list('All others'=dff), s);
  }
  
  c0<-paste(substr(c, 1, 7), '88', sep='');
  
  names(s)<-names(c)<-sub('[:;] ', '\n', names(s));  
  #################################################################################################
  dens<-PlotDensityFromList(s, xlab=paste(colnames(d)[2], 'vs.', colnames(d)[1]), c0, fill=TRUE); #
  #################################################################################################
  
  highlight<-highlight[highlight %in% names(dff)];
  if (length(highlight) > 0) points(dff[highlight], rep(0, length(highlight)), cex=2, pch=13, lwd=2, col=col[highlight]);
  
  list(density.plot=dens, highlight.color=col);  
  
}