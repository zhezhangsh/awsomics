## Make plot using a data matrix, whose columns are split into sample groups to be compared.

PlotMatrixGroupTypes<-function() c('Heatmap', 'Bar Plot', 'Box Plot', 'Density Plot', 'Cumulative Plot');

PlotMatrixGroup<-function(d, grp, type, normalize=TRUE, color=GetColorTypes()[1]) {
  # d           A data matrix
  # grp         Named list corresponding to sample groups and column names in <d>
  # normalize   Whether to normalize data across samples
  # color       Color code
  
  type=tolower(type)[1];
  tp<-tolower(PlotMatrixGroupTypes());
  
  grp<-lapply(grp, function(g) g[g %in% colnames(d)]);
  grp<-grp[sapply(grp, length)>0];
  d<-d[, unlist(grp, use.names=FALSE), drop=FALSE];
  d<-d[!is.na(rowMeans(d)), , drop=FALSE];
  if (normalize) d<-t(scale(t(d)));
  
  out<-list();
  
  if (ncol(d)==0 | nrow(d)==0) {
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Not enough data to plot'));
    out$message<-'Not enough data to plot';
  } else {
    plotted<-list();
    if (type == tp[1]) plotted<-PlotMatrixGroupHeatmap(d, grp, color, normalize) else 
      if (type == tp[2]) plotted<-PlotMatrixGroupBar(d, grp, color, normalize) else 
        if (type == tp[3]) plotted<-PlotMatrixGroupBox(d, grp, color, normalize) else 
          if (type == tp[4]) plotted<-PlotMatrixGroupDensity(d, grp, color, normalize) else 
            if (type == tp[5]) plotted<-PlotMatrixGroupCumulative(d, grp, color, normalize) else 
              #if (type == tp[6]) plotted<-PlotMatrixGroupLine(d, grp, color, normalize) else 
                plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Unknown plot type:', type)); 
    
    out<-list(plot.type=type);
    out<-append(out, plotted);  
  };

  out;
}

#############################################################################################
# Heatmap
PlotMatrixGroupHeatmap<-function(d, grp, color, normalize) {
  if (nrow(d) < 2) {
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste("Not enough genes to plot heatmap"));
    out<-"Not enought genes";
  } else {
    # block color
    c<-gplots::colorpanel(129, 'yellow', 'red');
    
    # set label size
    w0<-1.2/max(strwidth(colnames(d), unit='in'));
    cexCol<-min(1.5, w0);
    w<-1.2/max(strwidth(rownames(d), unit='in'));
    h<-(par()$fin[2]-2.5)/nrow(d)/max(strheight(rownames(d), unit='in'));
    cexRow<-min(1.5, min(w, h));

    col<-GetColors(length(grp), color);
    col<-rep(col, sapply(grp, length));
    
    mar.sz<-max(0, nrow(d)/-8+1.25);
    par(omi=c(mar.sz, 0, mar.sz, 0));
    
    if (normalize) xlab<-'Normalized data' else xlab<-'Un-normalized data';
    
    out<-gplots::heatmap.2(d, scale='none', cexCol=cexCol, cexRow=cexRow, col=c, ColSideColors=col, trace='none', srtCol=30, srtRow=30,
                           key=TRUE, keysize=1, key.title='', key.xlab=xlab, key.ylab='', density.info='none', margins=c(6, 10));
  }
  list(heatmap=out, col=col);
}

#############################################################################################
# Barplot
PlotMatrixGroupBar<-function(d, grp, color, normalize) {
  if (normalize) ylab<-'Normalized average expression level' else ylab<-'Average expression level';
  
  c<-GetColors(length(grp), color);
  
  m<-colMeans(d);
  s<-lapply(grp, function(g) m[g]);

  ########################################
  bar<-PlotBarFromList(s, ylab=ylab , c);#
  ########################################
  
  list(barplot=bar, color=c);  
}

#############################################################################################
# Boxplot
PlotMatrixGroupBox<-function(d, grp, color, normalize) {
  if (normalize) ylab<-'Normalized expression level' else ylab<-'Expression level';
  
  c<-GetColors(length(grp), color);
  
  m<-colMeans(d);
  s<-lapply(grp, function(g) m[g]);
  
  ########################################
  bar<-PlotBoxFromList(s, ylab=ylab , c);#
  ########################################
  
  list(boxplot=bar, color=c);  
}

#############################################################################################
# Density plot
PlotMatrixGroupDensity<-function(d, grp, color, normalize) {
  if (nrow(d) < 2) {
    dens<-"Not enough data points to make density plot";
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Not enough data to make density plot'));
  } else {
    s<-lapply(grp, function(g) rowMeans(d[, g, drop=FALSE]));
    if (normalize) xlab<-'Normalized expression level' else xlab<-'Expression level';
    c<-GetColors(length(grp), color);
    c0<-paste(substr(c, 1, 7), '88', sep='');
    ######################################################################
    dens<-PlotDensityFromList(s, xlab=xlab, c0, fill=TRUE, legend=TRUE); #
    ######################################################################
  }

  list(densityplot=dens, color=c);  
}


#############################################################################################
# Density plot
PlotMatrixGroupCumulative<-function(d, grp, color, normalize) {
  if (nrow(d) < 2) {
    dens<-"Not enough data points to make cumulative plot";
    plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=paste('Not enough data to make density plot'));
  } else {
    s<-lapply(grp, function(g) rowMeans(d[, g, drop=FALSE]));
    if (normalize) xlab<-'Normalized expression level' else xlab<-'Expression level';
    c<-GetColors(length(grp), color);
    #########################################################
    cm<-PlotCumulativeFromList(s, xlab=xlab, c, legend=TRUE); #
    #########################################################
  }
  
  list(cumulative=cm, color=c);  
}
