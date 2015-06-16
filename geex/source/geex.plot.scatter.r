# make scatterplot of group means

geex.plot.scatter<-function(cll, xgrp, ygrp, highlight=NA) {
  # make an empty plot
  plotEmpty<-function() plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main='No selected data set.'); 
  
  if (identical(NA, cll) | xgrp=='' | ygrp=='') plotEmpty() else {
    d<-geex.get2group(cll, xgrp, ygrp);
    
    main<-paste('Corr = ', round(cor(d[,2], d[,3]), 4), ', N = ', nrow(d), sep='');
    par(mar=c(5,5,2,2));
    plot(d[, 2], d[, 3], pch=19, cex=1, main=main, cex.main=1.5, col='#88888888', xlab=xgrp, ylab=ygrp, cex.lab=1.5);
    
    # Plot highlight genes
    highlight<-highlight[highlight %in% rownames(d)];
    if (length(highlight) > 0) {
      points(d[highlight, 2], d[highlight, 3], pch=19, cex=1.25, col='#0000FF');
      points(d[highlight, 2], d[highlight, 3], cex=2, col='black');      
    }
  }
}

# Get 2 groups of data for scatter plot
geex.get2group<-function(cll, xgrp, ygrp) {
  if (identical(NA, cll)  | xgrp=='' | ygrp=='') {
    matrix('', nr=1, nc=01, dimnames=list('', 'Empty table'));
  } else {   
    meta<-cll$metadata;
    mp<-cll$mapping;
    path<-cll$path;
    
    xid<-as.vector(mp$longname2id[xgrp]);
    yid<-as.vector(mp$longname2id[ygrp]);
    
    xds<-as.vector(meta$Group[xid, 'Dataset']);
    yds<-as.vector(meta$Group[yid, 'Dataset']);
    
    dx<-readRDS(paste(path, '/gex_', xds, '.rds', sep=''))$human[[1]];
    if (yds==xds) dy<-dx else dy<-readRDS(paste(path, '/gex_', yds, '.rds', sep=''))$human[[1]];
    
    gn.id<-intersect(rownames(dx), rownames(dy)); # common gene IDs
    gn.nm<-cll$gene[gn.id, 'Symbol'];
    
    d<-data.frame(row.names=gn.id, Name=gn.nm, round(dx[gn.id, xid], 4), round(dy[gn.id, yid], 4));
    names(d)[-1]<-c(xid, yid);
    
    d;
  }
}