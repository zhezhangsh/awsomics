geex.plot.two<-function(cll, xgrp, ygrp, type, scale, species, gn, gs) {
  cat('\n', xgrp, ygrp, scale, type, species, gn, names(gs), '\n');
  
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg);   
  
  gn<-CleanHtmlTags(gn);
  names(gs)<-CleanHtmlTags(gs);
  
  if (identical(NA, cll)) {
    plotEmpty("No loaded data collection");
  } else if (identical(NA, xgrp[1]) | xgrp[1]=='') {
    plotEmpty("Group 1 not selected");
  } else if (identical(NA, ygrp[1]) | ygrp[1]=='') {
    plotEmpty("Group 2 not selected");
  } else {
    d<-geex.get2group(cll, xgrp, ygrp, species[1])[, 3:4, drop=FALSE];
    PlotPairDiff(d, type, gs, gn);
  }
   
#     dff<-d[,3]-d[,2];
#     corr<-round(cor(d[,2], d[,3]), 4);
#     
#     main<-paste('Corr = ', corr, ', N = ', nrow(d), sep='');
#     par(mar=c(5,5,2,2));
#     plot(d[, 2], d[, 3], pch=19, cex=1, main=main, cex.main=1.5, col='#88888888', xlab=xgrp, ylab=ygrp, cex.lab=1.5);
}

# make scatterplot of group means
geex.plot.scatter<-function(cll, xgrp, ygrp, highlight=NA) {
 
  if (identical(NA, cll) | xgrp=='' | ygrp=='') plotEmpty() else {
    d<-geex.get2group(cll, xgrp, ygrp);
    corr<-round(cor(d[,2], d[,3]), 4);    
    main<-paste('Corr = ', corr, ', N = ', nrow(d), sep='');
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

geex.get2group.species<-function(cll, xgrp, ygrp) { 

  if (identical(NA, cll) | xgrp=='' | ygrp=='') c() else {
    mp<-cll$mapping;
    meta<-cll$metadata;

    xid<-as.vector(mp$longname2id[xgrp]);
    yid<-as.vector(mp$longname2id[ygrp]);
    
    xds<-as.vector(meta$Group[xid, 'Dataset']);
    yds<-as.vector(meta$Group[yid, 'Dataset']);
    
    xsp<-tolower(as.vector(meta$Dataset[xds, 'Species']));
    ysp<-tolower(as.vector(meta$Dataset[yds, 'Species']));
    
    sp<-unique(c('human', intersect(xsp, ysp)));
    sp[length(sp):1];
  } 
}

# Get 2 groups of data for plot
geex.get2group<-function(cll, xgrp, ygrp, sp) { 
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

    gex<-cll$gex_combined$logged[, c(xid, yid), drop=FALSE];
    gex<-gex[!is.na(gex[,1]) & !is.na(gex[,2]), , drop=FALSE];
        
    gn<-cll$gene[, c('Species', 'Symbol'), drop=FALSE]; 
    gn<-gn[rownames(gex), , drop=FALSE];
        
    if (sp[1] %in% as.vector(gn[[1]])) gn<-gn[gn[[1]]==sp, , drop=FALSE] else gn<-gn[gn[[1]]=='human', , drop=FALSE]
    gn.id<-rownames(gn);

    gex<-round(gex[rownames(gn), , drop=FALSE], 4);

    #dx<-readRDS(paste(path, '/gex_', xds, '.rds', sep=''))$human[[1]];
    #if (yds==xds) dy<-dx else dy<-readRDS(paste(path, '/gex_', yds, '.rds', sep=''))$human[[1]];
    
    #gn.id<-rownames(dx)[rownames(dx) %in% rownames(dy)]; # common gene IDs
    #gn.nm<-cll$gene[gn.id, 'Symbol'];
    
    #m0<-round(dx[gn.id, xid], 4);
    #m1<-round(dy[gn.id, yid], 4);
    d<-data.frame(AddHref(gn.id, UrlEntrezGene(gn.id)), gn$Symbol, gex[, 1], gex[, 2], gex[, 2]-gex[, 1], stringsAsFactors = FALSE);
    names(d)<-c('ID', 'Name', xid, yid, "Diff");
    
    d;
  }
}