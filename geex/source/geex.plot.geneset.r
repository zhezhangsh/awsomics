geex.plot.geneset<-function(cll, grp, type, scale, color, normalize, gs) { 
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg);   
  
  out<-list();
  
  scale<-tolower(scale);
  type<-tolower(type);
  grp<-as.vector(cll$mapping$longname2id[grp]);
  smp<-cll$metadata$Sample;
  grp2smp<-split(rownames(smp), as.vector(smp$Group));
  grp<-grp[grp %in% rownames(cll$metadata$Group)];
  
  if (identical(NA, cll)) {
    plotEmpty("No loaded data collection");
  } else if (length(grp)==0) {
    plotEmpty("No selected sample group");
  } else {
    grp2smp<-grp2smp[grp];
    gex<-cll$gex_combined;
    if (scale=='unlogged') gex<-exp(gex$logged*log(2)) else if (scale=='percentile') gex<-gex$percentile else gex<-gex$logged;
    gex<-gex[, unlist(grp2smp, use.names=FALSE), drop=FALSE];
    gex<-gex[rownames(gex) %in% gs, , drop=FALSE]; 
    if (nrow(gex) == 0) plotEmpty("No gene of this gene set found in data set") else {
      colnames(gex)<-paste(as.vector(cll$metadata$Sample[colnames(gex), 'Group']), colnames(gex), sep=' : ');
      grp2smp<-lapply(grp2smp, function(g) paste(as.vector(cll$metadata$Sample[g, 'Group']), g, sep=' : '));
      gid<-rownames(gex);
      gnm<-as.vector(cll$gene[gid, 'Symbol']);
      rownames(gex)<-paste(gid, gnm, sep=' : ');
      PlotMatrixGroup(gex, grp2smp, type, normalize, color);
    }
  } 
}