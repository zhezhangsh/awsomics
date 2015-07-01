geex.plot.pca<-function(cll, pc.x, pc.y, clr, ds, gp, rid) {
  # cll         A data collection returned by geex.load.collection
  # pc.x, pc.y  The PCs to show
  # clr         The color choice
  # ds          Data set long name as selected by pca.dataset
  # gp          Selected groups to be included
  # rid         Row ID of samples to highlight
  
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg); 
  
  if (identical(NA, cll)) plotEmpty('No selected data set') else {
    ds.id<-as.vector(cll$mapping$longname2id[ds]);
    e<-readRDS(paste(cll$path, '/gex_', ds.id, '.rds', sep=''))[[1]][[1]]; 
    smp<-cll$metadata$Sample;
    smp<-smp[smp$Dataset==ds.id, , drop=FALSE];
    grp<-cll$metadata$Group[as.vector(smp$Group), , drop=FALSE];
    grp.nm<-paste(as.vector(smp$Group), as.vector(grp$Name), sep=': ');
    e<-e[, rownames(smp), drop=FALSE];
    
    col0<-GetColors(length(unique(smp$Group)), clr);
    names(col0)<-unique(grp.nm);
    col<-col0[grp.nm];

    e<-e[, grp.nm %in% gp, drop=FALSE];
    col<-col[grp.nm %in% gp];
    grp.nm<-grp.nm[grp.nm %in% gp];
    
    if (ncol(e) < 2) plotEmpty('Not enough samples to run PCA') else {
      cex<-min(4, 60/ncol(e));
      pr<-prcomp(t(e));
      
      pc<-as.numeric(sub('PC', '', c(pc.x, pc.y), ignore.case=TRUE));
      
      PlotPCA(pr, grp.nm, col=col, cex=cex, legend=TRUE, highlight=rid, dimensions=pc, new.window=FALSE);
    }
  }
}