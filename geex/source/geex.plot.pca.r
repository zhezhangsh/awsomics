geex.plot.pca<-function(cll, nm, rid) {
  # cll       A data collection returned by geex.load.collection
  # nm        Data set long name as selected by pca.dataset
  # rid       Row ID of samples to highlight
  
  # make an empty plot
  plotEmpty<-function() plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main='No selected data set.'); 

  out<-list();
  out$msg<-'';
  
  if (identical(NA, cll)) plotEmpty() else {
    ds.id<-as.vector(cll$mapping$longname2id[nm]);
    e<-readRDS(paste(cll$path, '/gex_', ds.id, '.rds', sep=''))[[1]][[1]]; 
    smp<-cll$metadata$Sample;
    smp<-smp[smp$Dataset==ds.id, , drop=FALSE];
    grp<-cll$metadata$Group[as.vector(smp$Group), , drop=FALSE];
    grp.nm<-paste(as.vector(smp$Group), as.vector(grp$Name), sep=': ');
    e<-e[, rownames(smp), drop=FALSE];
    cex<-min(4, 48/nrow(smp));
    pr<-prcomp(t(e));
    
    PlotPCA(pr, grp.nm, cex=cex, legend=TRUE, highlight=rid, new.window=FALSE);
  }
  
  out;
}