geex.plot.pca<-function(cll, pc.x, pc.y, clr, ds, gp, rid, gn.subset) {
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
    d<-geex.load.dataset(cll, ds);
    #e<-readRDS(paste(cll$path, '/gex_', ds.id, '.rds', sep=''))[[1]][[1]]; 
    e<-d$data$logged;
    # if (!setequal('human', tolower(unique(d$anno$Species))))  e<-e[rownames(d$anno)[d$anno$Species!='human'], , drop=FALSE];
    e<-e[!is.na(rowSums(e, na.rm=TRUE)), , drop=FALSE]; 
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
      gn<-gn.subset[gn.subset %in% rownames(e)]; 
      if (!identical(NA, gn.subset) & length(gn)<3) plotEmpty('No enough genes to run PCA') else {
        if (length(gn) > 2) e<-e[gn, ];
        a<-d$anno[rownames(e), , drop=FALSE];
        if (length(setdiff(tolower(a$Species), 'human'))>0)  e<-e[a$Species!='human', , drop=FALSE];
        cex<-max(2, min(4, 60/ncol(e)));
        pr<-prcomp(t(e));
        
        pc<-as.numeric(sub('PC', '', c(pc.x, pc.y), ignore.case=TRUE));
        
        PlotPCA(pr, grp.nm, col=col, cex=cex, legend=TRUE, highlight=rid, dimensions=pc, new.window=FALSE);
      }
    }
  }
}