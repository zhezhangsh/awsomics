geex.summarize.gene<-function(cll, gn) { 
  
  gid<-CleanHtmlTags(gn);

  ds<-cll$metadata$Dataset;
  d<-cll$gex_combined$logged[gid, rownames(ds)];
  ds.id<-names(d)[!is.na(d)];
  
  out<-data.frame(ID=ds.id, stringsAsFactors=FALSE);
  out$Dataset_Name<-ds[ds.id, 'Name'];
  
  d.logged<-lapply(ds.id, function(id) {
    smp.id<-rownames(cll$metadata$Sample)[cll$metadata$Sample$Dataset==id];
    d<-cll$gex_combined$logged[gid, smp.id];
    d[!is.na(d)];
  });
  
  out$N_Sample<-sapply(d.logged, length);
  
  grp<-sapply(d.logged, function(d) {
    grp<-as.vector(cll$metadata$Sample[names(d), 'Group']);
    n<-length(unique(grp));
    if (n<2) p<-1 else p<-summary(aov(d~as.factor(grp)))[[1]][1, 5];
    if (is.null(p)) p<-1;
    c(n, p);
  });
  out$N_Group<-round(grp[1,]);
  
  out$Mean<-round(sapply(d.logged, mean), 4);
  out$Median<-round(sapply(d.logged, median), 4);
  out$Min<-round(sapply(d.logged, min), 4);
  out$Max<-round(sapply(d.logged, max), 4);
  out$Variance<-round(sapply(d.logged, var), 4);
  out$Skewness<-round(sapply(d.logged, e1071::skewness), 4);
  
  d.pct<-lapply(ds.id, function(id) {
    smp.id<-rownames(cll$metadata$Sample)[cll$metadata$Sample$Dataset==id];
    as.vector(cll$gex_combined$percentile[gid, smp.id]);
  });
  out$Percentile_Mean<-round(sapply(d.pct, mean), 4);
  out$Percentile_Median<-round(sapply(d.pct, median), 4);
  out$Percentile_Min<-round(sapply(d.pct, min), 4);
  out$Percentile_Max<-round(sapply(d.pct, max), 4);
  
  out$pANOVA<-round(grp[2, ], 6);  
  
  out;
}