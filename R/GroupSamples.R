# Group samples by one or several of their characteristics 
GroupSamples<-function(smp, cnm, separator='_', replace.missing.with='NA', reorder=TRUE) {
  # smp     A data.frame or matrix of sample metadata, with columns as sample characteristics
  # cnm     One or more column names of <smp>
 
  cnm<-cnm[cnm %in% colnames(smp)];
  
  if (length(cnm)==0) NA else {
    sid<-rownames(smp);
    s<-sapply(cnm, function(cnm) {
      s<-as.vector(smp[, cnm]); 
      s[is.na(s)|s=='']<-replace.missing.with; 
      s;
    });
    if (length(sid)==1) s<-matrix(s, nc=length(s));
    nm<-apply(s, 1, function(s) paste(s, collapse=separator));
    mp<-split(sid, nm);
    mp<-mp[order(names(mp))];
    
    grp<-lapply(mp, function(id) smp[id, , drop=FALSE]);
    
    # Reorder groups by their smallest sample ID
    if (reorder) {
      smp.id<-lapply(grp, rownames);
      smp.id<-sapply(smp.id, function(id) sort(id)[1]);
      grp<-grp[order(smp.id)];
    }
    
    grp;
  }
}
