# mapping functions

# a dataset longname to group longname of the data set
geex.longname.set2groups<-function(cll, longname.dataset) {
  
  longname.dataset<-longname.dataset[1];
  if (identical(NA, longname.dataset) | longname.dataset=='') c() else {
    grp<-cll$metadata$Group;
    name.dataset<-cll$mapping$longname2id[longname.dataset];
    grp<-grp[grp$Dataset == as.vector(name.dataset), , drop=FALSE];
    
    as.vector(cll$mapping$id2longname[rownames(grp)]);
  }
}
# a group longname to sample longname 
geex.longname.group2samples<-function(cll, longname.group) {
  
  longname.group<-longname.group[1];
  if (identical(NA, longname.group) | longname.group=='') c() else {
    smp<-cll$metadata$Sample;
    name.group<-cll$mapping$longname2id[longname.group];
    smp<-smp[smp$Group == as.vector(name.group), , drop=FALSE];
    
    as.vector(cll$mapping$id2longname[rownames(smp)]);
  }
}