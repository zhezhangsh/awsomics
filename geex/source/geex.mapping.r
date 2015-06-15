# mapping functions

# a dataset longname to grou longname of the data set
geex.longname.set2groups<-function(cll, longname.dataset) {
  
  longname.dataset<-longname.dataset[1];
  if (identical(NA, longname.dataset) | longname.dataset=='') c() else {
    grp<-cll$metadata$Group;
    name.dataset<-cll$mapping$longname2id[longname.dataset];
    grp<-grp[grp$Dataset == as.vector(name.dataset), , drop=FALSE];
    
    as.vector(cll$mapping$id2longname[rownames(grp)]);
  }
}