geex.load.dataset<-function(cll, ds.longname) {
  if (identical(cll, NA)) NA else {
    smp<-cll$metadata$Sample;
    ds<-cll$metadata$Dataset;
    ds.id<-cll$mapping$longname2id[ds.longname];
    ds.id<-as.vector(ds.id[ds.id %in% rownames(ds)][1]);
    
    if (length(ds.id) == 0) NA else {
      s<-smp[smp$Dataset==ds.id, ];
      e<-lapply(cll$gex_combined, function(g) g[, rownames(s), drop=FALSE]);
      e<-lapply(e, function(e) e[!is.na(rowMeans(e)), , drop=FALSE]);
      anno<-cll$gene[rownames(e[[1]]), 'Symbol', drop=FALSE];
      
      group<-split(rownames(s), s$Group);
      names(group)<-cll$mapping$id2longname[names(group)];
      
      list(anno=anno, group=group, data=e);
    }
  } 
}

geex.load.collection<-function(coll.name, GEX_HOME) {
  
  # Files to be loaded
  fn.load<-c('metadata', 'metadata_by_id', 'gene', 'mapping', 'browse_table', 'gex_combined');
  
  coll.name<-coll.name[1];
  if (is.na(coll.name) | coll.name=='') {
    NA;
  } else {
    msg<-c(); # message to return
    
    id<-strsplit(coll.name, ': ')[[1]][1];
    nm<-strsplit(coll.name, ': ')[[1]][2];
    paths<-c(paste(GEX_HOME, c('public', 'private'), tolower(nm), 'r', sep='/'));
    path<-paths[file.exists(paths)];
    
    # Return data
    out<-list(
      selection=coll.name,
      id=id,
      name=nm,
      path=path
    );
    
    if (length(path) > 0) {
      path<-path[1];
      
      # Load in collection data and metadata
      loaded<-lapply(fn.load, function(fn) {
        fn<-paste(path, '/', fn, '.rds', sep='');
        if (!file.exists(fn)) NA else readRDS(fn);
      });
      names(loaded)<-fn.load;
      
      # If there are required files not exist
      fn.unload<-fn.load[sapply(loaded, function(d) identical(NA, d))];
      if (length(fn.unload) > 0) msg<-c(msg, paste("File", fn.unload, "does not exist in collection"));
      
      out<-append(out, loaded);
    } else {
      msg<-c(msg, paste("Data collection <", nm, "> does not exist in Rchive", sep=''));
    }
    
    if (length(msg) == 0) msg<-paste('Data collection "', coll.name, '" has been successfully loaded.', sep='');

    #####################################################################################
    # Extra slots to be appended for fast access
    out$extra<-list();
 
    out$message<-msg;
    out;
  }
}