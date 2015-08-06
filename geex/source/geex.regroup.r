geex.regroup<-function(cll, cnm.ds, cnm.grp) {

  metadata<-cll$metadata;
  ds<-metadata$Dataset;
  grp<-metadata$Group;
  smp<-metadata$Sample;
  
  cnm.ds<-cnm.ds[cnm.ds %in% colnames(smp)];
  cnm.grp<-cnm.grp[cnm.grp %in% colnames(smp)];
  
  if (length(cnm.ds)==0 | length(cnm.grp)==0) {
    cll$extra$regrouped<-FALSE;
    cll;
  }  else {
    ds0<-GroupSamples(smp, cnm.ds);
    grp0<-lapply(ds0, function(d) GroupSamples(d, cnm.grp));
    
    # Update data set info
    if (!identical(tolower(cnm.ds), 'dataset')) {
      ds.nm<-names(ds0);
      ds.id<-sub('^1', 'D', 10000+1:length(ds.nm));
      names(ds.id)<-ds.nm;
      n.smp<-sapply(ds0, nrow)[ds.nm];
      n.grp<-sapply(grp0, length)[ds.nm];
      ds<-data.frame(row.names=ds.id, stringsAsFactors=FALSE, Name=ds.nm, Num_Group=n.grp, Num_Sample=n.smp);
      
      for (i in 1:length(ds.nm)) smp[rownames(ds0[[i]]), 'Dataset']<-ds.id[ds.nm[i]];
    }
    
    # Update group metadata
    ds.nm<-rep(names(grp0), sapply(grp0, length));
    ds.id<-rownames(ds);
    names(ds.id)<-ds$Name;
    ds.id<-ds.id[ds.nm];
    grp.nm<-unlist(lapply(grp0, names), use.names=FALSE);
    grp.id<-sub('^1', 'G', 10000+1:length(grp.nm));
    names(grp.id)<-grp.nm;
    n.smp<-unlist(lapply(grp0, function(g) sapply(g, nrow)), use.names=FALSE);
    grp<-data.frame(row.names=grp.id, stringsAsFactors=FALSE, Name=grp.nm, Dataset=ds.id, Dataset_Name=ds.nm, Num_Sample=n.smp);
    
    grp2smp<-unlist(grp0, recursive=FALSE);
    smp.id<-unlist(lapply(grp2smp, rownames), use.names=FALSE);
    smp[smp.id, 'Group']<-rep(grp.id, sapply(grp2smp, nrow));
    
    cll$metadata$Dataset<-ds;
    cll$metadata$Group<-grp;
    cll$metadata$Sample<-smp;
    
    cll$browse_table$"Data set"<-data.frame(ID=rownames(ds), ds, stringsAsFactors=FALSE);
    cll$browse_table$"Group"<-data.frame(ID=rownames(grp), grp, stringsAsFactors=FALSE);
    cll$browse_table$"Sample"<-data.frame(ID=rownames(smp), smp, stringsAsFactors=FALSE);
    
    cll$metadata_by_id<-NA;
    
    # update ID-name mapping
    id2nm<-do.call('c', lapply(cll$metadata, function(meta) meta[, 'Name']));
    names(id2nm)<-unlist(lapply(cll$metadata, rownames));
    nm2id<-names(id2nm);
    cll$mapping[1:6]<-list(
      id2name=id2nm,
      name2id=setNames(names(id2nm), as.vector(id2nm)),
      id2longname=setNames(paste(names(id2nm), id2nm, sep=': '), names(id2nm)),
      longname2id=setNames(names(id2nm), paste(names(id2nm), id2nm, sep=': ')),
      name2longname=setNames(paste(names(id2nm), id2nm, sep=': '), as.vector(id2nm)),
      longname2name=setNames(as.vector(id2nm), paste(names(id2nm), id2nm, sep=': '))
    );
    
    gex<-cll$gex_combined;
    gex<-lapply(gex, function(g) g[, grep('^S', colnames(g)), drop=FALSE]);
    gex<-lapply(gex, function(g) {
      ms<-sapply(rownames(grp), function(id) rowMeans(g[, rownames(smp)[smp$Group==id], drop=FALSE], na.rm=TRUE));
      cbind(ms, g);
    })
    gex<-lapply(gex, function(g) {
      ms<-sapply(rownames(ds), function(id) rowMeans(g[, rownames(smp)[smp$Dataset==id], drop=FALSE], na.rm=TRUE));
      cbind(ms, g);
    })
    cll$gex_combined<-gex;
    
    cll$extra$longname$dataset<-paste(rownames(cll$metadata$Dataset), cll$metadata$Dataset$Name, sep=': ');    
    cll$extra$regrouped<-TRUE;
    
    cll;
  }
}

geex.regroup.options<-function(smp) {
  cnm<-colnames(smp)[-(1:2)];
  cnm<-cnm[tolower(cnm) != 'url'];
  if (length(cnm) == 0) NA else {
    lvl<-lapply(cnm, function(c) {
      v<-as.vector(smp[, c]);
      v<-v[!is.na(v) & v!=''];
      sort(unique(v));
    });
    l<-sapply(lvl, function(l) if (length(l)<=1) l else paste(l[c(1, length(l))], collapse='; '));
    data.frame(Feature=cnm, Num_Value=sapply(lvl, length), Value_Example=l, stringsAsFactors=FALSE);
  }
}

geex.regroup.outputs<-function(smp, cnm.ds, cnm.grp) {
  cnm.ds<-cnm.ds[cnm.ds %in% colnames(smp)];
  cnm.grp<-cnm.grp[cnm.grp %in% colnames(smp)];
  
  if (length(cnm.ds) == 0) {
    list(dataset=geex.empty.matrix("No feature selected"), 
         group=geex.empty.matrix("No data set defined"));
  } else {
    ds<-GroupSamples(smp, cnm.ds);
    out<-list(dataset=data.frame(Temp_ID=sub('^1', 'TD', 10000+1:length(ds)), 
                                 Name=names(ds), Num_Sample=sapply(ds, nrow)));
    if (length(cnm.grp)==0) out$group<-geex.empty.matrix("No feature selected") else {
      grp<-lapply(ds, function(ds) GroupSamples(ds, cnm.grp));
      n<-unlist(lapply(grp, function(g) sapply(g, nrow)), use.names=FALSE);
      ds.nm<-rep(names(grp), sapply(grp, length));
      grp.nm<-unlist(lapply(grp, names), use.names=FALSE);
      out$group<-data.frame(Temp_ID=sub('^1', 'TG', 10000+1:length(grp.nm)), 
                            Name=grp.nm, Dataset=ds.nm, Num_Sample=n);
    }
    out;  
  }
}