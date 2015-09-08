geex.filter.table<-function(cll, ds.long, grp.long, smp.long) {
  out<-list();
  out$is.empty<-FALSE;
  out$message<-'';
  
  if (identical(NA, cll)) {
    out$message<-geex.html.msg(msg.nocollection);
    out$is.empty<-TRUE; 
  } else {
    gex<-cll$gex_combined$percentile;
    ds.id<-cll$mapping$longname2id[ds.long];
    sp<-tolower(cll$metadata$Dataset[ds.id, 'Species']);
    if (grp.long != '') grp.id<-cll$mapping$longname2id[grp.long] else grp.id<-'';
    if (smp.long != '') smp.id<-cll$mapping$longname2id[smp.long] else smp.id<-'';
    
    if (smp.id %in% colnames(gex)) {
      id<-smp.id;
      smp<-cll$metadata$Sample;
      e<-gex[, rownames(smp)[smp$Dataset==ds.id], drop=FALSE];
      out$message<-paste(geex.html.msg(id), 'vs.', ncol(e)-1, 'other sample');
    } else if (grp.id %in% colnames(gex)) {
      id<-grp.id;
      grp<-cll$metadata$Group;
      e<-gex[, rownames(grp)[grp$Dataset==ds.id], drop=FALSE];
      out$message<-paste(geex.html.msg(id), 'vs.', ncol(e)-1, 'other group');
    } else {
      id<-ds.id;
      e<-gex[, rownames(cll$metadata$Dataset), drop=FALSE];
      sp<-tolower(unique(cll$metadata$Dataset$Species));
      if ('human' %in% sp) sp<-'human'
      out$message<-paste(geex.html.msg(id), 'vs.', ncol(e)-1, 'other data set');
    }
    if (ncol(e)>2) out$message<-paste(out$message, 's', sep='');
    
    anno<-cll$gene[, c('Symbol', 'Species', 'type_of_gene', 'description'), drop=FALSE];
    colnames(anno)<-c('Name', 'Species', 'Gene_Type', 'Description');
    anno<-anno[anno$Species==sp, , drop=FALSE];
    anno<-anno[rownames(anno) %in% rownames(e), , drop=FALSE];
    e<-e[rownames(anno), , drop=FALSE];
    
    e0<-e[, id];
    e1<-e[, colnames(e)!=id, drop=FALSE];
    mn<-apply(e1, 1, function(e) sort(e)[1]);
    mx<-apply(e1, 1, function(e) sort(e, decreasing=TRUE)[1]);
    g<-data.frame(e0, Min_Others=mn, Max_Others=mx, e0-mn, e0-mx, stringsAsFactors=FALSE);
    colnames(g)[1]<-id;
    colnames(g)[4]<-paste(id, 'Min', sep='_vs_');
    colnames(g)[5]<-paste(id, 'Max', sep='_vs_');
      
    d<-data.frame(ID=AddHref(rownames(anno), UrlEntrezGene(rownames(anno))), anno[, -ncol(anno)], g, stringsAsFactors = FALSE);
    d$Description<-anno[, ncol(anno)];

    out$id<-id;
    out$name<-cll$mapping$id2longname[id];
    out$gex<-d;
  }
  out;
}