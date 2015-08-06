geex.combined.table<-function(cll, species, scale) {
  if (scale=='unlogged') gex<-exp(cll$gex_combined$logged*log(2)) else 
    if (scale=='percentile') gex<-cll$gex_combined$percentile else 
      gex<-cll$gex_combined$logged;
  gex<-gex[, rownames(cll$metadata$Dataset), drop=FALSE];
  anno<-cll$gene;
  anno<-anno[anno$Species==species, , drop=FALSE];
  gex<-gex[rownames(anno), , drop=FALSE];
  
  n<-apply(gex, 2, function(g) length(g[!is.na(g)]));
  gex<-gex[, n>0, drop=FALSE];
  
  if (nrow(gex) == 0) geex.empty.matrix("No more genes after filtering") else {
    id<-rownames(gex);
    out<-data.frame(row.names=id, ID=AddHref(id, UrlEntrezGene(id)), stringsAsFactors=FALSE);
    
    out$Name<-as.vector(anno[id, 'Symbol']);
    out$Species<-as.vector(anno[id, 'Species']);
    out$Gene_Type<-as.vector(anno[id, 'type_of_gene']);
    out$Num_Dataset<-as.vector(anno[id, 'Num_Dataset']);
    out$Num_Sample<-as.vector(anno[id, 'Num_Sample']);
    out$Synonyms<-as.vector(anno[id, 'Synonyms']);
    out$Description<-as.vector(anno[id, 'description']);
    
    stat<-round(GetRowStat(gex), 2); 
    
    n<-apply(stat, 2, function(s) length(s[!is.na(s)]));
    stat<-stat[, n>0, drop=FALSE];
    
    out<-cbind(out, stat);
    out$'Mx-Mn'<-out[, 'Maximum']-out[, 'Minimum']; 
    cbind(out, round(gex, 4));
  }

}

geex.dataset.table<-function(cll, ds.longname, group, species, scale, extra, desc, ann, subset=c()) {
  
  ds.id<-as.vector(cll$mapping$longname2id[ds.longname]); 
  grp.id<-as.vector(cll$mapping$longname2id[group]); 
  smp<-cll$metadata$Sample;
  smp<-smp[smp$Dataset == ds.id, , drop=FALSE]; 
  smp<-smp[smp$Group %in% grp.id, , drop=FALSE];
  smp.id<-rownames(smp); 
  
  if (length(smp.id) == 0) geex.empty.matrix("No selected samples") else {
    if (scale=='unlogged') gex<-exp(cll$gex_combined$logged[, c(smp.id, grp.id), drop=FALSE]*log(2)) else 
      if (scale=='percentile') gex<-cll$gex_combined$percentile[, c(smp.id, grp.id), drop=FALSE] else 
        gex<-cll$gex_combined$logged[, c(smp.id, grp.id), drop=FALSE];
    gex<-gex[!is.na(rowMeans(gex, na.rm=TRUE)), , drop=FALSE];
    anno<-cll$gene[rownames(gex), , drop=FALSE];
    if (length(species) == 0) species<-unique(anno$Species);
    anno<-anno[anno$Species %in% species, , drop=FALSE];
    gex<-gex[rownames(anno), , drop=FALSE];
    
    if (length(subset) > 0) {
      gn<-unique(unlist(subset, use.names=FALSE));
      anno<-anno[rownames(anno) %in% gn, , drop=FALSE];
      gex<-gex[rownames(anno), , drop=FALSE];
    }
    
    if (nrow(gex) == 0) geex.empty.matrix("No more genes after filtering") else {
      id<-rownames(gex);
      out<-data.frame(row.names=id, ID=AddHref(id, UrlEntrezGene(id)), stringsAsFactors=FALSE);
      
      if ('name' %in% tolower(ann)) out$Name<-as.vector(anno[id, 'Symbol']);
      if ('species' %in% tolower(ann)) out$Species<-as.vector(anno[id, 'Species']);
      if ('gene_type' %in% tolower(ann)) out$Gene_Type<-as.vector(anno[id, 'type_of_gene']);
      if ('num_dataset' %in% tolower(ann)) out$Num_Dataset<-as.vector(anno[id, 'Num_Dataset']);
      if ('num_sample' %in% tolower(ann)) out$Num_Sample<-as.vector(anno[id, 'Num_Sample']);
      if ('synonyms' %in% tolower(ann)) out$Synonyms<-as.vector(anno[id, 'Synonyms']);
      
      stat<-GetRowStat(gex[, smp.id, drop=FALSE], desc);
      if (!identical(NA, stat)) out<-cbind(out, round(stat, 4));
      if ('group mean' %in% tolower(extra)) out<-cbind(out, round(gex[, grp.id, drop=FALSE], 4));
      if ('single sample' %in% tolower(extra)) out<-cbind(out, round(gex[, smp.id, drop=FALSE], 4));
      
      if ('description' %in% tolower(ann)) out$Description<-as.vector(anno[id, 'description']);
      
      out;
    }
  }
}