geex.lookup.gene<-function(anno, species, key, GENE_HOME) {
  gn<-LookupGeneSynonym(key, species, GENE_HOME)
  
  if (identical(gn$result, NA)) geex.empty.matrix(gn$message) else {
    t<-anno[rownames(anno) %in% rownames(gn$result), , drop=FALSE];
    if (nrow(t) == 0) geex.empty.matrix('No matching gene in data set') else t[, 1:2];
  }
}