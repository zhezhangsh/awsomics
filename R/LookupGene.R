# Look up genes whose synonyms matching searching key
LookupGeneSynonym<-function(key, species, GENE_HOME, ignore.case=TRUE) {
  # key       Search key
  # species   Species of the gene
  # GENE_HOME Home directory of gene info.
  
  msg<-'';
  out<-NA;
  key<-key[1];
  species<-tolower(species);
  
  
  fn.syn2id<-paste(GENE_HOME, '/', species, '_genes_synonyms2id.rds', sep='');
  fn.tbl<-paste(GENE_HOME, '/', species, '_genes_full.rds', sep='');
  
  if (!file.exists(fn.tbl)) { # full gene annotation file not exist
    msg<-paste('Gene annotation file of', species, 'not found.');
    out<-NA;
  } else if (!file.exists(fn.syn2id)) { # synonym-to-gene mapping file not exist
    msg<-paste('Gene synonym file of', species, 'not found.');
    out<-NA;
  } else if (length(key) == 0) { # no search key
    msg<-paste('No search key');
    out<-NA;
  } else if (is.na(key) | key=='') { # invalid search key
    msg<-paste('Empty search key');
    out<-NA;
  } else {
    mp<-readRDS(fn.syn2id);
    tb<-readRDS(fn.tbl);
    
    match<-mp[grep(key, names(mp), ignore.case=ignore.case)];
    id<-unique(unlist(match, use.names=FALSE));
    
    out<-tb[rownames(tb) %in% id, , drop=FALSE];
    msg<-paste('Found', nrow(out), 'matches.');
  }
  
  list(message=msg, result=out);
}