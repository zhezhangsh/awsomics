# Perform keyword enrichment analysis based on search results
meta.gwas.enrichment<-function(rslt, minimum.phred, minimum.count, key.option, key.enrich) {   
  # re-formatting the result table
  # add an column (analysis name)    
  if(identical(NA, rslt)) rich.tbl<-data.frame('Message'="No search results") else if (is.character(rslt$phred)) rich.tbl<-data.frame('Message' = rslt$phred) else if (nrow(rslt$phred) == 0 ) rich.tbl<-data.frame('Message' = 'No GWAS rsult was found.') else {   
    # full result table and result table including only Phred score greater than threshold   
    rslt.ttl<-rslt$phred; # full search result
    rslt.up<-rslt.ttl[rslt.ttl$Phred>=minimum.phred, , drop=FALSE]; # search result with Phred score above cutoff
    
    ##############################################################################################################
    # calculate keyword enrichment
    if (nrow(rslt.up) == 0) rich.tbl<-data.frame('Message' = paste('No Phred score over', minimum.phred)) else {
      if (!(key.option %in% names(key.enrich))) rich.tbl<-data.frame('Message' = paste('Unknown type:', key.option)) else {              
        id.ttl<-unique(rslt.ttl[, key.option]);
        id.up<-unique(rslt.up[rslt.up$Phred>minimum.phred, key.option]);
        
        key.map<-key.enrich[[key.option]];
        key.ttl<-key.map[, colnames(key.map) %in% id.ttl, drop=FALSE];
        key.up<-key.ttl[, colnames(key.ttl) %in% id.up, drop=FALSE];
        
        # calculate odds ratios as enrichment
        n11<-rowSums(key.up);
        n10<-ncol(key.up)-n11;
        n01<-rowSums(key.ttl)-n11;
        n00<-ncol(key.ttl)-n11-n01-n10;
        #n<-cbind(n00, n01, n10, n11);
        or<-(n11*n00)/(n01*n10);
        or[is.na(or)]<-1;
        
        # The max Phred score associated to a keyword
        max.phred<-sapply(split(rslt.ttl$Phred, rslt.ttl[[key.option]]), max)[colnames(key.up)]; 
        max.phred<-apply(key.up, 1, function(mp) {
          if(sum(mp) < minimum.count) 0 else max(max.phred[colnames(key.up)[mp>0]]);
        })
        
        rich.tbl<-data.frame(rownames(meta.keyword), meta.keyword[, 'Type'], max.phred[rownames(meta.keyword)], ncol(key.up), n11, ncol(key.ttl), n11+n01, or, meta.keyword[, 'Name'], stringsAsFactors=FALSE);
        colnames(rich.tbl)<-c('ID', 'Type', 'Max_Phred', paste('Num', key.option, sep='_'), 'Occurance', paste('Total', key.option, sep='_'), 'Total_Occurance', 'Enrichment', 'Name')
        rich.tbl<-rich.tbl[rich.tbl$Occurance >= minimum.count, , drop=FALSE];
        rich.tbl<-rich.tbl[order(rich.tbl$Occurance, rich.tbl$Max_Phred, decreasing=TRUE), , drop=FALSE];
        if (nrow(rich.tbl) > 0) rich.tbl$ID<-AddHref(rich.tbl$ID, as.vector(meta.keyword[rich.tbl$ID, 'URL']));
      }
    }
    ##############################################################################################################
  }
  
  if (identical(rich.tbl, NA)) rich.tbl<-data.frame("Message"="No GWAS rsult was found.");
  if (colnames(rich.tbl)[1] == "Message") colnames(rich.tbl)<-"";
  
  rich.tbl;
}