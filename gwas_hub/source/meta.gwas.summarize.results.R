# Utility function that summarizes search results for the "Secondary analyses -> "Summary" tab
meta.gwas.summarize.results<-function(rslt, opt, summary.options, minimum.phred) {
  # rslt              Result set returned from the search.rslt<-reactive reactive expression
  # opt               User specified summary option
  # summary.options   Available summary options
  # minimum.phred     The minimum phred score of variants to be included in the summary
  
  # re-formatting the result table
  # add an column (analysis name)
  if(identical(NA, rslt)) smm<-data.frame('Message'="No search results") else if (is.character(rslt$phred)) smm<-data.frame('Message' = rslt$phred) else if (nrow(rslt$phred) == 0 ) smm<-data.frame('Message' = 'No GWAS rsult was found.') else {    
    ana.ttl<-rslt$analysis;
    #std.ttl<-rslt$study;
    
    rslt<-rslt$phred; # full search result
    rslt.up<-rslt[rslt$Phred>=minimum.phred, , drop=FALSE]; # search result with Phred score above cutoff
    
    ##############################################################################################################
    # calculate keyword enrichment
    if (nrow(rslt.up) == 0) smm<-data.frame('Message' = paste('No Phred score over', minimum.phred)) else {      
      if (opt==summary.options[1]) { # summarize results by SNP ID
        smm<-rslt.up[!duplicated(rslt.up$SNP), c('SNP', 'Chromosome', 'Position'), drop=FALSE];
        rownames(smm)<-as.vector(smm[[1]]);
        p<-as.vector(rslt.up$Phred);
        names(p)<-as.vector(rslt.up$Analysis);
        p<-split(p, as.vector(rslt.up$SNP))[rownames(smm)];
        mx<-lapply(p, function(mx) sort(mx)[length(mx)]);
        smm$Num_Phred<-sapply(p, length);
        smm$Max_Phred<-as.vector(unlist(mx));
        smm$Max_Analysis<-sapply(mx, names);
        a<-meta.analysis[smm$Max_Analysis, , drop=FALSE];
        smm$Analysis_Name<-as.vector(a$Name);
        smm$SNP<-AddHref(smm$SNP, UrlDbsnpId(smm$SNP));
        smm$Max_Analysis<-AddHref(rownames(a), a$URL);
        smm<-smm[order(smm$Max_Phred, decreasing=TRUE), , drop=FALSE];
      
        } else if (opt==summary.options[2]) { # summarize results by analysis ID
        smm<-rslt.up[!duplicated(rslt.up$Analysis), c(5, 3, 4, 1, 2), drop=FALSE];
        p<-rslt.up[, 'Phred'];
        names(p)<-rownames(rslt.up);
        p<-split(p, as.vector(rslt.up$Analysis));
        mx<-sapply(p, function(p) names(sort(p))[length(p)]);
        smm[, -1]<-rslt.up[mx, colnames(smm)[-1]];
        colnames(smm)[2:ncol(smm)]<-paste('Max', colnames(smm)[2:ncol(smm)], sep='_');
        smm$Num_Phred<-sapply(p, length);
        smm<-smm[, c(1, ncol(smm), 2:(ncol(smm)-1)), drop=FALSE];
        smm$Analysis_Name<-as.vector(meta.analysis[as.vector(smm[[1]]), 'Name']); 
        smm$Max_SNP<-AddHref(smm$Max_SNP, UrlDbsnpId(smm$Max_SNP));
        smm$Analysis<-AddHref(as.vector(smm$Analysis), meta.analysis[as.vector(smm$Analysis), 'URL']);          
        smm<-smm[order(smm$Max_Phred, decreasing=TRUE), ];
      
        } else if (opt==summary.options[3]) { # summarize results by study ID
        smm<-rslt.up[!duplicated(rslt.up$Study), c(6, 3, 4, 1, 2, 5), drop=FALSE];
        p<-rslt.up[, 'Phred'];
        names(p)<-rownames(rslt.up);
        p<-split(p, as.vector(rslt.up$Study));
        mx<-sapply(p, function(p) names(sort(p))[length(p)]);
        smm[, -1]<-rslt.up[mx, colnames(smm)[-1]];
        colnames(smm)[2:ncol(smm)]<-paste('Max', colnames(smm)[2:ncol(smm)], sep='_');
        smm$Num_Phred<-sapply(p, length);
        smm<-smm[, c(1, ncol(smm), 2:(ncol(smm)-1)), drop=FALSE];
        smm$Study_Name<-as.vector(meta.study[as.vector(smm[[1]]), 'Name']); 
        smm$Max_SNP<-AddHref(smm$Max_SNP, UrlDbsnpId(smm$Max_SNP));
        smm$Max_Analysis<-AddHref(as.vector(smm$Max_Analysis), meta.analysis[as.vector(smm$Max_Analysis), 'URL']);    
        smm$Study<-AddHref(as.vector(smm$Study), meta.study[as.vector(smm$Study), 'URL']);          
        smm<-smm[order(smm$Max_Phred, decreasing=TRUE), ];
      
        } else if (opt==summary.options[4]) { # summarize results by pubmed ID
        smm<-rslt.up[!duplicated(rslt.up$PubMed), c(7, 3, 4, 1, 2, 5), drop=FALSE];
        p<-rslt.up[, 'Phred'];
        names(p)<-rownames(rslt.up);
        p<-split(p, as.vector(rslt.up$PubMed));
        mx<-sapply(p, function(p) names(sort(p))[length(p)]);
        smm[, -1]<-rslt.up[mx, colnames(smm)[-1]];
        colnames(smm)[2:ncol(smm)]<-paste('Max', colnames(smm)[2:ncol(smm)], sep='_');
        smm$Num_Phred<-sapply(p, length);
        smm<-smm[, c(1, ncol(smm), 2:(ncol(smm)-1)), drop=FALSE];
        smm$PubMed_Title<-as.vector(meta.pubmed[as.vector(smm[[1]]), 'Title']); 
        smm$Max_SNP<-AddHref(smm$Max_SNP, UrlDbsnpId(smm$Max_SNP));
        smm$Max_Analysis<-AddHref(as.vector(smm$Max_Analysis), meta.analysis[as.vector(smm$Max_Analysis), 'URL']);    
        smm$PubMed<-AddHref(as.vector(smm$PubMed), meta.pubmed[as.vector(smm$PubMed), 'URL']);          
        smm<-smm[order(smm$Max_Phred, decreasing=TRUE), ];
      
        } else if (opt==summary.options[5]) { # summarize results by keyword ID, map to keyword by pubmed ID
        ky<-pub2key[rslt.up$PubMed];
        ky.all<-unlist(ky, use.names=FALSE); 
        rnm<-rep(rownames(rslt.up), sapply(ky, length));
        rup<-rslt.up[rnm, , drop=FALSE];
        rup$Keyword<-ky.all;
        smm<-rup[!duplicated(rup$Keyword), c(8, 3, 4, 1, 2, 5, 7), drop=FALSE];
        p<-rup[, 'Phred'];
        names(p)<-rownames(rup);
        p<-split(p, as.vector(rup$Keyword));
        mx<-sapply(p, function(p) names(sort(p))[length(p)]);
        smm[, -1]<-rup[mx, colnames(smm)[-1]];
        colnames(smm)[2:ncol(smm)]<-paste('Max', colnames(smm)[2:ncol(smm)], sep='_');
        smm$Num_Phred<-sapply(p, length);
        smm<-smm[, c(1, ncol(smm), 2:(ncol(smm)-1)), drop=FALSE];
        smm$Keyword_Type<-as.vector(meta.keyword[as.vector(smm[[1]]), 'Type']);
        smm$Keyword_Name<-as.vector(meta.keyword[as.vector(smm[[1]]), 'Name']); 
        smm$Max_SNP<-AddHref(smm$Max_SNP, UrlDbsnpId(smm$Max_SNP));
        smm$Max_Analysis<-AddHref(as.vector(smm$Max_Analysis), meta.analysis[as.vector(smm$Max_Analysis), 'URL']);    
        smm$Max_PubMed<-AddHref(as.vector(smm$Max_PubMed), meta.pubmed[as.vector(smm$Max_PubMed), 'URL']);  
        smm$Keyword<-AddHref(as.vector(smm$Keyword), meta.keyword[as.vector(smm$Keyword), 'URL']); 
        smm<-smm[order(smm$Max_Phred, decreasing=TRUE), ];
      
        } else {
          smm<-data.frame('Message' = paste('Unknown summary option:', opt))
        }
    }
    ##############################################################################################################
  }
  
  if (identical(smm, NA)) smm<-data.frame("Message"="No GWAS rsult was found.");
  if (colnames(smm)[1] == "Message") colnames(smm)<-"";
  
  smm;
}