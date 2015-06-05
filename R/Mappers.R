# Helper functions used for mapping IDs, names, etc.


##########################################################################################################################################################
## Mapping a gene symbol to Entrez gene ID
Symbol2Entrez<-function(sym, sym2id, syn2id=NA, ignore.case=TRUE) {
    # sym           A gene symbol
    # sym2id        Pre-compiled symbol to ID mapping
    # syn2id        Optional, pre-compiled gene synoym to ID mapping
    # ignore.case   Whether to ignore the symbol case
    
    if (identical(sym, NA) | identical(sym, '')) '' else {
        s<-paste('^', sym, '$', sep='');
        ind<-grep(s, names(sym2id), ignore.case=ignore.case);
        
        ##############################################
        if (length(ind) > 0) { # found the gene symbol in the names of sym2id
            sort(unique(as.vector(unlist(sym2id[ind], use.names=FALSE)))); # return Entrez gene ID
        } else if (!identical(syn2id, NA) & length(syn2id)>0) { # not found gene symbol, looking gene synonyms instead
            ind<-grep(s, names(syn2id), ignore.case=ignore.case);
            if (length(ind) > 0) { # found the synonym
                sort(unique(as.vector(unlist(syn2id[ind], use.names=FALSE)))); # return Entrez gene ID
            } else '';
        } else '';
        ##############################################
    } 
}
##########################################################################################################################################################


