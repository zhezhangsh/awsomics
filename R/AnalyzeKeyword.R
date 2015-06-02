# Helper functions used for analyzing keywords

##########################################################################################################################################################
# Count word occurance
CountWordsFreq<-function(txt) {
    library(tm);
    library(SnowballC);

    ch<-c('-', '_', '/', ':');
    for (i in 1:length(ch)) txt<-gsub(ch[i], ' ', ta);
    
    wds<-strsplit(tolower(removePunctuation(txt)), ' ');
    wds<-unlist(wds, use.names=FALSE);
    wds<-removeWords(wds, stopwords('SMART'));
    wds<-wds[wds!=''];
    wds<-wds[removeNumbers(wds) == wds];
    tbl<-table(wds);
    ct<-as.vector(tbl);
    names(ct)<-names(tbl);
    sort(ct)[length(ct):1];
}

##########################################################################################################################################################
# Evaluate the overlapping of 2 text strings
TextOverlap<-function(ta, tb, exclude=c()) {
    # ta, tb       2 text strings, words separated by space. 
    # exclude      Words to be excluded
    
    library(tm);
    library(SnowballC);
    
    ch<-c('-', '_', '/', ':');
    for (i in 1:length(ch)) {
        ta<-gsub(ch[i], ' ', ta);
        tb<-gsub(ch[i], ' ', tb);
    }
    
    # split into lower case words and clean up words
    wa<-strsplit(tolower(removePunctuation(ta)), ' ')[[1]];
    wb<-strsplit(tolower(removePunctuation(tb)), ' ')[[1]];
    wa<-removeWords(wa, stopwords('SMART'));
    wb<-removeWords(wb, stopwords('SMART'));
    wa<-wa[wa!=''];
    wb<-wb[wb!=''];
    wa<-stemDocument(wa);
    wb<-stemDocument(wb);
    
    if (length(exclude) > 0) {
        exclude<-tolower(exclude);
        wa<-wa[!(wa %in% exclude)];
        wb<-wb[!(wb %in% exclude)];
    }
    
    wds<-list(
        WordsA=wa,
        WordsB=wb,
        WordsBoth=intersect(wa, wb),
        AinB=wa[wa %in% wb],
        BinA=wb[wb %in% wa]
    );
    
    list(Words=wds, N=sapply(wds, length))
}

##########################################################################################################################################################
# Text mining of genes in plain documents
PlainDocGene<-function(docs, genes, synonyms=NA, upper1st=TRUE) {
    library(tm);
    library(SnowballC);

    # whether gene synonyms were provided
    if (identical(synonyms, NA)) synonyms<-as.list(genes) else synonyms<-synonyms[genes];
    names(synonyms)<-genes;
    
    # all synonyms
    sym<-c(genes, rep(names(synonyms), sapply(synonyms, length)));
    syn<-c(genes, unlist(synonyms, use.names=FALSE));
    names(syn)<-sym;
    
    # documents into words
    #docs<-gsub('-', '', docs);
    words<-lapply(docs, function(doc) strsplit(removePunctuation(doc), ' ')[[1]]);
    words<-lapply(words, function(w) w[grep('^[A-Z]', w)]);
    words<-lapply(words, function(w) w[nchar(w)>2]); # at least 3 letters
    wrd<-unique(unlist(words, use.names=FALSE));
    
    # synonyms appeared at least once in all docs
    syn<-syn[tolower(syn) %in% tolower(wrd)];
    lwr<-tolower(syn); 

    # mapping synonums to individual docs
    syn2doc<-sapply(words, function(w) as.integer(as.vector(lwr) %in% tolower(w)));
    
    gn<-sort(unique(names(syn))); # unique gene name
    gene2doc<-t(sapply(gn, function(gn) pmin(1, colSums(syn2doc[names(syn)==gn, , drop=FALSE]))));
    
    # map gene to synonyms
    gene2syn<-split(as.vector(syn), names(syn));
    gene2syn<-lapply(gene2syn, unique);
    gene2syn<-lapply(gene2syn, sort);
    gene2syn<-gene2syn[rownames(gene2doc)];
    names(gene2syn)<-rownames(gene2doc);
    
    list(gene2doc=gene2doc, synonyms=gene2syn);
}

##########################################################################################################################################################
# Text mining of plain documents
PlainDocMining<-function(docs, countlimit=c(2, length(docs)/5), corlimit=0.5, lengthlimit=c(3, 12)) {
    # docs          Plain documents as string vector
    # countlimit    Limits of the occurance of the terms
    # corlimit      Corrletion limit between terms that are associated
    
    library(tm);
    library(SnowballC);
        
    # Create a Corpus object defined by the tm package for the following steps
    docs<-tolower(iconv(docs,"WINDOWS-1252","UTF-8"));
    docs<-gsub('-', ' ', docs);
    
    crps<-Corpus(VectorSource(docs), readerControl = list(language="english"));
    
    # preprocessing docs
    crps<-tm_map(crps, removeWords, stopwords('SMART'));
    crps<-tm_map(crps, removeNumbers);
    crps<-tm_map(crps, removePunctuation);
    crps<-tm_map(crps, stripWhitespace);
    
    # Full words before stemming
    words<-lapply(crps, function(x) strsplit(as.character(x), ' '));
    words<-sort(unique(unlist(words, use.names=FALSE)))
    words<-words[nchar(gsub('[a-z]', '', words))==0];
    
    
    crps<-tm_map(crps, stemDocument);
    
    
    tdm<-TermDocumentMatrix(crps);
    
    # Convert to matrix
    m<-as.matrix(tdm);
    m[m>1]<-1; # Don't count terms more than once in one doc
    
    ############################################################
    # remove terms
    nc<-nchar(rownames(m));
    m<-m[nc>=min(lengthlimit) & nc<=max(lengthlimit),]; # remove long and short terms
    
    # URL-like
    www<-grep('www', rownames(m)); # remove URL-like terms
    if (length(www) > 0) m<-m[-www, ];
    
    # foreign characters
    nc<-nchar(gsub('[a-z]', '', rownames(m))); # remove terms with non-English characters
    m<-m[nc==0, , drop=FALSE];
    
    # only include acgt
    m<-m[nchar(gsub('acgt', '', rownames(m)))>0, ];
    ############################################################

    
    ############################################################
    # remove too frequent terms
    # too frequent
    sm<-rowSums(m);
    freq.terms<-rownames(m)[sm > max(countlimit)];
    m0<-m[freq.terms, , drop=FALSE];
    
    # frequency within range
    m<-m[sm>=min(countlimit) & sm<=max(countlimit), , drop=FALSE]; # remove singleton terms
    
    # correlation to frequent terms
    corr<-cor(t(m0), t(m));
    trm<-apply(corr, 1, function(r) colnames(corr)[r>corlimit]); # terms closely correlated to frequent terms
    trm<-unique(unlist(trm));
    
    m<-m[!(rownames(m) %in% trm), ];
    ############################################################
    
    ############################################################
    # term-synonyms mapping
    # synonyms
    syn<-lapply(rownames(m), function(trm) words[grep(paste('^', trm, sep=''), words)]);
    
    # synonyms of each term
    trm<-rep(rownames(m), sapply(syn, length));
    syn<-unlist(syn);
    
    # if a synonym is associated to more than one term, use the longer one
    syn2trm<-split(trm, syn);
    trm<-sapply(syn2trm, function(trm) trm[order(nchar(trm))][length(trm)]);
    
    trm2syn<-split(names(syn2trm), trm)[rownames(m)];
    names(trm2syn)<-rownames(m);
    
    list(term2doc=m, synonym=trm2syn)
}
