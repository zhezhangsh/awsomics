# Helper functions generate URLs to different types of database

##########################################################################################################################################################
# Make URLs to UCSC genome browser by specifying genomic position
UrlUcscPosition<-function(chr, from, to=from, min.ws=100, genome='hg38') {
    # chr       Chromosome name as chr1, chrX
    # from, to  Position on chromosome
    # min.ws    Minimal number of basepairs to show in UCSC
    # genome    Genome version
    
    genome<-as.vector(genome);
    chr<-as.vector(chr);
    from<-as.vector(from);
    to<-as.vector(to);
    
    genome<-tolower(genome);
    if (genome[1]=='grch38') genome<-'hg38' else if (genome[1]=='grch37') genome<-'hg19';
    
    min.ws<-max(1, min.ws, na.rm=TRUE);
    
    # chromsome name starts with 'chr'
    chr<-tolower(chr);
    chr[!grepl('^chr', chr)]<-paste('chr', chr[!grepl('^chr', chr)], sep='');
    
    # get positions
    from<-as.integer(from);
    to<-as.integer(to);
    from[is.na(from)]<-to[is.na(to)];
    to[is.na(to)]<-from[is.na(from)];
    
    stt<-pmin(from, to);
    end<-pmax(from, to);
    mid<-(stt+end)/2;
    wid<-end-stt+1;
    
    # extend to minimal window size
    stt[wid<min.ws]<-max(1, round(mid[wid<min.ws]-min.ws/2));
    end[wid<min.ws]<-round(mid[wid<min.ws]+min.ws/2);
    
    url<-paste('http://genome.ucsc.edu/cgi-bin/hgTracks?db=', genome, '&position=', chr, '%3A', stt, '-', end, sep='');
    url[is.na(from) | is.na(to)]<-'';
    
    url
}

# Make URL to NCBI gene
UrlEntrezGene<-function(ids) {
  paste("http://www.ncbi.nlm.nih.gov/gene/?term=", ids, sep='');
}

##########################################################################################################################################################
# Make URLs to dbSNP with dbSNP IDs
UrlDbsnpId<-function(ids) {
    # ids   dbSNP IDs
    
    ids<-sub('^rs', '', as.character(ids), ignore.case=TRUE);
    
    paste('http://www.ncbi.nlm.nih.gov/projects/SNP/snp_ref.cgi?rs=', ids, sep='');
}