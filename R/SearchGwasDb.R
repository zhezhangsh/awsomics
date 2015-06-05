# A set of utility functions to search GWAS p values in a sqlite database

################################################################################################################################
################################################################################################################################
# Get the GWAS p values of a SNP from the SQLite database
SearchGwasDbById<-function(ids, tbls=NA, db=NA, fn=NA, genome=c('GRCh38', 'GRCh37'), min.phred=0, correct.id=TRUE, tables.out=NA, analyses.out=NA) {
	# ids							One or more dbSNP IDs to search in the database
	# tbls, db, fn					Data source, could be connections to a set of tables (tbls); a connection to the database (db); or file name of the database (fn)
    # genome                          Genome version
    # min.phred						Minimum phred score to be included in the results
	# correct.id                      Correct dbSNP IDs if they have the wrong format
    # tables.out, analyses.out		Tables and analyses to be excluded from the results

    library(dplyr);
    
	# Get all the database tables
	if (identical(tbls, NA)) {
		if (!identical(db, NA)) tbls<-ConnectToGwasDb(db) else tbls<-ConnectToGwasDb(fn); # connect to database and its tables
	}
    
    # get table names and exclude tables not to query
	names(tbls)<-sapply(tbls, function(t) as.character(t$from));
    tbls<-tbls[!(names(tbls) %in% tables.out)];
    
    # Make sure ids are in the right format
    if (correct.id) {
        ids<-tolower(ids);
        ind<-which(!grepl('^rs', ids));
        if (length(ind) > 0) ids<-paste('rs', ids, sep='');
    }
    
    ###########################################################################
    if (length(tbls) == 0) data.frame(Chromosome=character(0), Position=integer(0), Phred=integer(0), SNP=character(0), Analysis=character(0)) else {
        # Make IDs in the right format
        ids<-tolower(ids);
        ids[!grepl("^rs", ids)]<-paste("rs", ids[!grepl('^rs', ids)], sep='');
        ids<-gsub("[\t ]", "", ids);
        
        ###########################################################################
        # query tables to get phred scores
        phred<-lapply(tbls, function(t) {
            if (length(ids)==1) out<-dplyr::filter(t, id==ids) else out<-dplyr::filter(t, id %in% ids);
            if (genome[1]=='GRCh37') ind<-c(1,2,4, 6:ncol(out)) else ind<-c(1,3,5,6:ncol(out)); 
            out<-dplyr::select(out, ind); 
            CollectGwasDbData(out, min.phred, analyses.out=analyses.out);
        });
                
        # combine search results and return values
        out<-do.call("rbind", phred);
        if(nrow(out)>0) rownames(out)<-1:nrow(out);
        out;
    }
}
################################################################################################################################


################################################################################################################################
################################################################################################################################
# Get the GWAS p values of all SNPs within a specified region from the SQLite database
SearchGwasDbByLoci<-function(chrom, start, end, tbls=NA, db=NA, fn=NA, genome=c('GRCh38', 'GRCh37'),  min.phred=0, tables.out=NA, analyses.out=NA) {
	# chrom, start, end				Chromosome name, start position and end position
	# tbls, db, fn					Data source, could be connections to a set of tables (tbls); a connection to the database (db); or file name of the database (fn)
	# genome                          Genome version
    # min.phred						Minimum phred score to be included in the results
	# tables.out, analyses.out		Tables and analyses to be excluded from the results
	
    library(dplyr);
    
	# Get all the database tables
	if (identical(tbls, NA)) {
		if (!identical(db, NA)) tbls<-ConnectToGwasDb(db) else tbls<-ConnectToGwasDb(fn); # connect to database and its tables
	}
    
    # remove prefix in chromosome name
    chrom<-sub('^chr', '', chrom, ignore.case=TRUE);
    chrom<-sub('^ch', '', chrom, ignore.case=TRUE);

    # get table names and exclude tables not to query
	names(tbls)<-sapply(tbls, function(t) as.character(t$from));
    tbls<-tbls[!(names(tbls) %in% tables.out)];
    
    if (is.na(end[1])) end[1]<-start[1];
    
    ###########################################################################
    if (length(tbls) == 0) data.frame(Analysis=character(0), SNP=character(0), Chromosome=character(0), Position=integer(0), Phred=integer(0)) else {
        # query tables to get phred scores
        phred<-lapply(tbls, function(t) {
            if (genome[1] == 'GRCh37') {
                out<-dplyr::filter(t, GRCh37_chr==chrom[1], GRCh37_pos>=start[1], GRCh37_pos<=end[1]);
                ind<-c(1, 2, 4, 6:ncol(out));
            } else {
                out<-dplyr::filter(t, GRCh38_chr==chrom[1], GRCh38_pos>=start[1], GRCh38_pos<=end[1]);
                ind<-c(1, 3, 5, 6:ncol(out))
            }
            CollectGwasDbData(dplyr::select(out, ind), min.phred, analyses.out=analyses.out);
        });
        
        # combine search results and return values
        out<-do.call("rbind", phred);
        if(nrow(out)>0) rownames(out)<-1:nrow(out);
        out;
    }
}
################################################################################################################################


################################################################################################################################
################################################################################################################################
# Get the position of a SNP from GWAS SQLite database
SnpPosFromGwasDb<-function(ids, tbl, correct.id=TRUE, as.GRanges=FALSE, genome=c('GRCh38', 'GRCh37')) {
    # ids							One or more dbSNP IDs to search in the database
    # tbl                             Table to be queired, with columns 'id', 'chr', 'pos', 'width'
    # correct.id                      Correct dbSNP IDs if they have the wrong format
    # as.GRanges                      Whether to return result as a GRanges object
	# genome                          Genome version

    library(dplyr);
    
    # Make sure ids are in the right format
    if (correct.id) {
        ids<-tolower(ids);
        ind<-which(!grepl('^rs', ids));
        if (length(ind) > 0) ids<-paste('rs', ids, sep='');
    }
   
    ###########################################################################
    if (length(ids) > 1) t<-as.data.frame(dplyr::collect(dplyr::filter(tbl, id %in% ids))) else t<-as.data.frame(dplyr::collect(dplyr::filter(tbl, id==ids)));
    
    if (genome[1] == 'GRCh37') t<-t[, c(1, 2, 4)] else t<-t[, c(1, 3, 5)];

    if (as.GRanges) {
        library(GenomicRanges);
        gr<-GRanges(as.vector(t[['chr']], IRanges(t[['pos']], widht=t[['width']])));
        names(gr)<-as.vector(t[['id']]);
        gr;
    } else t;    
}


################################################################################################################################
################################################################################################################################
# Connect to the SQLite database of GWAS results; Return a set of connections to the database tables
ConnectToGwasDb<-function(db, required.columns=c("id", "GRCh37_chr", "GRCh38_chr", "GRCh37_pos", "GRCh38_pos")) {
	# db	An established connection to the GWAS sqlite database, returned by the src_sqlite function; or a full file name to the database location
	
	library(dplyr);
	
	if (identical(class(db), "character")) db<-src_sqlite(db);
	
	# table names
	t<-src_tbls(db);
	#t<-t[t != 'sqlite_stat1']; # remove stat table
	
	# connect to tables
	tbls<-lapply(t, function(t) tbl(db, t));
	
    # remove tables without required columns
    has.columns<-sapply(tbls, function(t) length(required.columns[required.columns %in% colnames(t)]) == length(required.columns));
    tbls<-tbls[has.columns];
    names(tbls)<-t[has.columns];
	
	tbls;
}
################################################################################################################################


################################################################################################################################
################################################################################################################################
# Collect data from GWAS SQLite database
CollectGwasDbData<-function(tbl, min.phred=0, analyses.out=NA) {
    # tbl                   A SQLite query results from the GWAS SQLite database
    # min.phred             Minimum phred score to be included in the results
    # analyses.out          Analyses to be excluded from the results

	library(dplyr);
    
    # select analysis columns not to exclude
    if (!identical(analyses.out, NA)) {
        ind<-which(!(colnames(tbl) %in% analyses.out));
        ind<-c(1:3, ind[ind>3]);
        tbl<-dplyr::select(tbl, ind);
    }
    
    if (nrow(tbl)==0 | ncol(tbl)==3) {
        data.frame(Chromosome=character(0), Position=integer(0), Phred=integer(0), SNP=character(0), Analysis=character(0));
    } else {
        tbl<-as.data.frame(dplyr::collect(tbl)); # collect query result and convert to a data.frame

        ana<-rep(colnames(tbl)[4:ncol(tbl)], each=nrow(tbl)); # Analysis ID
        snp<-rep(as.vector(tbl[[1]]), ncol(tbl)-3); # SNP ID
        chr<-rep(as.vector(tbl[[2]]), ncol(tbl)-3); # chromosome
        pos<-rep(as.vector(tbl[[3]]), ncol(tbl)-3); # position

        
        # Get Phred scores
        phred<-tbl[, 4:ncol(tbl), drop=FALSE];
        phred<-as.matrix(phred[, !(colnames(phred) %in% analyses.out), drop=FALSE]); # Exclude analysis
        
        out<-data.frame(Chromosome=chr, Position=pos, Phred=as.vector(phred), SNP=snp, Analysis=ana, stringsAsFactors=FALSE);
        #for (i in 1:3) out[[i]]<-as.vector(out[[i]]);
        out<-out[!is.na(out$Phred) & out$Phred >= min.phred, , drop=FALSE];
        
        if (nrow(out)>0) rownames(out)<-1:nrow(out);
        
        out;
    }
}
################################################################################################################################
