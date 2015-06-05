# A set of utility functions to search variants in a sqlite database

################################################################################################################################
################################################################################################################################
# Get the GWAS p values of a SNP from the SQLite database
SearchVariantPositionDbById<-function(ids, tbl, correct.id=TRUE, as.GRanges=FALSE) {
    # ids							One or more dbSNP IDs to search in the database
    # tbl                             Table to be queired, with columns 'id', 'chr', 'pos', 'width'
    # correct.id                      Correct dbSNP IDs if they have the wrong format
    # as.GRanges                      Whether to return result as a GRanges object
    library(dplyr);
    
    # Make sure ids are in the right format
    if (correct.id) {
        ids<-tolower(ids);
        ind<-which(!grepl('^rs', ids));
        if (length(ind) > 0) ids<-paste('rs', ids, sep='');
    }
   
    ###########################################################################
    if (length(ids) > 1) t<-as.data.frame(collect(filter(tbl, id %in% ids))) else t<-as.data.frame(collect(filter(tbl, id==ids)));
    
    if (as.GRanges) {
        library(GenomicRanges);
        gr<-GRanges(as.vector(t[['chr']], IRanges(t[['pos']], widht=t[['width']])));
        names(gr)<-as.vector(t[['id']]);
        gr;
    } else t;    
}

################################################################################################################################
################################################################################################################################
# Get the positions of all variants within a specified region from the SQLite database
SearchVariantPositionDbByLoci<-function(chrom, start, end, tbl) {
	# chrom, start, end				Chromosome name, start position and end position
	# tbl                             Data source, a connection to database table with variant position info. 
	
    library(dplyr);
        
    # remove prefix in chromosome name
    chrom<-sub('^chr', '', chrom, ignore.case=TRUE);
    chrom<-sub('^ch', '', chrom, ignore.case=TRUE);
    
    if (is.na(end[1])) end[1]<-start[1];
    
    ###########################################################################
    out<-filter(tbl, chr==chrom[1], pos>=start[1], pos<=end[1]);
    as.data.frame(collect(out));
}
################################################################################################################################



################################################################################################################################
################################################################################################################################
# Connect to the SQLite database of variant position information; Return a set of connections to the database tables
ConnectToVariantPositionDb<-function(db, required.columns=c('id', 'chr', 'pos', 'width')) {
	# db        An established connection to the sqlite database, returned by the src_sqlite function; or a full file name to the database location
	
	library(dplyr);
	
	if (identical(class(db), "character")) db<-src_sqlite(db);
	
	# table names
	t<-src_tbls(db);
	
	# connect to tables
	tbls<-lapply(t, function(t) tbl(db, t));
	
    # remove tables without required columns
    has.columns<-sapply(tbls, function(t) length(required.columns[required.columns %in% colnames(t)]) == length(required.columns));
    tbls<-tbls[has.columns];
    names(tbls)<-t[has.columns];
	
	tbls;
}