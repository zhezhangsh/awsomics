# Pre-load data when this App is started
AWSOMICS_HOME<-"/zhangz/awsomics_dev/geex";
RCHIVE_HOME<-"/zhangz/rchive/";
APP_HOME<-paste(AWSOMICS_HOME, "geex", sep='/');

# subfolders
GENE_HOME<-paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
GEX_HOME<-paste(RCHIVE_HOME, '/data/gex', sep=''); # Gene expression data collections

# load source code 
src<-paste(AWSOMICS_HOME, '/R/', dir(paste(AWSOMICS_HOME, '/R/', sep="")), sep='');
src<-src[grep('.R$', src, ignore.case=TRUE)];
src<-lapply(src, source); # load functions

# Loading housekeeping data and custom function
fn<-paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc<-sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

##########################################################################################################
# load collection metadata
coll<-readRDS(paste(GEX_HOME, 'r/collection.rds', sep='/'));
select.collection<-paste(rownames(coll), coll$Name, sep=': ');

##########################################################################################################
# Message text
msg.noload<-'No loaded data collection'

