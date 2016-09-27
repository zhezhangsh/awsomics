# Pre-load data when this App is started
AWSOMICS_HOME<-"/zhangz/awsomics";
RCHIVE_HOME<-"/zhangz/rchive";
APP_HOME<-paste(AWSOMICS_HOME, "geex", sep='/');

# subfolders
GENE_HOME<-paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
GENESET_HOME<-paste(RCHIVE_HOME, '/data/gene.set/r', sep=''); # Location of gene annotation data
GEX_HOME<-paste(RCHIVE_HOME, '/data/gex', sep=''); # Gene expression data collections

library(rchive);
library(awsomics); 
# load source code
# src<-paste(AWSOMICS_HOME, '/R/', dir(paste(AWSOMICS_HOME, '/R/', sep="")), sep='');
# src<-src[grep('.R$', src, ignore.case=TRUE)];
# src<-lapply(src, source); # load functions

# Loading housekeeping data and custom function
fn<-paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc<-sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

##########################################################################################################
# load collection metadata
coll<-readRDS(paste(GEX_HOME, 'r/collection.rds', sep='/'));
select.collection<-paste(rownames(coll), coll$Name, sep=': ');

##########################################################################################################
# load gene set metadata
geneset<-readRDS(paste(GENESET_HOME, 'metadata_as_tree.rds', sep='/'));

##########################################################################################################
# Column names of expression data matrix
expr_columns<-c("Group mean", "Single sample");
expr_anno_columns<-c('Name', 'Species', 'Gene_Type', 'Num_Dataset', 'Num_Sample', 'Synonyms', 'Description');

##########################################################################################################
# options
data.level.options<-c('Data set', 'Group', 'Sample');

##########################################################################################################
# Message text
msg.nocollection<-'No loaded data collection';
msg.nodataset<-'No selected data set';
msg.nogroup<-'No selected group';
msg.nogene<-'No selected gene';
msg.nogeneset<-'No selected gene set';

##########################################################################################################
# datatable options
dt.options1<-list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 50 , 
                  sScrollX = '100%');
dt.options2<-list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 5  , 
                  pagingType = 'simple', lengthChange = FALSE, sScrollX = '100%');
dt.options3<-list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 10 , 
                  pagingType = 'simple', lengthChange = FALSE, sScrollX = '100%');
dt.options4<-list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 5  , 
                  pagingType = 'simple', lengthChange = FALSE, searching = FALSE);
dt.options5<-list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 100, 
                  pagingType = 'simple', lengthChange = FALSE, searching = FALSE, dom='t');


