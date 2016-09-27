# Pre-load data when this App is started
AWSOMICS_HOME<-"/zhangz/awsomics";
RCHIVE_HOME<-"/zhangz/rchive";
APP_HOME<-paste(AWSOMICS_HOME, "rnaseq_2g", sep='/');

# subfolders
GENE_HOME<-paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
GENESET_HOME<-paste(RCHIVE_HOME, '/data/gene.set/r', sep=''); # Location of gene annotation data
GEX_HOME<-paste(RCHIVE_HOME, '/data/gex', sep=''); # Gene expression data collections

require(XML);
require(readxl);
require(shinythemes);

require(rchive);
require(awsomics);
require(RoCA);
require(DEGandMore);

# Loading housekeeping data and custom function
fn  <- paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc <- sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

ncls <- 2; 

count_example <- readRDS(paste(APP_HOME, 'data/count_example.rds', sep='/'));
group_example <- list(Control=colnames(count_example)[1:3], Patient=colnames(count_example)[4:6]);

data(DeMethodMeta); 
DeRNAseqMs    <- DeMethodMeta;
method.group  <- c('None', 'Default', 'Fast', 'Fast + Medium', 'Fast + Mediumn + Slow', 'All');
norm.count    <- as.character(as.list(args(DeRNAseq))$norm.count)[-1];
norm.logged   <- as.character(as.list(args(DeRNAseq))$norm.logged)[-1];

plot.type     <- list('Pvalue'=1, 'FDR'=2, 'Volcano'=3, 'M-A'=4);
button.style  <- "color: #fff; background-color: #337ab7; border-color: #2e6da4";

choices.pv    <- list('1' = 1, '0.5' = 0.5, '0.25' = 0.25, '0.10' = 0.1, '0.05' = 0.05, '0.01' = 0.01, '0.001' = 0.001); 
choices.fc    <- list('None' = 0, '5%' = log2(1.05), '10%' = log2(1.1), '25%' = log2(1.25), '50%' = log2(1.5), '100%' = 1,
                      '200%' = log2(3), '400%' = 2, '800%' = 3);
##########################################################################################################
# datatable options
dt.options1 <- list(autoWidth = TRUE, paging = FALSE, searching = FALSE, info = FALSE, 
                    scrollX = '100%', scrollY = "120px", scrollCollapse = FALSE);
dt.options2 <- list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = nrow(DeRNAseqMethods), 
                    pagingType = 'simple', lengthChange = FALSE, searching = FALSE, dom='t');
dt.options3 <- list(autoWidth = TRUE, paging = TRUE,  searching = TRUE,  info = TRUE, 
                    scrollX = '100%', scrollCollapse = FALSE, pageLength = 10);
dt.options4 <- list(autoWidth = TRUE, paging = TRUE,  searching = TRUE,  info = TRUE, 
                    scrollX = '100%', scrollCollapse = FALSE, pageLength = 15);
