# Pre-load data when this App is started
AWSOMICS_HOME<-"/zhangz/awsomics";
RCHIVE_HOME<-"/zhangz/rchive";
APP_HOME<-paste(AWSOMICS_HOME, "project/sle_h3k4me3_breadth", sep='/');
PROJECT_HOME<-paste(RCHIVE_HOME, 'data/project/public/2015_09_sle_h3k4me3_breadth', sep='/'); # Location of project data in rchive

# subfolders
GENE_HOME<-paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
GENESET_HOME<-paste(RCHIVE_HOME, '/data/gene.set/r', sep=''); # Location of gene annotation data
GEX_HOME<-paste(RCHIVE_HOME, '/data/gex', sep=''); # Gene expression data collections

library(rchive);

# load source code 
src<-paste(AWSOMICS_HOME, '/R/', dir(paste(AWSOMICS_HOME, '/R/', sep="")), sep='');
src<-src[grep('.R$', src, ignore.case=TRUE)];
src<-lapply(src, source); # load functions

# Loading housekeeping data and custom function
fn<-paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc<-sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

##########################################################################################################
# load gene set metadata
geneset<-readRDS(paste(GENESET_HOME, 'metadata_as_tree.rds', sep='/'));

##########################################################################################################
# # load project data
index.page<-readRDS(paste(APP_HOME, 'index.rds', sep='/'));
tss<-readRDS(paste(PROJECT_HOME, 'r', 'tss.rds', sep='/')); # TSS annotation
rna<-readRDS(paste(PROJECT_HOME, 'r', 'rna.rds', sep='/')); # RNA-seq data and results
pwm<-readRDS(paste(PROJECT_HOME, 'r', 'pwm.rds', sep='/')); # Protein binding motifs

cov0<-tss$cov[[1]]; # H3K4me3 coverage around TSS
cov1<-tss$cov[[2]]; # H3K4me3 coverage around TSS
tss.dt<-tss$datatable;

s<-rna$stat;
s<-s[rowSums(s[, 3:4])>0, ];
stat.rna<-cbind(Percentile=round(100*rank(rowMeans(s[, 3:4]))/nrow(s), 2), Log2FPKM=round(log2(rowMeans(s[, 3:4])), 4), 
                 Log2Ratio=s[, 'Log2FC'], p=s[, 'PValue'], FDR=s[, 'FDR']);
s<-tss$stat;
pct<-apply(s[, 3:5], 2, function(x) round(100*rank(x, ties.method = 'min')/length(x), 2))
colnames(pct)<-sub('Mean', 'Percentile', colnames(pct));
stat.tss<-as.matrix(cbind(pct, round(s[, 3:8], 2)));
rownames(stat.tss)<-tss$table$Gene;

gene.u<-intersect(rownames(stat.tss), rownames(stat.rna));
stat.rna<-stat.rna[rownames(stat.rna) %in% gene.u, ];
stat.tss<-stat.tss[rownames(stat.tss) %in% gene.u, ];

venn.tss.choice<-c('TSS, -250 to +250bp', 'Upstream, -400 to -500bp', 'Downstream, +600 to +700bp');

##########################################################################################################
fpkm.scale.options<-c('Log2(FPKM)', 'FPKM', 'Percentile');

##########################################################################################################
plot.cov.choice1<-c('Individual TSS', 'Gene set average');
plot.cov.choice2<-c('Control/SLE separately', 'Control-SLE difference');
plot.cov.choice3<-c('Plot landscape', 'Plot bar');
plot.cov.choice4<-c('Logged', 'Unlogged');

plot.pwm.choice<-colnames(pwm$enrich)[-1];
plot.pwm.col<-c('gold', 'red', 'blue', 'purple', 'green', 'black');
names(plot.pwm.col)<-plot.pwm.choice;

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


