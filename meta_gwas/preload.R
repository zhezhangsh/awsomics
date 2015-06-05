# Pre-load data when this App is started
AWSOMICS_HOME<-"/zhangz/awsomics/";
RCHIVE_HOME<-"/zhangz/rchive/";
APP_HOME<-paste(AWSOMICS_HOME, "meta_gwas", sep='/');

# subfolders
GWAS_HOME<-paste(RCHIVE_HOME, '/data/gwas', sep=''); # Location of GWAS data
GENE_HOME<-paste(RCHIVE_HOME, '/data/gene/public/entrez/r', sep=''); # Location of gene annotation data
SNP_HOME<-paste(RCHIVE_HOME, '/data/variant', sep=''); # Location of variant information

# load source code 
src<-paste(AWSOMICS_HOME, '/R/', dir(paste(AWSOMICS_HOME, '/R/', sep="")), sep='');
src<-src[grep('.R$', src, ignore.case=TRUE)];
src<-lapply(src, source); # load functions

# Loading housekeeping data and custom function
fn<-paste(APP_HOME, 'source', dir(paste(APP_HOME, 'source', sep='/')), sep='/');
fnc<-sapply(fn, function(fn) if (gregexpr('\\.R$', fn, ignore.case=TRUE)>0) source(fn));

### connect to database tables
tbls<-ConnectToGwasDb(paste(GWAS_HOME, 'db', 'gwas_phred.sqlite', sep='/'));
tbls<-tbls[names(tbls) != 'just_position'];
#tbl.pos<-tbls[['just_position']];
tbl.pos<-ConnectToVariantPositionDb(paste(RCHIVE_HOME, 'data/variant/db/variant_position.sqlite', sep='/'));
names(tbl.pos)<-sub('^dbsnp_', '', names(tbl.pos));
tbl2analysis<-lapply(tbls, function(t) colnames(t)[6:ncol(t)]);

##########################################################################################################
genome.versions<-c("GRCh38", "GRCh37");

# Choices of selectInput menu
search.options<-c('dbSNP ID', 'Genomic Position', 'Entrez Gene ID', 'Official Gene Symbol');
search.extend.options<-c('Both sides', 'Left only', 'Right only', 'Gene upstream', 'Gene downstream');
search.restrict.options<-c('All fields', 'Analysis', 'Study', 'Database table');
factoid.types<-c('Analysis ID', 'Study ID', 'PubMed ID', 'Gene ID', 'Gene symbol');
exclude.tables<-as.list(paste(names(tbls), ' (', sapply(tbls, ncol)-3, ' analyses)', sep=''));
summary.options<-c('SNP', 'Analysis', 'Study', 'Pubmed', 'Keyword');

######################################################################
## Generic meta data
# loading gene annotation
id2gene<-readRDS(paste(GENE_HOME, "human_by_id.rds", sep="/")); # gene annotation list named by Entrez ID
sym2id<-readRDS(paste(GENE_HOME, "human_genes_symbol2id.rds", sep="/")); # gene symbol to gene ID mapping
syn2id<-readRDS(paste(GENE_HOME, "human_genes_synonyms2id.rds", sep="/")); # gene symbol synonyms to gene ID mapping
names(sym2id)<-toupper(names(sym2id));
names(syn2id)<-toupper(names(syn2id));

# loading GWAS metadata
meta.table<-readRDS(paste(GWAS_HOME, "r/table.rds", sep="/"));
meta.study<-readRDS(paste(GWAS_HOME, "r/study.rds", sep="/"));
meta.analysis<-readRDS(paste(GWAS_HOME, "r/analysis.rds", sep="/"));
meta.pubmed<-readRDS(paste(GWAS_HOME, "r/pubmed.rds", sep="/"));
meta.keyword<-readRDS(paste(GWAS_HOME, "r/keyword.rds", sep="/"));
meta.phred<-readRDS(paste(GWAS_HOME, "r/phred_stat.rds", sep="/"));


if (setequal(rownames(meta.phred), rownames(meta.analysis))) {
  meta.phred<-meta.phred[rownames(meta.analysis), ];
} else {
  warning("Unmatched row names of meta.phred and meta.analysis objects");
  cmmn<-intersect(rownames(meta.phred), rownames(meta.analysis));
  meta.phred<-meta.phred[cmmn, ];
  meta.analysis<-meta.analysis[cmmn, ];
}

pub2key<-readRDS(paste(GWAS_HOME, "r/pubmed2keyword.rds", sep="/"));

# DataTables to be browsed 
browse.tbls<-readRDS(paste(GWAS_HOME, "r/browse_table.rds", sep="/")); 
browse.tbls$Genes<-readRDS(paste(GENE_HOME, 'human_genes_datatable.rds', sep='/'));

# Factoids by ID
by.id<-list(
  readRDS(paste(GWAS_HOME, "r/analysis_by_id.rds", sep="/")),
  readRDS(paste(GWAS_HOME, "r/study_by_id.rds", sep="/")),
  readRDS(paste(GWAS_HOME, "r/pubmed_by_id.rds", sep="/")),
  id2gene
);
names(by.id)<-factoid.types[1:4];

# summary numbers
factoid.summary<-readRDS(paste(GWAS_HOME, "r/total_numbers.rds", sep='/'));

#######################################################################################
### Prepare for session-specific data
# fields of analysis and study to be searched against 
desc.fields<-cbind(as.vector(meta.analysis$Name), as.vector(meta.analysis$Description), # a temp table with descriptive info about analysis/study
                   as.vector(meta.study[as.vector(meta.analysis$Study_ID) , 'Name']),
                   as.vector(meta.study[as.vector(meta.analysis$Study_ID) , 'Description']));

# search result table column names
rslt.cnm<-c('Phred', 'Rank','Position', 'SNP', 'Analysis', 'Study', 'PubMed', 'Analysis_Name');

# Data that support enrichment analysis of keywords
key.enrich<-list(
  Analysis=readRDS(paste(GWAS_HOME, "r/keyword2analysis.rds", sep="/")),
  Study=readRDS(paste(GWAS_HOME, "r/keyword2study.rds", sep="/")), 
  PubMed=readRDS(paste(GWAS_HOME, "r/keyword2pubmed.rds", sep="/"))
);


#########################################################################################################################
################################################## Heler Functions ######################################################
# Get rank of phred scores within analysis
phred.top<-meta.phred[, c('Max', colnames(meta.phred)[grep('Top', colnames(meta.phred))])];
phred.top<-list(num=phred.top[, -grep('%', colnames(phred.top))], pct=phred.top[, grep('%', colnames(phred.top))]);
getPhredRank<-function(ana.id, phred, mtrx.lst, meta.ana) { # helper function to get the rank;
  # ana.id    Analysis ID
  # phred     Phred score, the same length as ana.id
  # mtrx.lst  Ranking matrixes of analyses
  # meta.ana  Analysis metadata
  lo<-meta.ana[ana.id, 'Min_Phred'];
  n<-meta.ana[ana.id, 'Num_Variants'];
  
  rnk<-lapply(mtrx.lst, function(mtrx) {
    cn<-colnames(mtrx);
    rk<-rep('', length(ana.id));
    for (i in length(cn):1) rk[phred >= mtrx[ana.id, i]] <- cn[i];
    rk;
  })
  
  rk1<-rnk[[1]];
  rk1[rk1=='Max']<-'Top 1';
  rk2<-rnk[[2]];
  rk3<-rep('', length(rk1));
  rk3[lo>=30]<-paste('Top', n[lo>=30]);     
  
  rk0<-rep('', length(rk1));
  rk0[rk1=='' & rk3!='']<-rk3[rk1=='' & rk3!=''];
  rk0[rk1!='' & rk3=='']<-rk1[rk1!='' & rk3==''];
  rk0[rk1!='' & rk3!='']<-paste('Top', pmin(as.numeric(sub('Top ', '', rk1[rk1!='' & rk3!=''])), as.numeric(sub('Top ', '', rk3[rk1!='' & rk3!='']))));
  
  rk0[rk0=='Top 1']<-'Max';
  rk2[lo>=30]<-'';
  
  rnk<-paste(rk0, rk2, sep='; ');
  rnk<-sub('^; ', '', rnk);
  rnk<-sub('; $', '', rnk);
  
  rnk;
}
