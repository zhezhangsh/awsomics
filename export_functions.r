#!/usr/bin/env Rscript

# Export functions in NAMESPACE file
path<-paste(Sys.getenv('AWSOMICS_HOME'), 'R', sep='/');
fn<-paste(path, dir(path), sep='/');
fn<-fn[grep('.r$', fn, ignore.case=TRUE)];
if (length(fn)>0) {
	fnc<-lapply(fn, function(fn) {
		ln<-scan(fn, what='', flush=TRUE, sep='\n');
    ln<-ln[!grepl('^ ', ln)];
		ln<-gsub(' ', '', ln);
		ln<-ln[grep("<-function\\(", ln)];
		sapply(strsplit(ln, "<-function\\("), function(x) x[1]);
	});
	fnc<-sort(as.vector(unlist(fnc)));
  ln<-sort(paste('export("', fnc, '");', sep=''));
  writeLines(c('', ln, ''), paste(Sys.getenv('AWSOMICS_HOME'), 'NAMESPACE', sep='/'));
}

# Loading function script
#ln<-c(paste('AWSOMICS_HOME<-Sys.getenv("AWSOMICS_HOME");', sep=''), 'library(devtools);');
#ln<-c(ln, paste('source_url("https://raw.githubusercontent.com/zhezhangsh/rchive/master/', sub('./', '', fn), '");', sep=''));
#writeLines(c('', ln, ''), file(paste(Sys.getenv('AWSOMICS_HOME'), 'load_by_url.r', sep='/')));
