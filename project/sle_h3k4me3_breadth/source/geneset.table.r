# prepare the gene set table for group comparison
geneset.table<-function(gs, src, col, spe) {
  src<-src[1];
  col<-col[1];
  spe<-spe[1];
  
  empty.matrix<-function(cnm) matrix('', nr=1, nc=01, dimnames=list('', cnm));
  if (!(src %in% names(gs))) empty.matrix(paste("Gene set source not found:", src)) else {
    gs1<-gs[[src]];
    if (!(col %in% names(gs1))) empty.matrix(paste("Gene set collection not found: ", src, '/', col, sep='')) else {
      gs2<-gs1[[col]];
      if (!(spe %in% names(gs2))) empty.matrix(paste("Gene set species not found: ", src, '/', col, '/', spe, sep='')) else{
        t<-gs2[[spe]];
        data.frame(ID=AddHref(rownames(t), t$URL), Name=t$Name, Size=t$Size, stringsAsFactors=FALSE);
      }
    }
  }
}
