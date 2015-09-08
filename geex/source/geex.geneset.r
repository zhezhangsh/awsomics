# prepare the gene set table for group comparison
geex.geneset.table<-function(gs, src, col, spe) {
  src<-src[1];
  col<-col[1];
  spe<-spe[1];

  if (!(src %in% names(gs))) geex.empty.matrix(paste("Gene set source not found:", src)) else {
    gs1<-gs[[src]];
    if (!(col %in% names(gs1))) geex.empty.matrix(paste("Gene set collection not found: ", src, '/', col, sep='')) else {
      gs2<-gs1[[col]];
      if (!(spe %in% names(gs2))) geex.empty.matrix(paste("Gene set species not found: ", src, '/', col, '/', spe, sep='')) else{
        t<-gs2[[spe]];
        data.frame(ID=AddHref(rownames(t), t$URL), Name=t$Name, Size=t$Size, stringsAsFactors=FALSE);
      }
    }
  }
}
