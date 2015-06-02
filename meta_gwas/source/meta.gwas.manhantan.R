# Make 2D or 3D manhantan plots using Phred scores
meta.gwas.manhantan3d<-function(rslt) {
  # re-formatting the result table
  # add an column (analysis name)
  if(identical(NA, rslt)) rslt<-NA else if (is.character(rslt$phred)) rslt<-data.frame('Message' = rslt$phred) else {
    rslt<-rslt$phred;
    n.snp<-length(unique(rslt$SNP));
    par(mar=c(5,5,2,2));
    
    if (n.snp == 1) {
      plot(1:nrow(rslt), rslt$Phred, xlab='Analysis', ylab='Phred', cex.lab=2, main='Manhattan plot', 
           cex.main=2, col='#88888888', pch=19, axes=FALSE);
    } else {
      library(scatterplot3d);
      
      ######## temp line
      rslt<-rslt[rslt$Phred>=10,]
      
      snps<-rslt[!duplicated(rslt$SNP),];
      ps<-snps$Position;
      names(ps)<-snps$SNP
      ps<-sort(ps);
      aa<-sort(unique(rslt$Analysis));
      anas<-1:length(aa);
      names(anas)<-aa;
      
      xx<-rslt$Phred
      names(xx)<-rslt$Analysis;
      x<-anas[names(xx)];
      for (i in 1:length(anas)) x[names(x)==names(anas)[i]]<-x[names(x)==names(anas)[i]]+i-1;
      
      yy<-rslt$Phred;
      names(yy)<-rslt$SNP;
      y<-ps[names(yy)];
      for (i in 1:length(ps)) y[names(y)==names(ps)[i]]<-y[names(y)==names(ps)[i]]+i-1;
      
      cls<-rainbow(length(ps));
      names(cls)<-names(ps);
      col<-cls[rslt$SNP];
      
      scatterplot3d(anas[rslt$Analysis], ps[rslt$SNP], rslt$Phred, zlab='Phred', xlab='Analysis', ylab='SNP', cex.lab=2, main='Manhattan plot', 
                    cex.main=2, color=col, pch=19, axis=TRUE, x.ticklabs=names(anas), y.ticklabs=names(ps));
    }
    
  }
}
