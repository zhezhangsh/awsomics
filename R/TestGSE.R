# Gene set enrichment analysis by Fisher test
TestGSE<-function(gs, u, coll, size.min=10, size.max=500, p.cutoff=0.05) {
  # gs                    A vector of testing genes
  # u                     Universe set of genes
  # coll                  Gene set collections; list with unique names
  # size.min, size.max    Minimum and maximum number of genes in a gene set to be tested
  # p.cutoff              P value cutoff to be included in the results
  
  gs<-gs[gs %in% u];
  
  id<-rep(names(coll), sapply(coll, length));
  gn<-unlist(coll, use.names=FALSE);
  
  # Genes in the gene set
  inc0<-gn %in% gs;
  inc1<-gn %in% u;
  
  l0<-split(gn[inc0], id[inc0]);
  l1<-split(gn[inc1], id[inc1]);
  
  n<-N<-rep(0, length(coll));
  names(n)<-names(N)<-names(coll);
  
  n[names(l0)]<-sapply(l0, length);
  N[names(l1)]<-sapply(l1, length);

  n11<-n;
  n10<-N-n;
  n01<-length(gs[gs %in% u])-n;
  n00<-length(u[!(u %in% gs)])-(N-n);
  
  n<-cbind(n00, n01, n10, n11);
  n<-n[(n[,3]+n[,4])>=size.min & n[,3]+n[,4]<=size.max, , drop=FALSE];
  
  or<-(n[,4]/n[,3])/(n[,2]/n[,1]);
  
  # prepare to run HyperGeometric test
  logP<-apply(n, 1, function(n) phyper(n[4]-1, n[3]+n[4], n[1]+n[2], n[2]+n[4], lower.tail=FALSE, log.p=TRUE));
  p<-round(exp((ceiling(abs(logP))+logP)*log(10)), 2)*10^floor(logP);
  
  q<-p.adjust(p, method='BH');
  q<-round(exp((ceiling(abs(log10(q)))+log10(q))*log(10)), 2)*10^floor(log10(q));
  
  fwer<-p.adjust(p, method='bonferroni');
  fwer<-round(exp((ceiling(abs(log10(fwer)))+log10(fwer))*log(10)), 2)*10^floor(log10(fwer));
  
  stat<-cbind(N_Set=n[, 3]+n[,4], N_Within=n[,4], Percent=round(100*n[,4]/length(gs), 1), Odds_Ratio=round(or, 3), P_HyperGeo=p, FDR_BH=q, FWER=fwer);
  stat<-stat[order(stat[, 'P_HyperGeo']), , drop=FALSE];
  stat<-stat[stat[, 'P_HyperGeo']<=0.05, , drop=FALSE];
  
  list(stat=stat, list=l0[rownames(stat)], size=n);
}