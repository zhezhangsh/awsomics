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
  #p<-round(exp((ceiling(abs(logP))+logP)*log(10)), 2)*10^floor(logP);
  
  p<-exp(logP);
  p<-round(p, ceiling(abs(log10(p)))+2);
  
  q<-p.adjust(p, method='BH');
  q<-round(q, ceiling(abs(log10(q)))+2);
  
  fwer<-p.adjust(p, method='bonferroni');
  fwer<-round(fwer, ceiling(abs(log10(fwer)))+2);
  
  stat<-cbind(N_Set=n[, 3]+n[,4], N_Within=n[,4], Percent=round(100*n[,4]/length(gs), 1), Odds_Ratio=round(or, 3), P_HyperGeo=p, FDR_BH=q, FWER=fwer);
  stat<-stat[order(stat[, 'P_HyperGeo']), , drop=FALSE];
  stat<-stat[stat[, 'P_HyperGeo']<=0.05, , drop=FALSE];
  
  list(stat=stat, list=l0[rownames(stat)], size=n);
}


# Summarize, format, and write out results of gene set enrichment analysis from the TestGSE function
WrapGSE<-function(stat, anno, output=getwd(), split.collection=FALSE) {
  # stat              Statistic table, the first element of the TestGSE outputs
  # anno              Gene set annotation
  # output            Path to output files
  # split.collection  Whether to further split gene sets by their collection, default is FALSE
  
  if (!file.exists(output)) dir.create(output, recursive = TRUE);
  
  library(awsomics);
  
  stat<-stat[rownames(stat) %in% rownames(anno), , drop=FALSE]; 
  stat<-awsomics::FormatNumeric(stat); 
  anno<-anno[rownames(stat), ]; 
  anno$Collection<-sub('/', '-', anno$Collection); 
  
  # prepare full table
  tbl1<-tbl0<-cbind(anno, stat); 
  tbl1$Name<-AddHref(tbl1$Name, tbl1$URL);
  tbl1<-tbl1[, !(colnames(tbl1) %in% c('URL', 'N_Set', 'FWER'))];
  
  src<-sort(unique(anno[[1]])); 
  
  # split by source
  tbls<-lapply(src, function(x) tbl1[tbl1$Source==x, -1, drop=FALSE]); 
  names(tbls)<-src;
  
  ##########
  if (split.collection) {
    cll<-lapply(split(anno$Collection, anno$Source), unique)[src];
    cll<-lapply(cll, sort); 
    tbls<-lapply(src, function(s) lapply(cll[[s]], function(c) tbls[[s]][tbls[[s]]$Collection==c, -1, drop=FALSE])); 
    names(tbls)<-src;
    for (i in 1:length(tbls)) names(tbls[[i]])<-cll[[i]];
    
    # write datatables
    fn<-lapply(names(tbls), function(nm) { 
      pth<-paste(output, nm, sep='/'); 
      if (!file.exists(pth)) dir.create(pth, recursive = TRUE); 
      t<-tbls[[nm]]; 
      f<-sapply(names(t), function(nm2) {
        f1<-paste(pth, paste(nm2, '.html', sep=''), sep='/');
        f2<-CreateDatatable(t[[nm2]], f1, caption = paste(nm, nm2, sep=': ')); 
        f1;
      }); 
      names(f)<-names(t); 
      f;
    }); 
    names(fn)<-src;
  } else {
    # write datatables
    fn<-sapply(names(tbls), function(nm) {
      fn<-paste(output, paste(nm, '.html', sep=''), sep='/'); 
      CreateDatatable(tbls[[nm]], fn, caption = nm); 
    }); 
    names(fn)<-src;
  }
  
  saveRDS(tbl0, file=paste(output, 'GSE_All_Sig.rds', sep='/'));
  
  list(all=tbl0, formatted=tbls, file=fn); 
}