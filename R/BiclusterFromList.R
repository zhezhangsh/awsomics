# perform biclustering based on a list
BiclusterFromList<-function(lst) {
  lst<-lst[!duplicated(lst)]; 
  if (length(lst) <= 1) NA else {
    cnm<-unique(unlist(lst, use.names=FALSE));
    rnm<-names(lst);
    mp<-matrix(0, nrow = length(rnm), ncol = length(cnm), dimnames = list(rnm, cnm));
    for (i in 1:length(lst)) mp[rnm[i], lst[[i]]]<-1
    
    library(biclust);
    library(isa2);

    module<-isa(mp, direction=c('up', 'up'));
    
    rows<-module$rows;
    cols<-module$columns;
    n1<-apply(rows,2, function(x) length(x[x!=0]));
    n2<-apply(cols,2, function(x) length(x[x!=0]));
    rows<-rows[, n1>1&n2>1, drop=FALSE];
    cols<-cols[, n1>1&n2>1, drop=FALSE];
    if (ncol(rows)==0) bic<-NA else {
      modules<-lapply(1:ncol(rows), function(i) list(rows=which(rows[,i]!=0), cols=which(cols[,i]!=0)));
      names(modules)<-1:length(modules);
      modules<-lapply(modules, function(x) mp[x[[1]], x[[2]], drop=FALSE]);
      ncol<-sapply(modules, ncol);
      nrow<-sapply(modules, nrow);
      mean<-sapply(modules, mean);
      N<-length(mp);
      N1<-length(mp[mp==1]);
      n<-cbind(sapply(modules, length), sapply(modules, function(x) length(x[x==1])));
      p<-apply(n, 1, function(n) prop.test(c(n[2], N1-n[2]), c(n[1], N-n[1]))$p.value[[1]]); 
      rob<-robustness(list(Er=t(mp), Ec=mp), rows, cols);
      
      # write index file
      bic.smm<-data.frame(ID=paste('Bicluster_', 1:length(mean), sep=''), Num_Terms=nrow, Num_Genes=ncol, Robustness=rob, Pct_Present=mean, Enrichment=mean/mean(mp), P_Enrichment=p);
      bic<-list(summary=bic.smm, clusters=modules);
    }
    
    bic;
  }
  
}

