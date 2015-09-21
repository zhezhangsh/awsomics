# Make multiple run of gap analysis to find the optimal number of k for kmean cluster

RunGap<-function(x, func=c('kmeans', 'pam', 'hclust'), k.max=ncol(x)-1, make.plot=FALSE, 
                 n.run=3, n.bootstrap=100, n.node=1, n.se=c(1, 1.5, 2, 3)) {
  # x             The data matrix, assume each column is a sample
  # func          The name of clustering function ('kmeans', 'pam', or 'hclust') if a string; or an actual function returns a list with an $cluster element
  # k.max         Maximal number of clusters
  # make.plot     Make plots for ou\
  # n.run         Number of runs performed in parallel
  # n.bootstrap   Number of bootstrap re-sampling during each run
  # n.node        Number of CPU nodes to perform multiple runs in parallel
  # n.se          Values of SE cutoffs for the maxSE function
  
  d<-t(x);
  
  # clustering function
  if (identical(tolower(func[1]), 'kmeans')) fn<-kmeans else 
    if (identical(tolower(func[1]), 'pam')) fn<-function(x,k) list(cluster = pam(x,k, cluster.only=TRUE)) else 
      if (identical(tolower(func[1]), 'hclust')) fn<-function(x,k) list(cluster = cutree(hclust(dist(scale(x))), k)) else 
        fn<-func;

  # Gap core function, return gap statistics
  if (n.node > 1) { # run in parallel
    library(snow);
    runG<-function(d, k, n) cluster::clusGap(d, kmeans, k, n)[[1]];
    ds<-lapply(1:n.run, function(i) d); 
    cl<-makeCluster(n.node, type='SOCK');
    gap<-clusterApplyLB(cl, ds, runG, k=k.max, n=n.bootstrap);
    stopCluster(cl);
  } else {
    gap<-lapply(1:n.run, function(i) cluster::clusGap(d, kmeans, k.max, n.bootstrap)[[1]]);
  }
  
  # SE of gap scores between runs
  if (n.run == 1) SE.gap<-NA else {
    gaps<-sapply(gap, function(x) x[, 3]);
    SE.gap<-apply(gaps, 1, sd)/sqrt(ncol(gaps));
  }
  
  # average runs
  gap<-Reduce('+', gap)/n.run;
  rownames(gap)<-1:k.max;
  
  # Pick a k based on a few different rules (see maxSE function manual) 
  mets <- eval(formals(cluster::maxSE)$method);
  k<-sapply(n.se, function(se) sapply(mets, function(mt) cluster::maxSE(gap[, 3], gap[, 4], method=mt, SE.factor=se)));
  colnames(k)<-paste(n.se, 'xSE', sep='');
  rownames(k)<-mets;
  
  if (make.plot) {
    par(mfrow=c(1, 2), mar=c(6, 5, 2, 2)); 
    
    
    plot(1:nrow(gap), gap[, 1], type='b', pch='O', cex=0.5, ylim=c(min(gap[,1:2]), max(gap[, 1:2])), xlab="Number of clusters (k)", ylab='Obs vs. Exp Log(Wk)', cex.lab=1.5);
    lines(1:nrow(gap), gap[, 2], type='b', pch='E', cex=0.5);
    
    gplots::barplot2(gap[, 3], plot.ci=TRUE, ci.l=gap[,3]-gap[,4], ci.u=gap[,3]+gap[,4], space=0, ylim=c(min(gap[,3]+gap[,4])-0.05, max(gap[, 3]+gap[,4])+0.05), cex.lab=1.5, cex.sub=1.5, ylab='gap statistic', sub='Number of clusters k');
    ind1<-as.vector(which(diff(sign(diff(gap[,3])))==-2)+1);
    ind2<-which.max(gap[,3]);
    ind1<-setdiff(ind1, ind2);
    ind1<-ind1[gap[ind1, 3]>0];
    if (length(ind1)>0) 
      text((1:nrow(gap))[ind1]-0.5, gap[ind1,3]+gap[ind1,4], pos=2*as.integer(gap[ind1,3]+gap[ind1,4]>0)+1, label='^', cex=1.25);
    if (gap[ind2, 3]>0 & ind2>1) {
      text((1:nrow(gap))[ind2]-0.5, gap[ind2,3]+gap[ind2,4], pos=2*as.integer(gap[ind2,3]+gap[ind2,4]>0)+1, label='+', cex=1.5);
      legend(-.25, max(gap[,3]+gap[,4])+0.05, bty='n', pch=c('^', '+'), legend=c('Local maximum', 'Global maximum'), cex=1);
    }
  }
  
  # outputs
  out<-list(Input=c(N.sample=ncol(x), FUN=alist(fn)[[1]], K.max=k.max, B=n.bootstrap, N.run=n.run), Statistics=gap, Selection=k, SE.gap=SE.gap)
  
  out;
}