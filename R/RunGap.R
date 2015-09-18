# Make multiple run of gap analysis to find the optimal number of k for kmean cluster

RunGap<-function(d, k.max=ncol(d)-1, n.run=20, n.bootstrap=100, n.node=n.run) {
  # d   The data matrix, assume each column is a sample
  
  d<-t(d);
  
  # Gap core function, return gap statistics

  if (n.node > 1) { # run in parallel
    runG<-function(d, k, n) cluster::clusGap(d, kmeans, k, n)[[1]][, 3];
    ds<-lapply(1:n.run, function(i) d); 
    cl<-snow::makeCluster(n.node, type='SOCK');
    gap<-snow::clusterApplyLB(cl, ds, runG, k=k.max, n=n.bootstrap);
    snow::stopCluster(cl);
  } else {
    gap<-lapply(1:n.run, function(i) cluster::clusGap(d, kmeans, k.max, n.bootstrap)[[1]][, 3]);
  }
  
  # all gap scores
  gap<-Reduce('+', gap)/n.run;
  rownames(gap)<-1:k.max;
  
  gap;
}