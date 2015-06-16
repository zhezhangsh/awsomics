# make barplot of a single gene

geex.plot.bar<-function(ds, rid, scale, selected.group='', show.mean=FALSE) {
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg); 

  if (identical(NA, ds)) plotEmpty('No selected data set.') else {
    if (scale=='unlogged') d<-exp(ds$data$logged*log(2)) else d<-ds$data[[scale]];
    if (length(rid)==0) plotEmpty('No selected genes') else {
      rid<-rid[rid %in% rownames(d)];
      grp<-ds$group;
      grp<-lapply(grp, function(grp) grp[grp %in% colnames(d)]);
      grp<-grp[sapply(grp, length)>0];
      
      if (length(rid)==0) plotEmpty('Gene not found.') else {
        if (!is.null(selected.group)) selected.group<-selected.group[!is.na(selected.group) & selected.group!=''];
        if (length(selected.group) > 0) {
          col0<-rainbow(length(grp));
          names(col0)<-names(grp);
          grp<-grp[names(grp) %in% selected.group];
          grp<-grp[order(names(grp))];
          col1<-rep(col0[names(grp)], sapply(grp, length));
          if (length(grp) == 0) plotEmpty('Group not found') else {
            par(mar=c(8, 5, 2, 2));
            ylab<-paste('Expression level,', scale);
            col<-rep(col1, each=length(rid));
            smp<-do.call('c', grp);
            if (!show.mean) {
              barplot(d[rid, smp], las=3, col=col, be=TRUE, ylab=ylab, cex.lab=1.5);          
            } else {
              m<-sapply(rid, function(rid) sapply(grp, function(s) mean(d[rid, s])));
              se<-sapply(rid, function(rid) sapply(grp, function(s) sd(d[rid, s])/sqrt(length(s))));
              se[is.na(se)]<-0;
              u<-m+se;
              l<-m-se;
              col<-rep(col0[rownames(m)], each=length(rid));
              gplots::barplot2(t(m), las=3, col=col, plot.ci=TRUE, ci.l=t(l), ci.u=t(u), ci.lwd=2, ylab=ylab, cex.lab=1.5, beside=TRUE);
            }
          }
        } else plotEmpty('No selected group.');
      }
    }  
  } 
}