# make barplot of a single gene

geex.plot.bar<-function(ds, rid, selected, scale, color, selected.group, show.mean=FALSE) {
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main=msg); 

  if (identical(NA, ds)) plotEmpty("No selected data set.") else if (length(selected.group)==0) plotEmpty("No selected group.") else 
    if (length(rid)==0 & selected) plotEmpty('') else if (length(rid)==0) plotEmpty("No selected gene.") else {

    if (scale=='unlogged') d<-exp(ds$data$logged*log(2)) else d<-ds$data[[scale]];

    rid<-CleanHtmlTags(rid);
    
    rid<-rid[rid %in% rownames(d)];
    grp<-ds$group;
    grp<-lapply(grp, function(grp) grp[grp %in% colnames(d)]);
    grp<-grp[sapply(grp, length)>0];
    
    if (length(rid)==0) plotEmpty('Selected gene not found in data set.') else {
      if (!is.null(selected.group)) selected.group<-selected.group[!is.na(selected.group) & selected.group!=''];
      if (length(selected.group) > 0) {
        col0<-GetColors(length(grp), color);
        names(col0)<-names(grp);
        grp<-grp[names(grp) %in% selected.group];
        grp<-grp[order(names(grp))];
        col1<-rep(col0[names(grp)], sapply(grp, length));
        if (length(grp) == 0) plotEmpty("") else {
          par(mai=c(1.5, 1, 0.2, 0.2));
          ylab<-paste('Expression level,', scale);
          col<-rep(col1, each=length(rid));
          smp<-do.call('c', grp);
          if (!show.mean) {
            cnm<-names(smp);
            x<-barplot(d[rid, smp], names.arg=rep('', length(smp)), col=col, be=TRUE, ylab=ylab, cex.lab=1.5);  
            if (length(rid)==1) x<-x[,1] else x<-colMeans(x);
          } else {
            m<-sapply(rid, function(rid) sapply(grp, function(s) mean(d[rid, s])));
            se<-sapply(rid, function(rid) sapply(grp, function(s) sd(d[rid, s])/sqrt(length(s))));
            #se[is.na(se)]<-0;
            u<-m+se;
            l<-m-se;
            col<-rep(col0[rownames(m)], each=length(rid));
            cnm<-rownames(m);
            x<-gplots::barplot2(t(m), names.arg=rep('', nrow(m)), col=col, plot.ci=TRUE, ci.l=t(l), ci.u=t(u), ci.lwd=2, ylab=ylab, cex.lab=1.5, beside=TRUE);
            if (ncol(m)==1) x<-x[1, ] else x<-colMeans(x);
          }
          wd<-max(strwidth(as.vector(cnm), units='inch'));
          cx<-min(1.2/wd, 1.5);
          text(x, par('usr')[3], label=cnm, cex=cx, srt=30, adj=c(1,1), xpd=TRUE);
        }
      } else plotEmpty('No selected group.');
    }  
  }  
}

# prepare the gene list table for barplot
geex.plot.bar.table<-function(cll, ds) {
  if (identical(NA, cll)) geex.empty.matrix('No loaded data collection') else 
    if (identical(NA, ds)) geex.empty.matrix('No loaded data set')  else ds$anno;  
}

geex.plot.bar.table2<-function(ds, rid) {
  t<-ds$anno;
  t<-t[rownames(t) %in% CleanHtmlTags(rid), , drop=FALSE];
  if (nrow(t) == 0) geex.empty.matrix('No selected gene') else t;
}