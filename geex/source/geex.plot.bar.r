# make barplot of a single gene

geex.plot.bar<-function(ds, rid, scale, color, selected.group, show.mean=FALSE) { 
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main=msg); 

  if (identical(NA, ds)) plotEmpty("No selected data set.") else if (length(selected.group)==0) plotEmpty("No selected group.") else 
    if (length(rid)==0) plotEmpty('') else if (length(rid)==0) plotEmpty("No selected gene.") else 
      if (show.mean & length(selected.group)<2) plotEmpty("Need more than 1 group to plot group means") else {

    if (scale=='unlogged') d<-exp(ds$data$logged*log(2)) else d<-ds$data[[scale]];

    rid<-CleanHtmlTags(rid);
    
    rid<-rid[rid %in% rownames(d)];
    grp<-ds$group;
    grp<-lapply(grp, function(grp) grp[grp %in% colnames(d)]);
    grp<-grp[sapply(grp, length)>0];
    
    if (length(rid)==0) plotEmpty('') else {
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
          smp<-unlist(grp, use.names=FALSE);
          names(smp)<-paste(rep(names(grp),sapply(grp, length)), smp, sep=': ');
          if (!show.mean) {
            #cnm<-names(smp);
            cnm<-paste(ds$sample[smp, 'Group'], ds$sample[smp, 'Name'], sep=': ');
            x<-barplot(d[rid, smp], names.arg=rep('', length(smp)), col=col, be=TRUE, ylab=ylab, cex.lab=1.5);  
            if (length(rid)==1) x<-x[,1] else x<-colMeans(x);
          } else {
            m<-sapply(rid, function(rid) sapply(grp, function(s) mean(d[rid, s])));
            se<-sapply(rid, function(rid) sapply(grp, function(s) sd(d[rid, s])/sqrt(length(s))));
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

geex.select.gene<-function(cll, ds.id, grp.id, gn.id) {
  out<-list();
  out$message<-'';
  
  ds.id<-ds.id[1];
  
  if (identical(NA, cll)) {
    out$message<-msg.nocollection;
    out$table<-geex.empty.matrix(out$message);
  } else {
    if (identical(NA, ds.id) | ds.id=='') {
      out$message<-msg.nodataset;
      out$table<-geex.empty.matrix(out$message);
    } else {
      ds<-geex.load.dataset(cll, ds.id);
      grp.id<-grp.id[grp.id %in% names(ds$group)];
      if (length(grp.id) == 0) {
        out$message<-msg.nogroup;
        out$table<-geex.empty.matrix(out$message);
      } else {
        gn.id<-CleanHtmlTags(gn.id);
        gn.id<-gn.id[gn.id %in% rownames(ds$data$logged)];
        if (length(gn.id)==0) {
          out$message<-msg.nogene;
          out$table<-geex.empty.matrix(out$message);
        } else { ###################################################### finally to do the real thing
          d<-ds$data$logged[gn.id, , drop=FALSE];
          grp2smp<-lapply(ds$group, function(s) s[s %in% colnames(d)]);
          smp.id<-unlist(grp2smp, use.names=FALSE);
          grp<-rep(names(grp2smp), sapply(grp2smp, length));
          d<-d[, smp.id, drop=FALSE];
          grp.id<-grp.id[grp.id %in% grp];
          if (length(grp.id) == 0) {
            out$message<-msg.nogroup;
            out$table<-geex.empty.matrix(out$message);
          } else {
            grp2smp<-grp2smp[grp.id];
            d<-d[, unlist(grp2smp, use.names=FALSE), drop=FALSE];
            if (length(grp2smp)>1) {
              ms<-sapply(grp2smp, function(s) rowMeans(d[, s, drop=FALSE], na.rm=TRUE)); 
              if (class(ms) == 'numeric') ms<-matrix(ms, nr=1); 
              rownames(ms)<-rownames(d);
              colnames(ms)<-paste('Mean', cll$mapping$longname2id[names(grp2smp)], sep='_'); 
              f<-as.factor(rep(names(grp2smp), sapply(grp2smp, length)));
              p<-apply(d, 1, function(d) {
                p<-summary(aov(d~f))[[1]][1, 5];
                if (is.null(p)) NA else p;
              })
              stat<-data.frame(round(ms, 4), p_ANOVA=as.numeric(format.pval(p, 2)), FDR=round(p.adjust(p, method='BH'), 3), stringsAsFactors=FALSE);
            } else {
              m<-rowMeans(d, na.rm=TRUE);
              p<-rep(NA, nrow(d));
              stat<-data.frame(round(m), as.numeric(format.pval(p, 2)), stringsAsFactors = FALSE)
            }
            stat<-data.frame(ds$anno[rownames(stat), 1:3, drop=FALSE], stat, stringsAsFactors=FALSE);
            ds<-cll$gex_combined$logged[rownames(d), rownames(cll$metadata$Dataset), drop=FALSE];
            ds<-colnames(ds)[!is.na(colSums(ds))];
            ds<-as.vector(cll$mapping$id2longname[ds]);
            out$message<-geex.html.gene.name(gn.id, FALSE, cll);
            out$data<-d;
            out$table<-stat;
            out$dataset<-ds;
          }
        }
      }
    }
  }
  
  out;
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