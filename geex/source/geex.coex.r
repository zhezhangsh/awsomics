geex.coex.plot<-function(data, type=3, highlight=c()) {
  
  # make an empty plot
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', main=msg);   
  
  if (!data$plot) plotEmpty(data$status) else {
    if (type<1 | type>3) type<-3;
    
    par(mar=c(5,5,2,2));
    
    d<-data$data[[type]]; 
    c<-data$color[[type]];
    cex<-max(min(4, 4/log10(ncol(d))), 0.25);
    plot(d[1,], d[2, ], pch=19, cex=cex, cex.main=1.5, cex.lab=1.5, col=c, xlab=data$x, ylab=data$y);  
    
    mp<-data$mapping[[type]];    
    highlight<-unlist(mp[highlight[highlight %in% names(mp)]], use.names=FALSE);
    highlight<-highlight[highlight %in% colnames(d)];
    if (length(highlight)>0) points(d[1, highlight], d[2, highlight], cex=1.5*cex, pch=13);
  }
}

geex.coex.data<-function(cll, id1, id2, scale, color) {
  out<-list();
  out$plot<-FALSE;
  
  if (identical(NA, cll)) out$status<-msg.nocollection else 
    if (length(id1)==0) out$status<-"First gene not selected" else 
      if (length(id2)==0) out$status<-"Second gene not selected" else 
        if (!(id1 %in% rownames(cll$gex_combined$logged))) out$status<-paste('Gene', id1, 'not found in data collection') else 
          if (!(id2 %in% rownames(cll$gex_combined$logged))) out$status<-paste('Gene', id2, 'not found in data collection') else {
            
            # get data of both genes in required scale
            if (tolower(scale)=='unlogged') gex<-exp(cll$gex_combined$logged[c(id1, id2), ]*log(2)) else 
              if(tolower(scale)=='percentile') gex<-cll$gex_combined$percentile[c(id1, id2), ] else 
                gex<-cll$gex_combined$logged[c(id1, id2), ];
            
            nm1<-as.vector(cll$gene[id1, 'Symbol']);
            nm2<-as.vector(cll$gene[id2, 'Symbol']);
            out$status<-paste(nm1, 'vs.', nm2);
            
            out$x<-paste(id1, nm1, sep=': ');
            out$y<-paste(id2, nm2, sep=': ');
            
            g.ds<-gex[, colnames(gex) %in% rownames(cll$metadata$Dataset), drop=FALSE];
            g.grp<-gex[, colnames(gex) %in% rownames(cll$metadata$Group), drop=FALSE];
            g.smp<-gex[, colnames(gex) %in% rownames(cll$metadata$Sample), drop=FALSE];
            
            g.ds<-g.ds[, !is.na(colSums(g.ds)), drop=FALSE];
            g.grp<-g.grp[, !is.na(colSums(g.grp)), drop=FALSE];
            g.smp<-g.smp[, !is.na(colSums(g.smp)), drop=FALSE];
            
            if (ncol(g.smp) == 0) out$status<-'No data set includes both genes' else {
              smp<-cll$metadata$Sample[colnames(g.smp), , drop=FALSE];
              ds2smp<-split(rownames(smp), smp$Dataset);
              grp<-cll$metadata$Group[colnames(g.grp), , drop=FALSE];
              ds2grp<-split(rownames(grp), grp$Dataset);
              ds2ds<-names(ds2grp);
              names(ds2ds)<-ds2ds;
              out$mapping<-list(dataset2dataset=ds2ds, dataset2group=ds2grp, dataset2sample=ds2smp);
              
              # data set stats
              n<-sapply(ds2smp, length);
              r<-round(sapply(ds2smp, function(s) cor(g.smp[1, s], g.smp[2, s], use='pairwise.complete.obs')), 4);
              p<-format.pval(sapply(ds2smp, function(s) if (length(s)<3) 1 else cor.test(g.smp[1, s], g.smp[2, s])$p.value[1]), 4);
              stat<-data.frame(row.names=names(ds2smp), ID=names(ds2smp), N=n, Corr=r, p_Corr=p, Name=cll$metadata$Dataset[names(ds2smp), 'Name']);
              stat<-stat[1:(nrow(stat)+1), , drop=FALSE];
              rownames(stat)[nrow(stat)]<-stat[nrow(stat), 1]<-cll$id;
              stat[nrow(stat), 2:3]<-c(nrow(stat)-1, cor(g.ds[1, ], g.ds[2, ]));
              if (nrow(stat)<4) stat[nrow(stat), 4]<-1 else stat[nrow(stat), 4]<-format.pval(cor.test(g.ds[1, ], g.ds[2, ])$p.value[1], 4);
              stat[nrow(stat), 5]<-'Across data sets';
              out$stat<-stat;
              
              g.ds<-g.ds[, names(ds2grp), drop=FALSE];
              g.grp<-g.grp[, unlist(ds2grp, use.names=FALSE), drop=FALSE];
              g.smp<-g.smp[, unlist(ds2smp, use.names=FALSE), drop=FALSE];
              
              out$data<-list(dataset=g.ds, group=g.grp, sample=g.smp);
              
              colors.ds<-GetColors(ncol(g.ds), color);
              names(colors.ds)<-colnames(g.ds);
              colors.grp<-colors.ds[cll$metadata$Group[colnames(g.grp), 'Dataset']];
              names(colors.grp)<-colnames(g.grp);
              colors.smp<-colors.ds[cll$metadata$Sample[colnames(g.smp), 'Dataset']];
              names(colors.smp)<-colnames(g.smp);
              
              out$color<-list(dataset=colors.ds, group=colors.grp, sample=colors.smp);
              
              out$plot<-TRUE;
            } 
          }
  out;
}