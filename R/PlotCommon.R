# A set of utility functions to make common plots
################################################################################################################################
################################################################################################################################
PlotBarFromList<-function(d, ylab='', color='#88888888') {
  d<-lapply(d, function(d) d[!is.na(d)]);
  m<-sapply(d, mean);
  se<-sapply(d, function(d) sd(d)/sqrt(length(d)));
  up<-m+se;
  lo<-m-se;
  lims<-c(min(0, min(lo, na.rm=TRUE)), max(0, max(up, na.rm=TRUE)));
  rng<-lims[2]-lims[1];
  
  ch<-unlist(strsplit(names(d), '\n'), use.names=FALSE);
  wd<-1.1*max(strwidth(ch, units='inch'));  
  par(mai=c(wd, 1, 0.2, 0.2), omi=c(0.1, 0, 0, 0));
  
  bar<-gplots::barplot2(m, xaxt='n', ylab=ylab, cex.lab=1.25, plot.ci=TRUE, ci.u=up, ci.l=lo, space=1/3, ylim=c(lims[1]-0.1*rng, lims[2]+0.1*rng), col=color);
  if (lims[2]>0 & lims[1]<0) abline(h=0, lty=1);
  
  text(bar[,1], par('usr')[3], label=names(d), cex=min(3.6/wd, 1.5), srt=30, adj=c(1,1), xpd=TRUE);
  
  list(xaxis.pos=bar[,1], mean=m, se=se);
}

################################################################################################################################
################################################################################################################################
PlotBoxFromList<-function(d, ylab='', color='#88888888') {
  d<-lapply(d, function(d) d[!is.na(d)]);
  
  n<-sapply(d, length);
  if (max(n)>50000) ol<-FALSE else ol<-TRUE;
  
  ch<-unlist(strsplit(names(d), '\n'), use.names=FALSE);
  wd<-1.1*max(strwidth(ch, units='inch'));
  
  par(mai=c(wd, 1, 0.2, 0.2), omi=c(0.1, 0, 0, 0));
  
  mx<-sapply(d, max);
  mn<-sapply(d, min);
  box<-boxplot(d, xaxt='n', cex=0.75, pch=18, col=color, ylab=ylab, cex.lab=1.5, boxwex=.6, outline=ol);
  if (max(mx)>0 & min(mn)<0) abline(h=0, lty=1, col='#88888888');
  
  text(1:length(d), par('usr')[3], label=names(d), cex=min(3.6/wd, 1.5), srt=30, adj=c(1,1), xpd=TRUE);
  
  box;
}

################################################################################################################################
################################################################################################################################
PlotDensityFromList<-function(d, xlab='', color='#88888888', fill=FALSE, legend=FALSE) {
  d<-lapply(d, function(d) d[!is.na(d)]);

  if (length(color) != length(d)) color<-rep(color[1], length(d));
  
  dens<-lapply(d, function(d) if (length(d)==0) NA else if (length(d)==1) d else density(d));
  ymx<-max(sapply(dens, function(d) if (class(d) == 'density') max(d$y) else 0), na.rm=TRUE);
  xmx<-max(sapply(dens, function(d) if (class(d) == 'density') max(d$x) else d), na.rm=TRUE);
  xmn<-min(sapply(dens, function(d) if (class(d) == 'density') min(d$x) else d), na.rm=TRUE);
  rng<-xmx-xmn;

  dev.sz<-dev.size();
  mar.sz<-max(0, (dev.sz[2]-0.75*dev.sz[1])/2);
  par(mar=c(5,5,3,2), omi=c(mar.sz, 0, mar.sz, 0));
  plot(0, type='n', main='', xlab=xlab, ylab='Density', cex.lab=1.5, yaxs='i', ylim=c(0, 1.1*ymx), xlim=c(xmn, xmx));
 
  for (i in 1:length(dens)) {
    c<-color[i];
    if (class(dens[[i]]) == 'density') {
      if (fill) polygon(c(dens[[i]]$x[1], dens[[i]]$x), c(0, dens[[i]]$y), col=c, border="#888888", lwd=0.75) else lines(dens[[i]], col=c);
    } else segments(dens[[i]], 0, dens[[i]], 0.25*ymx, lwd=3, col=c);
  }
  
  abline(v=0, lwd=.75, lty=3, col='black');
  
  if (legend) legend(par()$usr[1], par()$usr[4], legend=names(d), bty='n', col=color, pch=15, cex=1.5);
  
  dens;
}

################################################################################################################################
################################################################################################################################
PlotCumulativeFromList<-function(d, xlab='', color='#88888888', legend=FALSE) {
  d<-lapply(d, function(d) d[!is.na(d)]);
  
  if (length(color) != length(d)) color<-rep(color[1], length(d));
  
  cdf<-lapply(d, function(d) if (length(d)==0) NA else ecdf(d));
  mn<-min(sapply(d, function(d) min(d, na.rm=TRUE)));
  mx<-max(sapply(d, function(d) max(d, na.rm=TRUE)));
  dev.sz<-dev.size();
  mar.sz<-max(0, (dev.sz[2]-0.75*dev.sz[1])/2);
  par(mar=c(5,5,3,2), omi=c(mar.sz, 0, mar.sz, 0));
  plot(0, type='n', main='', xlab=xlab, ylab='Cumulative density', cex.lab=1.5, yaxs='i', ylim=c(0, 1), xlim=c(mn, mx));
  
  for (i in 1:length(cdf)) {
    c<-color[i];
    if (class(cdf[[i]])[1] == 'ecdf') {
      plot(cdf[[i]], add=TRUE, lty=1, do.points=FALSE, verticals=TRUE, col=c, lwd=1.5)
    }
  }
  
  if (legend) legend(par()$usr[1], par()$usr[4], legend=names(d), bty='n', col=color, lty=1, lwd=2, cex=1.25);
  
  cdf;
}


################################################################################################################################
################################################################################################################################
# make a QQ plot using p values
PlotQqFromP<-function(p, max.ps=10000, col='#FF000066', pch=19, cex=1, main='Q-Q plot of p values', xlab='Expected -Log10(p value)', ylab='Observed -Log10(p value)') {
    # max.ps    Maximum number of non-significant p values to plot. If more p values were provided, randomly select this number of p values less than 0.01 to plot

    names(p)<-1:length(p);
    
    if (length(col)==length(p)) names(col)<-names(p);
    if (length(pch)==length(p)) names(pch)<-names(p);

    p<-p[!is.na(p)&p<=1&p>=0];
    
    x<--1*log10((1:length(p))/(1+length(p)));
    
    y<--1*log10(p);
    y1<-sort(y[y>2]);
    y2<-y[y<=2];
    length(length(y2))
    
    y<--1*log10(sort(p));
    
    # reduce the plotting time by randomly selecting non-signficant p values to plot
    sub<-which(y<2);
    if (length(sub) > max.ps) {
        rmd<-sample(sub, max.ps);
        ind<-c(rmd, which(y>=2));
        x<-x[ind];
        y<-y[ind];
    }
    
    if (length(col)==length(p)) col<-col[names(y)];
    if (length(pch)==length(p)) pch<-pch[names(y)]; 

    if(identical(cex, 1)) {
        cex<-max(0.25, 0.75^(log10(length(x))-3))
    }
    
    mx<-max(c(x, y));

    par(mar=c(5,5,2.5,2.5));

    plot(x, y, col=col, pch=pch, cex=cex, xlim=c(0, 1.05*mx), ylim=c(0, 1.05*mx), xaxs='i', yaxs='i', xlab=xlab, ylab=ylab, , main=main, cex.main=2, cex.lab=2);
    
    if (length(unique(col)>1)) {
        c<-sort(table(col));
        c0<-names(c)[length(c)];
        points(x[col!=c0], y[col!=c0], col=col[col!=c0], pch=pch[col!=c0]);
    }

    abline(0, 1, lty=1);
    abline(2, 1, lty=2, col='darkgrey');
}

################################################################################################################################
################################################################################################################################
# make a QQ plot using phred scores
PlotQqFromPhred<-function(phred, max.ps=10000, col='#FF000066', pch=19, cex=1, main='Q-Q plot of p values', xlab='Expected -Log10(p value)', ylab='Observed -Log10(p value)') {
    
    p<-exp((phred/-10)*log(10))
    
    PlotQqFromP(p, max.ps, col=col, pch=pch, cex=cex, main=main, xlab=xlab, ylab=ylab);
}