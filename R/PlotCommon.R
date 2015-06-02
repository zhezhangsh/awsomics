# A set of utility functions to make common plots

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