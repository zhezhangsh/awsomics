plot.cov<-function(cov, selected=c(), plot.unit, plot.value, plot.type, plot.scale, plot.range) {
  plotEmpty<-function(msg) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main=msg); 
  
  if ((plot.range[2]-plot.range[1]) < 2) plotEmpty("Please select a larger range around TSS to make plot") else 
    if (length(selected)==0 & plot.unit==plot.cov.choice1[1]) plotEmpty("Select one gene to make plot") else 
      if (length(selected)==0 & plot.unit==plot.cov.choice1[2]) plotEmpty("Select one gene set to make plot") else {
      
        c<-sapply(cov, function(c) colMeans(c[selected, , drop=FALSE]));
        c<-c[seq(plot.range[1], plot.range[2], 50)/50+101, , drop=FALSE];
        
        if (plot.value == plot.cov.choice2[1]) d<-c else d<-matrix(c[, 2]-c[, 1], nc=1);
        if (plot.scale == plot.cov.choice4[2]) d<-exp(d*log(2));
        
        mx<-max(d, na.rm=TRUE);
        mn<-min(d, na.rm=TRUE);
        rg<-mx-mn;

        if (plot.type == plot.cov.choice3[1]) xlim=c(1, nrow(d)) else xlim=c(0, nrow(d))
        if (plot.value == plot.cov.choice2[2]) col='#00FF0066' else col<-c('#0000FF66', '#FF000066');
        
        par(mar=c(5, 5, 2, 2));        
        plot(0, type='n', yaxs='i', ylim=c(min(0, mn-0.05*rg), mx+0.05*rg), xaxs='i', xlim=xlim, xaxt='n', 
             xlab='Distance to TSS', ylab='Sequencing depth', cex.lab=2);
        abline(h=0, col='darkgrey');
        axis(1, at=1:nrow(d), label=seq(plot.range[1], plot.range[2], 50));
        if (plot.type == plot.cov.choice3[1]) { # Making line plot
          abline(v=(0-plot.range[1])/50+1);
          for (i in 1:ncol(d)) polygon(c(1, 1:nrow(d), nrow(d)), c(0, d[, i], 0), col=col[i], lwd=1, border='black');
        } else {
          barplot(t(d), be=TRUE, col=col, width=1/(ncol(d)+1), names.arg=rep('', nrow(d)), add=TRUE);
        }
        if (ncol(d) > 1) legend(1, mx, legend=c('Control', 'SLE'), bty='n', col=col, cex=2, pch=15);
      }
}