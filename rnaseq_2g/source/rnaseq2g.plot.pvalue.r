rnaseq2g.plot.pvalue <- function(res, nm1, nm2, typ, mth) {
  if (!is.null(res)) {
    if (nm1=='' | nm2=='') plot(0, type='n', axes=FALSE, xlab='', ylab='') else {
      p1 <- res[[2]][[nm1]][, 5]; 
      p2 <- res[[2]][[nm2]][, 5]; 
      p1[is.na(p1)] <- 1;
      p2[is.na(p2)] <- 1;
      
      par(mar=c(5,5,2,2)); 
      if (typ == '1') {
        x  <- sort(-log10(p1));
        y  <- sort(-log10(p2)); 
        lb <- paste('-Log10(p value)', c(mth[nm1, 1], mth[nm2, 1]), sep=', ');
        plot(x, y, pch=18, cex=1, col='#88888888', cex.lab=2, xlab=lb[1], ylab=lb[2]);
        abline(0, 1, col=4, lty=2, lwd=2);
      } else {
        rk1 <- rank(p1);
        rk2 <- rank(p2);
        lb  <- paste('P value ranking', c(mth[nm1, 1], mth[nm2, 1]), sep=', ');
        len <- length(rk1); 
        plot(rk1, rk2, log='xy', pch=18, cex=1, col='#88888888', cex.lab=2, xlab=lb[1], ylab=lb[2], xlim=c(1, len), ylim=c(1, len));
        abline(0, 1, col=4, lty=2, lwd=2);
      } 
      box();
    }
  } else plot(0, type='n', axes=FALSE, xlab='', ylab=''); 
}