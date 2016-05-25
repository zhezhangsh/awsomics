### Helper function related to linear models

# Calculate number of sigma for each observation, using linear model built with all other observations
CalcLmSigma <- function(x, y, plot=FALSE, xlab=NA, ylab=NA) {
  # x, y        Vectors to fit linear model lm(y~x). Must have the same length
  # plot        Whether to make the regression plot
  # xlab, ylab  Axis labels for the plot
  
  c <- cbind(x, y); 
  
  sigma <- sapply(1:nrow(c), function(i) {
    x<-c[-i, ];
    mdl<-lm(x[, 2]~x[, 1]); 
    coe<-as.vector(coefficients(mdl)); 
    prd<-c[i, 1]*coe[2]+coe[1]; 
    sig<-summary(mdl)$sigma;
    (prd-c[i, 2])/sig; 
  }); 
  
  if (plot) {
    par(mar=c(5,5,2,2));
    col <- rep('#88888888', length(x)); 
    col[abs(sigma) >= 1.5] <- '#FF888888'; 
    col[abs(sigma) >= 3.0] <- '#FF000088'; 
    plot(x, y, col=col, pch=19, cex=2, xlab='Total input (million)', ylab='Uniquely mapped (million)', cex.lab=1.5); 
    points(x, y, col='black', pch=19, cex=.2);
    abline(lm(y~x), col='blue', lty=2);
    if (cor(c[, 1], c[, 2], use='pair') > 0) loc <- 'topleft' else loc <- 'topright';
    legend(loc, legend=c('Num_Sigma < 1.5', 'Num_Sigma >= 1.5', 'Num_Sigma >=3.0'), cex=1, bty='n', pch=18, 
           col=c('#88888888', '#FF888888', '#FF000088')); 
    abline(lm(y~x), col='blue', lty=2);
  }
  
  names(sigma) <- names(y); 
  sigma;
}