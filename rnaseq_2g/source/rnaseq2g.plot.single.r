rnaseq2g.plot.single <- function(res, gid, typ) {
  if (!is.null(res) & typ %in% c('1', '2', '3')) {
    if (!(gid %in% rownames(res[[2]][[1]]))) plot(0, type='n', axes=FALSE, xlab='', ylab='') else {
      grp <- res$input$groups;
      if (typ == '1') d <- res$input$original else if (typ == '2') d <- res$input$normalized$count else d <- res$input$normalized$logged;
      x1 <- d[gid, grp[[1]]];
      x2 <- d[gid, grp[[2]]]; 
      x1 <- x1[!is.na(x1)];
      x2 <- x2[!is.na(x2)];
      
      if (length(c(x1, x2)) <= 6) sp <- 1/2 else if (length(c(x1, x2)) <= 12) sp <- 1/3 else sp <- 1/4;
      
      mn <- min(c(x1, x2)); 
      
      
      par(mar=c(3,3,3,2)); 
      barplot(c(x1, x2), cex.names=0.75, col='#FFFF0033', ylim=c(1.05*min(0, min(x1, x2)), 1.05*max(c(x1, x2), na.rm=TRUE)), yaxs='i', 
              main=gid, cex.main=2, space=sp, border=c(rep('cyan', length(x1)), rep('purple', length(x2))));
      if (mean(x1)>mean(x2)) pos <- 'topright' else pos <- 'topleft';
      legend(pos, legend=names(grp), lty=1, lwd=2, col=c('cyan', 'purple'), cex=2, bty='n');
      box();
    }

  } else plot(0, type='n', axes=FALSE, xlab='', ylab=''); 
}