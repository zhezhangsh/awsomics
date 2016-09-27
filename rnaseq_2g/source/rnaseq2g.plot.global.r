rnaseq2g.plot.global <- function(res, tbl, typ) {
  if (!is.null(res)) {
    tbl <- res[[2]][[tbl]]; 
    grp <- names(res$input$groups); 
    if (!is.null(tbl) & typ=='4') PlotMA(rowMeans(log2(tbl[, 1:2]+1), na.rm=TRUE), tbl[, 4]) else 
      if (!is.null(tbl) & typ=='3') PlotVolcano(tbl[, 4], tbl[, 5]) else 
        if (!is.null(tbl) & typ=='1') PlotPValue(tbl[, 5]) else 
          if (!is.null(tbl) & typ=='2') PlotFDR(tbl[, 6])
  } else plot(0, type='n', axes=FALSE, xlab='', ylab=''); 
}