rnaseq2g.rank.pvalue <- function(res, nms, mth) {
  nms  <- nms[1:2]; 
  tbl1 <- res[[2]][[nms[1]]];
  tbl2 <- res[[2]][[nms[2]]];
  nms  <- mth[nms, 1]; 
  
  x1 <- tbl1[, 5]; x1[is.na(x1)] <- 0; rk1 <- rank(x1);
  x2 <- tbl2[, 5]; x2[is.na(x2)] <- 0; rk2 <- rank(x2);
  
  tbl1 <- cbind(Mean=rowMeans(tbl1[, 1:2]), tbl1[, 4:6]);
  tbl2 <- cbind(Mean=rowMeans(tbl2[, 1:2]), tbl2[, 4:6]);
  colnames(tbl1) <- paste(colnames(tbl1), nms[1], sep='_'); 
  colnames(tbl2) <- paste(colnames(tbl2), nms[2], sep='_'); 
  tbl <- FormatNumeric(cbind(tbl1, tbl2)[, c(3, 7, 1, 5, 2, 6, 4, 8)]);
  tbl <- cbind(rk1, rk2, rk2-rk1, tbl);
  colnames(tbl)[1:3] <- c(paste('Rank', nms, sep='_'), 'Rank_Diff');
  
  tbl <- tbl[order(rk1+rk2), , drop=FALSE]; 
  tbl; 
}

