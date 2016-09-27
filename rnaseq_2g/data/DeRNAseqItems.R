x <- readRDS(paste(APP_HOME, 'data/DeRNAseqMethods.rds', sep='/'));

y <- sapply(1:8, function(i) {
  z <- x[[i]]; 
  n <- nchar(z); 
  N <- max(n);
  a <- N - n + 1;
  a <- sapply(a, function(a) paste(rep(' ', a), collapse=''));
  paste(z, a, sep='');
});

z <- apply(y, 1, function(y) paste(y, collapse=' | '));

saveRDS(z, paste(APP_HOME, 'data/DeRNAseqItems.rds', sep='/'));
