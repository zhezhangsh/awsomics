# Utility functions related to file path

# Convert absolute to relative path, using a reference path as the current path
ConvertPath2Relative <- function(path, curr) {
  
  x <- strsplit(path, '/')[[1]]; 
  y <- strsplit(curr, '/')[[1]];
  x <- x[x!=''];
  y <- y[y!=''];
  l <- min(length(x), length(y)); 
  
  if (l==0) ind <- integer() else ind <- which(x[1:l]==y[1:l]);
  if (length(ind) == 0) ind <- 0 else ind <- max(ind); 
  
  x <- paste(x[(ind+1):length(x)], collapse='/'); 
  y <- paste(rep('../', length(y)-ind), collapse=''); 
  z <- paste(y, x, sep='');
  
  z; 
}

# Truncate a path to remove a given prefix
TruncatePathPrefix<-function(path, prefix) {
  # path    The original path, usually an absolute path to a file or folder
  # prefix  The prefix of path to be removed
  
  path<-sub(prefix, '', path); 
  path<-sapply(path, function(path) {
    while(grepl('^/', path)) path<-sub('^/', '', path); 
    path;
  });
  
  path; 
}

# Remove all path, return just the file name
TrimPath<-function(path) {
  sapply(strsplit(path, '/'), function(x) x[length(x)]); 
}

# Remove file extension
RemoveExtension<-function(path) {
  path<-sapply(path, function(f) {
    is.dir<-file.info(f)$isdir; 
    if (!is.dir | is.na(is.dir)) {
      x<-strsplit(f, '/')[[1]]; 
      mx<-max(gregexpr('\\.', x[length(x)])[[1]]);
      if (mx <= 1) mx<-nchar(x[length(x)]) else mx<-mx-1; 
      x[length(x)]<-substr(x[length(x)], 1, mx); 
      paste(x, collapse='/'); 
    } else f;
  }); 
  path;
}
