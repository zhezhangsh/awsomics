# Utility functions related to file path

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
