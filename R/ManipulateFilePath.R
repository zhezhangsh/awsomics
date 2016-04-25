# Utility functions related to file path

# Truncate a path to remove a given prefix
TruncatePathPrefix<-function(path, prefix) {
  # pth    The original path, usually an absolute path to a file or folder
  # prefix  The prefix of path to be removed
  
  pth<-sub(prefix, '', path); 
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


