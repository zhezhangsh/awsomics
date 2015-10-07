# format numeric columns in a matrix of data.frame
FormatNumeric<-function(tbl, digit=4) {
  for (i in 1:ncol(tbl)) {
    x<-tbl[, i];
    if (is.numeric(x)) {
      x<-as.vector(x); 
      if (!identical(round(x), x)) {
        if (max(abs(x), na.rm=TRUE)>1) x<-round(x, digit) else {
          y<-sign(x);
          a<-floor(log10(abs(x)));
          z<-round(abs(x)/10^a, 1);
          v<-y*z*10^a;
          v[x==0]<-0;
          x<-v;
        }
      }
    tbl[, i]<-x;
    }
  }
  tbl;
}