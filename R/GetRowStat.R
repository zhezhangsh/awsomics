# Get descriptive statistics of each row in a data matrix
GetRowStatTypes<-function() c('Mean', 'Median', 'Minimum', 'Maximum', 'Variance', 'Skewness');

GetRowStat<-function(d, stats=GetRowStatTypes()) { 
  types<-tolower(GetRowStatTypes());
  
  stats<-stats[tolower(stats) %in% types]; 
  if (length(stats) == 0) NA else { 
    out<-sapply(stats, function(s) {
      s<-tolower(s);
      if (s == types[1]) apply(d, 1, function(d) mean(d, na.rm=TRUE)) else 
        if (s == types[2]) apply(d, 1, function(d) median(d, na.rm=TRUE)) else 
          if (s == types[3]) apply(d, 1, function(d) min(d, na.rm=TRUE)) else 
            if (s == types[4]) apply(d, 1, function(d) max(d, na.rm=TRUE)) else 
              if (s == types[5]) apply(d, 1, function(d) var(d, na.rm=TRUE)) else 
                if (s == types[6]) apply(d, 1, function(d) e1071::skewness(d, na.rm=TRUE));
    });
    colnames(out)<-stats;
    out;
  }
}