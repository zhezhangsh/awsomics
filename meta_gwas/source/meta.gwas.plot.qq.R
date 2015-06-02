# Utility function that makes a QQ plot based on search results for the "Secondary analyses -> "QQ plot" tab
meta.gwas.plot.qq<-function(rslt, highlight=c()) {
  # rslt              Result set returned from the search.rslt<-reactive reactive expression
  # highligh          The IDs of analyses to be highlighted
      
  # re-formatting the result table
  # add an column (analysis name)
  if(identical(NA, rslt)) rslt<-NA else if (is.character(rslt$phred)) rslt<-data.frame('Message' = rslt$phred) else {
    rslt<-rslt$phred;
    p<-rslt$Phred;
    
    # color-coding selected analysis
    chosen<-highlight;
    col<-rep('#88888888', length(p))
    if (length(chosen) >0) {
      chosen.ana<-intersect(rownames(meta.analysis), sapply(strsplit(chosen, ' '), function(x) x[1]));
      
      if (length(chosen.ana) > 0) {
        cls<-rainbow(length(chosen.ana));
        for (i in 1:length(chosen.ana)) col[rslt$Analysis == chosen.ana[i]]<-sub('FF$', '88', cls[i]);
      }
    }
    PlotQqFromPhred(p, col=col);
  }
}