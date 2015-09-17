# Generate a Java DataTable from a matrix or data.frame in a "standard" way
CreateDatatable<-function(t, fn, rownames=TRUE) {
  # t         The table
  # fn        The HTML file to be written to
  # rownames  If TRUE, make the row names of the table as the first column named "ID"; not include row names in the output otherwise.
  
  if (!grepl('.html$', fn, ignore.case = TRUE)) fn<-paste(fn, '.html', sep='');
  
  for (i in 1:ncol(t)) if (class(t[, i]) == 'factor') t[, i]<-as.vector(t[, i]);
  
  if (rownames) t<-cbind(ID=rownames(t), t); 
  pg<-DT::datatable(dt, options=list("pageLength"=50, 'server'=object.size(t)>10^5), rownames=FALSE, filter='bottom', escape=FALSE);
  htmlwidgets::saveWidget(pg, fn, selfcontained = FALSE);
  
  fn;                  
}