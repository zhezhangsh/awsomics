# Use this template to create a report that compares 2 pairwise comparisons of differential gene expression 
CreateReport<-function(yml, overwrite=FALSE) {
  # yml     The yaml file or an yaml list defines the inputs and parameters of the analysis
  
  library(awsomics);
  library(gplots);
  library(knitr);
  library(rmarkdown); 
  library(yaml);
  
  # Load YAML file
  if (class(yml) == 'character') {
    if (!file.exists(yml)) stop('YAML file: ', yml, ' not found\n'); 
    yml <- yaml::yaml.load_file(yml);  
  }
  
  # Output path
  path<-yml$output;
  if (!file.exists(path)) dir.create(path, recursive = TRUE);
  
  # Download or copy Rmarkdown template and YAML file to output path  
  path.tmpl<-yml$template;
  fn.tmpl<-rev(strsplit(path.tmpl, '/')[[1]])[1]; 
  fn.yaml<-paste(sub('Rmd$', '', fn.tmpl, ignore.case = TRUE), 'yml', sep='')
  if (file.exists(path.tmpl)) file.copy(path.tmpl, paste(path, fn.tmpl, sep='/')) else 
    if (RCurl::url.exists(path.tmpl)) writeLines(RCurl::getURL(path.tmpl, paste(path, fn.tmpl, sep='/'))) else 
      stop('Rmarkdown template file: ', path.tmpl, ' not exist.\n'); 
  writeLines(as.yaml(yml), paste(path, fn.yaml, sep='/'));
  
  wd<-getwd();
  setwd(path); 
  
  errors<-list(); 
  
  fn.md<-paste(sub('Rmd$', '', fn.tmpl, ignore.case = TRUE), 'md', sep='');
  if (file.exists(fn.md)) file.remove(fn.md); 
  
  # Run template, save error message
  errors$knit<-try(knit('DdReport.Rmd', fn.md)); 
  
  # Convert markdown file to html file
  errors$render<-try(rmarkdown::render(fn.md, output_format="html_document", output_file="index.html", output_dir=path, 
                                quiet=TRUE, envir=new.env()), silent=TRUE);

  # zip whole folder
  fld<-rev(strsplit(path, '/')[[1]])[1];
  setwd('..'); 
  fn.zip<-paste(fld, '.zip', sep=''); 
  zip(fn.zip, fld, flag='-r0X', zip='zip');
  file.rename(fn.zip, paste(fld, fn.zip, sep='/')); 
  
  setwd(wd);
  
  list(output=path, message=errors); 
}
