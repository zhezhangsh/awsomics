ConvertPdf2Png<-function(fn.pdf, resize=80, density=150) {
  # fn.pdf  PDF file name
  # resize  0-100; output image size
  # density image pixel density; value more than 600 will generate very large file
  
  if (grepl('.pdf$', fn.pdf, ignore.case = TRUE)) fn.png<-sub('.pdf$', '.png', fn.pdf, ignore.case = TRUE) else fn.png<-paste(fn.pdf, '.png', sep='');
  
  if (!grepl('%$', resize)) resize<-paste(resize, '%', sep='');
  cmmd<-paste('convert -density', density, '-resize', resize, fn.pdf, fn.png);
  
  x<-system(cmmd);

  fn.png;
}