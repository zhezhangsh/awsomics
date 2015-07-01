
# Type of color generator
GetColorTypes<-function() {
  c('rainbow', 'heat', 'terrain', 'topo', 'random');
}

# Get colors according to generator type
GetColors<-function(n, type, rearrange=c('none', 'reverse', 'random')) {
  if(n<1) n<-1;
  
  t<-tolower(type)[1];
  
  if (t == 'rainbow') col<-rainbow(n) else 
    if (t == 'heat') col<-heat.colors(n) else 
      if (t == 'terrain') col<-terrain.colors(n) else 
        if (t == 'topo') col<-topo.colors(n) else 
          col<-sample(colors(), n, replace=TRUE);
  
  if (tolower(rearrange[1]) == 'reverse') col<-rev(col) else 
    if (tolower(rearrange[1]) == 'random') col<-sample(col, n);
  
  col;
}

# Get colors transiting from blue to yellow to red
GetPrimeColors<-function(n, adj='00', alpha='FF') {
  if (n < 1) n<-1;
  if (n %% 2 == 0) N<-n+1  else N<-n;
  
  col<-gplots::colorpanel(N, 'blue', 'yellow', 'red');
  
  if (adj != '00') col<-gsub('00', adj, col);
  
  if (alpha != 'FF') col<-paste(col, alpha, sep='');
  
  if (N>n) if (n == 4) col<-col[-2] else col<-col[-((N+1)/2)];
  
  col;
}