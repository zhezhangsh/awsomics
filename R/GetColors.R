
# Types of color generator
GetColorTypes<-function() {
  c('rainbow', 'heat', 'terrain', 'topo', 'just black', 'just grey', 'blue-red', 'green-red', 'grey-black', 'white-black', 'silver-gold', 'random');
}

# Types of generator of color panel
GetColorPanelTypes<-function() {
  c('blue-red',  'green-red', 'grey-black', 'white-black', 'silver-gold', 'heat', 'terrain', 'topo');
}

# Get colors according to generator type
GetColors<-function(n, type, rearrange=c('none', 'reverse', 'random')) {
  library(gplots);
  if(n<1) n<-1;
  
  t<-tolower(type)[1];

  if (t == 'rainbow') col<-rainbow(n) else 
    if (t == 'heat') col<-heat.colors(n) else 
      if (t == 'terrain') col<-terrain.colors(n) else 
        if (t == 'topo') col<-topo.colors(n) else 
          if (t == 'just black') col<-rep('#000000', n) else 
            if (t == 'just grey') col<-rep('#A9A9A9', n) else 
              if (t == 'blue-red') col<-colorpanel(n, 'blue', 'red') else 
                if (t == 'green-red') col<-colorpanel(n, 'green', 'red') else 
                  if (t == 'grey-black') col<-colorpanel(n, 'grey', 'black') else 
                    if (t == 'white-black') col<-colorpanel(n, 'white', 'black') else 
                      if (t == 'silver-gold') col<-colorpanel(n, '#C0C0C0', 'gold') else {
                        col<-col2rgb(sample(colors(), n, replace=TRUE));
                        col<-apply(col/255, 2, function(c) rgb(c[1], c[2], c[3]));
                      }
                        
  
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