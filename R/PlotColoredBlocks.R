
# Default method to plot customized heatmap-like blue-red colored grid, with column and row name labels
PlotColoredBlock<-function(d, min=NA, max=NA, num.breaks=15, key='', groups=c()) {
  # d             Data matrix to plot; column and row names will be plotted
  # num.breaks    Number of color breakpoints
  # min, max      Minimal and maximal values to plot; round values lower or higher than the values
  # key           Plot key if not empty
  
  library(awsomics); 
  
  if (num.breaks<1) num.breaks<-32;
  col<-GetBluePickogramColors(num.breaks);
  
  if (is.na(min)) min<-min(d, na.rm=TRUE); 
  if (is.na(max)) max<-max(d, na.rm=TRUE); 
  
  if (min > min(d, na.rm=TRUE)) d[!is.na(d) & d<min]<-min;
  if (max < max(d, na.rm=TRUE)) d[!is.na(d) & d>max]<-max;
  breaks<-seq(min, max, length.out = length(col)+1); 
  
  # column/row names
  rnm<-rownames(d); 
  cnm<-colnames(d); 
  
  nr<-nrow(d);
  nc<-ncol(d);
  
  # Current device size in inch
  W<-par()$fin[1] - .2;
  H<-par()$fin[2] - .2; 
  str.h<-strheight('', unit='inches'); # default string height
  
  # Total block size
  block.w<-1.2*str.h*nc; 
  block.h<-1.2*str.h*nr;  
  
  ratio<-c(0.1, 10); # minimal and maximal label/block ratio
  
  # lable size
  wid.r<-max(strwidth(rnm, units='inches')); 
  wid.c<-max(strwidth(cnm, units='inches')); 
  
  str.r<-wid.r/max(1, str.h/(0.75*H/nr)); 
  str.c<-wid.c/max(1, str.h/(0.75*W/nc));
  
  str.r<-max(ratio[1]*block.w, min(ratio[2]*block.w, str.r)); 
  str.c<-max(ratio[1]*block.h, min(ratio[2]*block.h, str.c)); 
  
  if (0.9*block.w/ncol(d) < str.h) str.c<-str.c*(0.9*block.w/ncol(d)/str.h);
  
  # label + block size
  full.w<-block.w+str.r;
  full.h<-block.h+str.c;
  
  # ratio to device size
  ratio.w<-W/full.w;
  ratio.h<-H/full.h;
  
  # plot heatmap
  par(mar=c(0.1, 0.1, 0.1, 0.1)); 
  limit.w<-ceiling(full.w/(block.w/nc)); 
  limit.h<-ceiling(full.h/(block.h/nr)); 
  plot(0, type='n', xlim=c(0, limit.w), ylim=c(0, limit.h), xaxs='i', yaxs='i', axes=FALSE, xlab='', ylab='');
  image(0:nc, 0:nr, t(d[nr:1,]), col=col, breaks=breaks, add=TRUE);

  # column background label
  if (length(groups) > 0) {
    c0<-rep('#FFFFFF', length(groups));
    c0[seq(1, length(groups), 2)]<-"#DDDDDD"
    c1<-rep(c0, sapply(groups, length));
    names(c1)<-unlist(groups);
    c1<-c1[colnames(d)]; 
    rect(0:(ncol(d)-1), nrow(d), 1:ncol(d), limit.h, border=NA, col=c1);
  }
  
  # row background label
  c2<-rep('#FFFFFF', nrow(d));
  c2[seq(1, nrow(d), 2)]<-'#DDDDDD'; 
  rect(ncol(d), 0:(nrow(d)-1), limit.w, 1:nrow(d), border=NA, col=rev(c2)); 
  
  # plot row names
  space.w<-(1-nc/limit.w)*W;
  cex.w<-(space.w-0.2)/wid.r; 
  cex.w<-min(cex.w, 0.66*(H/limit.h)/str.h); 
  text(nc, (nr:1)-0.5, pos=4, label=rnm, cex=cex.w);
  
  # plot column names
  space.h<-(1-nr/limit.h)*H;
  cex.h<-(space.h-0.2)/wid.c; 
  cex.h<-min(cex.h, 0.66*(W/limit.w)/str.h); 
  text((1:nc)-0.5, (nr+limit.h)/2, srt=90, label=cnm, cex=cex.h);

  # Plot key
  if (!is.na(key) & key!='') {
    # key name
    key.w<-0.6*(space.w-0.2)/strwidth(key, units='inches');
    key.h<-0.2*space.h/str.h
    text((nc+limit.w)/2, nr, pos=3, label=key, col='purple', cex=min(key.w, key.h)); 
    
    key.w<-limit.w-nc; 
    key.h<-limit.h-nr;
    
    key.x<-c(nc + 0.15*key.w, nc + 0.85*key.w); 
    key.y<-c(nr + 0.4*key.h, nr + 0.6*key.h); 
    image(seq(key.x[1], key.x[2], length.out=num.breaks+1), key.y, matrix(breaks, nc=1), col=col, breaks=breaks, add=TRUE);
    
    # Round key labels
    abs<-max(abs(max), abs(min));
    if (abs>=100) rnd<-0 else if(abs>=10) rnd<-2 else if (abs>=1) rnd<-3 else if (abs==0) rnd<-1 else 
      rnd<-ceiling(-log10(abs))+2;
    min<-round(min, rnd); 
    max<-round(max, rnd); 
    
    text(nc+0.15*key.w, nr + 0.4*key.h, pos=1, label=min, cex=0.8*cex.h); 
    text(nc+0.85*key.w, nr + 0.4*key.h, pos=1, label=max, cex=0.8*cex.h); 
  }
  

  # Not plotting grid if too many rows or columns
  if (nr<=100) abline(h=0:nr, lwd=0.5, col='#333333') else if (nr<=250) abline(h=0:nr, lwd=0.25, col='#333333') else if (nr<=500) abline(h=0:nr, lwd=0.1, col='#333333')
  if (nc<=100) abline(v=0:nc, lwd=0.5, col='#333333') else if (nc<=250) abline(v=0:nc, lwd=0.25, col='#333333') else if (nc<=500) abline(v=0:nc, lwd=0.1, col='#333333')
  
  box();
}

# Return blue-pinkogram colors. 
GetBluePickogramColors<-function(n) {
  # n   Number of colors
  
  library(gplots);
  
  if (n<2) {
    warning('Number of colors less than 2, return NA');
    NA;
  } else {
    l<-floor(n/2); 
    
    if (l<=4) lo<-c('#A9A9FF', '#FF9DB0') else 
      if (l<=8) lo<-c('#D5D5FF', '#FFAADA') else
        if (l<=16) lo<-c('#E2DFFF', '#FFC2E6') else
          lo<-c('#EEE5FF', '#FFE5EE');
    
    col1<-colorpanel(l, '#0000FF', lo[1]);
    col2<-colorpanel(l, lo[2], '#FF0000');
    
    if (2*l==n) c(col1, col2) else c(col1, '#EEE5EE', col2); 
  }
}

# Helper function of PlotColoredBlock(), calculate preferred device size for color block heatmap
CalculateColoredBlockSize<-function(d, ratio=1, max.size=12) {
  # d         The matix to be plotted
  # ratio     Default height-to-width ratio of each block
  # max.size  Maximum width and height allowed for the whole plot, in inches
  
  sz<-0.2;
  
  nr<-nrow(d);
  nc<-ncol(d);
  
  block.w<-sz*nc;
  block.h<-ratio*sz*nr;
  
  full.w<-block.w + ratio*0.12*max(nchar(rownames(d))) + 0.4; 
  full.h<-block.h + 0.12*max(nchar(colnames(d))) + 0.4; 
  
  c(full.w, full.h)/max(1, max(c(full.w, full.h))/max.size); 
}
