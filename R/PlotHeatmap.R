# Call the gplots::heatmap.2 function in a 'simple' way

# blue-pinkogram colors. 
GetBluePickogramColors<-function() {
  c("#0000FF", "#0707FF", "#0E0EFF", "#1515FF", "#1C1CFF", "#2424FF", "#2B2BFF", "#3232FF", "#3939FF", "#4040FF", "#4848FF", "#5050FF", "#5858FF", "#6060FF", "#6868FF", "#7070FF", "#7878FF", "#8080FF", "#8888FF", "#9191FF", "#9999FF", "#A2A2FF", "#AAAAFF", "#B3B3FF", "#BBBBFF", "#C4C4FF", "#CCCCFF", "#D5D5FF", "#DAD0FB", "#DECBF7", "#E3C7F3", "#E8C2EF", "#ECBDEA", "#F1B8E6", "#F6B4E2", "#FAAFDE", "#FFAADA", "#FFA4D0", "#FF9DC6", "#FF97BC", "#FF90B2", "#FF8AA8", "#FF839E", "#FF7D94", "#FF768A", "#FF7080", "#FF6B79", "#FF6572", "#FF606B", "#FF5B64", "#FF555C", "#FF5055", "#FF4B4E", "#FF4547", "#FF4040", "#FF3939", "#FF3232", "#FF2B2B", "#FF2424", "#FF1C1C", "#FF1515", "#FF0E0E", "#FF0707", "#FF0000");
}


PlotHeatmap.2<-function(d, col='default', key=FALSE, trace='none', sepwidth=c(0,0), ...) {
  
  if (identical(col, 'default')) {
    col<-gplots::colorpanel(128, 'blue', 'white', 'red');
  }
  
  gplots::heatmap.2(d, col=col, key=key, trace=trace, sepwidth=sepwidth, ...);
  
}

# More advanced heatmap plotting 
PlotHeatmap<-function(d, reorder=TRUE, size.max=10, size.block=0.2, col.max='#FF0000', col.min='#00FF00', col.na='#EEEEEE', col.num=128, fn='', plot.new=TRUE) {
  
  if (is.null(colnames(d))) msize.right<-0; # if row or column names are not available, remove margin
  if (is.null(rownames(d))) msize.up<-0;
  
  # reorder matrix to emphasize pattern
  if(reorder & nrow(d)>1 & ncol(d)>1) {
    r1<-cor(d, use='pair');
    r1[is.na(r1)]<-1;
    hc1<-hclust(as.dist(1-r1));
    
    r2<-cor(t(d), use='pair');
    r2[is.na(r2)]<-1;
    hc2<-hclust(as.dist(1-r2));
    
    d<-d[hc2$order, hc1$order];
  }
  
  
  # width of plot, cannot be more than given max size
  w<-ncol(d)*size.block;
  if (w>size.max) {
    w<-size.max;
    block.width<-w/ncol(d);
  } else block.width<-size.block;
  
  msize.up<-max(nchar(colnames(d)))*block.width/4;
  
  # height of plot, cannot be more than given max size
  h<-nrow(d)*size.block;
  if(h>size.max) {
    h<-size.max;
    block.height<-h/nrow(d);
  } else block.height<-size.block;
  
  msize.right<-max(nchar(rownames(d)))*block.height/3;
  
  if (plot.new) {
    if (fn=='' | is.na(fn[1])) {
      quartz(w=0.1+w+msize.right, h=0.1+h+msize.up);
    } else pdf (paste(fn[1], '.pdf', sep=''), w=0.1+w+msize.right, h=0.1+h+msize.up);
  } else {
    w0<-dev.size()[1];
    wl<-max(nchar(rownames(d)));
    msize.right<-max(w0/5, w0*wl/(wl+4*ncol(d)));
    block.width<-(w0-msize.right)/ncol(d);
    
    h0<-dev.size()[2];
    hl<-max(nchar(colnames(d)));
    msize.up<-max(h0/5, h0*hl/(hl+3*nrow(d)));
    block.height<-(h0-msize.up)/nrow(d);
  }
  
  library(gplots);
  col<-colorpanel(col.num+1, col.min, col.max);
  
  par(mai=c(0.1,0.1,msize.up,msize.right), omi=c(0,0,0,0));
  plot(0, type='n', ylab='', xlab='', yaxs='i', xaxs='i', yaxt='n', xaxt='n', ylim=c(0, nrow(d)), xlim=c(0, ncol(d)));
  
  # plot blocks in color
  mx<-max(d);
  mn<-min(d);
  c<-round(col.num*(d-mn)/(mx-mn))+1;
  c<-apply(c, 2, function(c) col[c]);
  c[is.na(c)]<-col.na;
  c<-as.vector(c);
  if (mx==mn) c[1:length(c)]<-col[length(col)];
  x1<-rep(1:ncol(d), each=nrow(d));
  y0<-nrow(d)-rep(1:nrow(d), ncol(d));
  rect(x1-1, y0, x1, y0+1, col=c, border=NA);
  abline(v=0:ncol(d), h=0:nrow(d), col='#88888888', lwd=0.25);
  box();
  
  # Plot row names
  rnm<-rownames(d);
  ch<-0.5*block.height/max(strheight(rnm, units='inch'));
  if (fn=='' | is.na(fn[1])) cw<-0.9*msize.right/max(strwidth(rnm, units='inch')) else cw<-0.85*msize.right/max(strwidth(rnm, units='inch'))
  text(ncol(d), nrow(d):1-0.5, label=rnm, cex=min(ch, cw), xpd=TRUE, pos=4);
  
  # Plot column names
  cnm<-colnames(d);
  ch<-0.4*block.width/max(strheight(cnm, units='inch'));
  cw<-1.75*msize.up/max(strwidth(cnm, units='inch'));
  text(1:ncol(d)-1, nrow(d)+0.1*msize.up*nrow(d)/h, label=cnm, cex=min(ch, cw), srt=30, pos=4, xpd=TRUE);
  
  if (plot.new & fn!='' & !is.na(fn[1])) dev.off();
  
  list(width=0.1+w+msize.right, height=0.1+0.667*h+msize.up);
}  
  
