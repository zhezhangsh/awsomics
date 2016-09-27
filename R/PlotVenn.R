# Draw a 2-set Venn diagram
PlotVenn<-function(s1, s2, names=c('Set1', 'Set2'),  universe=c(), fisher=TRUE, pdf=FALSE, plot.new=FALSE, lwd=4, cex=1, title='') {

if (length(universe)>=2) {
s1<-intersect(s1, universe);
s2<-intersect(s2, universe);
}

if (pdf) pdf(paste(names, collapse=' - '), w=8, h=6)
else if (plot.new) quartz(w=8, h=6);

if (length(s1)==0&length(s2)==0) {
cat('Both sets are empty.\n');
NA;
} else {
out<-list();
if (title[1]=='') par(mar=rep(1, 4)) else par(mar=c(1, 1, 3, 1));
plot(0, type='n', xlim=c(0, 8), ylim=c(0, 6), axes=F, bty='n', xaxs='i', yaxs='i', xlab='', ylab=''); # plot an empty space
bg<-'yellow1';
fg<-'lightblue';
symbols(3, 3, circles=1.75, bg=bg, fg=fg, lwd=lwd, add=TRUE, inches=FALSE);
symbols(5, 3, circles=1.75, bg=bg, fg=fg, lwd=lwd, add=TRUE, inches=FALSE);
symbols(3, 3, circles=1.75, fg=fg, lwd=lwd, add=TRUE, inches=FALSE);

rect(0, 0, 8, 6, lwd=lwd, border='black');
title(main=title, cex.main=2); 

s0<-intersect(s1, s2);
l0<-length(s0);
l1<-length(s1)-l0;
l2<-length(s2)-l0;
l<-length(universe)-l0-l1-l2;
out$counts<-c(l, l1, l2, l0);
col='maroon';
text(2, 3, labels=l1, col=col, cex=cex);
text(6, 3, labels=l2, col=col, cex=cex);
text(4, 3, labels=l0, col=col, cex=cex);

text(0.5, 3, labels=names[1], cex=cex, col='darkblue', srt=90);
text(7.5, 3, labels=names[2], cex=cex, col='darkblue', srt=270);

if (length(universe)>=2) {
text(4, 0, labels=l, pos=3, cex=cex, col=col);
if (fisher) {
out$fisher<-fisher.test(matrix(out$counts, nr=2));
p<-out$fisher[[1]];
ci<-out$fisher[[2]];
or<-out$fisher[[3]];

if (p<0.0001) p<-format(p, scientific=T, digit=2) else p<-round(p, 4);
p<-gsub(' ', '', p);
line<-paste(
paste('Odds ratio=', round(or, 2), sep=''), '; ',
paste('95% C.I.=(', round(ci[1], 2), ', ', round(ci[2], 2), ')', sep=''), '; ',
paste('p=', p), sep='');
text(4, 6, labels=line, cex=cex, pos=1);

} # end of fisher test
}
} # end of else

if (pdf) dev.off();

out;
}

PlotVenn3Way<-function(s1, s2=NA, s3=NA, names=rep('', 3), fill=c('#FF666666', '#66FF6666', '#6666FF66'), plot.new=TRUE, ...) {
  
  if(!identical(s2, NA) & !identical(s3, NA)) sets<-list(s1, s2, s3) else sets<-s1;
  
  n1<-sapply(sets, length);
  n2<-apply(cbind(c(1, 2, 1), c(2, 3, 3)), 1, function(ind) length(intersect(sets[[ind[1]]], sets[[ind[2]]])));
  n3<-length(intersect(sets[[1]], intersect(sets[[2]], sets[[3]])));
  
  if (plot.new) plot.new();
  VennDiagram::draw.triple.venn(n1[1], n1[2], n1[3], n2[1], n2[2], n2[3], n3, category=names, fill=fill, ...);
}
