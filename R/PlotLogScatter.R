# Make a scatterplot in log-scale by treating zeros in a sidebar
PlotLogScatter<-function(x, y, base=10, xlab='', ylab='', main='', col='#22222266', col.side='#22228866', add=FALSE) {

x1<-log(x[x>0&y>0], base);
y1<-log(y[x>0&y>0], base);

x0<-log(x[y==0], base);
y0<-log(y[x==0], base);

mn<-min(min(x1), min(y1));
mx<-max(max(x1), max(y1));

mn<-min(0, mn);

if (main[1]=='') mar3<-1 else mar3<-3;
if (xlab[1]==''&ylab[1]=='') mar1<-3 else mar1<-5;

par(mar=c(mar1, mar1, mar3, mar3));

if (length(x0)>0 | length(y0)>0) sp<-(mx-mn)/10 else sp<-0;

if (!add) {
plot(x1, y1, xlim=c(mn-sp, mx), ylim=c(mn-sp, mx), axes=FALSE, xlab=xlab, ylab=ylab, main=main, cex.lab=1.8, cex.main=2, pch=18, col=col);

if (length(x0)>0) segments(x0, rep(mn-1.2*sp, length(x0)), x0, rep(mn-.8*sp, length(x0)),pch=124, col=col.side);
if (length(y0)>0) segments(rep(mn-1.2*sp, length(y0)), y0, rep(mn-.8*sp, length(y0)), y0,pch=124, col=col.side);

tck<-floor(mn):ceiling(mx);
lab<-exp(tck*log(base));

if (sp!=0) {
tck<-c(mn-sp, tck);
lab<-c(0, lab);
}

axis(1, at=tck, lab=lab);
axis(2, at=tck, lab=lab);
}
else {
points(x1, y1, cex.lab=1.8, pch=18, col=col);
}
box();
}
