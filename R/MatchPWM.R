# Match sequence to PWM

MatchPWM<-function(pwm, seq, score=80, score.up=100, score.low=65, min.hits=0, max.hits=Inf, verbose=TRUE) {
  # pwm		PWM matrix
  # seq		DNA sequence set to search for the PWM
  # score		Default matching score
  # min.hits	Minimum number of hits to expected
  # max.hits	Maximum number of hits to expected
  # score.low	Minimum match score allowed
  # score.up	Maximum match score allowed
  
  if (class(pwm)!='list') pwm<-list(pwm);	
  pwm<-lapply(pwm, function(pwm) {
    pwm<-apply(pwm, 2, function(x) x/sum(x));
    rev<-pwm[4:1, ncol(pwm):1];
    rownames(rev)<-rownames(pwm)<-c('A', 'C', 'G', 'T');
    colnames(rev)<-colnames(pwm)<-1:ncol(pwm);
    list(fw=pwm, rv=rev);
  })
  
  score<-max(0, min(score, 100));
  x<-c(min.hits, max.hits);
  min.hits<-max(0, min(x));
  max.hits<-max(min.hits, max(x));
  
  x<-c(score.up, score.low);
  score.up<-min(100, max(x));
  socre.low<-max(1, min(x));
  
  nm<-names(seq);
  seq<-as.character(seq);
  names(seq)<-1:length(seq);
  #seq<-seq[nchar(seq)>=ncol(pwm)];
  nc<-nchar(seq);
  sq<-DNAString(paste(seq, collapse=''));
  sm<-cumsum(nc);
  
  hits<-lapply(1:length(pwm), function(i) {
    if (verbose) print(i); 
    pwm<-pwm[[i]];
    
    # mask edges
    wd<-pmin(nc, ncol(pwm[[1]])); 
    mk<-append(Mask(sum(nc), end=sm, width=wd), Mask(sum(nc), start=c(1, sm[-length(sm)])+1, width=wd));
    masks(sq)<-mk;
    
    sc<-score;
    ht<-lapply(pwm, function(pwm) matchPWM(pwm, sq, min.score=paste(sc, '%', sep='')));
    n<-length(ht[[1]])+length(ht[[2]]);
    
    while(n>max.hits&sc<score.up) {
      sc<-min(score.up, sc+1);
      ht<-lapply(pwm, function(pwm) matchPWM(pwm, sq, min.score=paste(sc, '%', sep='')));
      n<-length(ht[[1]])+length(ht[[2]]);
    }
    
    while(n<min.hits&sc>score.low) {
      sc<-max(score.low, sc-1);
      ht<-lapply(pwm, function(pwm) matchPWM(pwm, sq, min.score=paste(sc, '%', sep='')));
      n<-length(ht[[1]])+length(ht[[2]]);
    }
    
    ht<-lapply(ht, function(ht) {
      s<-rep(1:length(seq), nc)[start(ht)];
      spt<-split(start(ht)+ncol(pwm[[1]])/2-.5, s);
      spt<-spt[as.character(1:length(seq))];
      spt<-lapply(1:length(seq), function(i) spt[[i]]-(sm[i]-nc[i]));
      names(spt)<-nm;
      spt;
    })
    
    list(score=sc, hits=ht);
  });
  
  hits;
}