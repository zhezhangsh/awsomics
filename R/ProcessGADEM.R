### Process GADEM outputs

# deconstruct a GADEM object, to get a list of motifs with their p value, hits, etc.
RetrieveGadem<-function(gadem) {
  # gadem: the output object from GADEM function
  # return of list of motifs, for each motif retrieve its PWM, consensus, and hit list
  motifList<-gadem@motifList;
  
  retrieveMotif<-function(motif) {
    pwm<-motif@pwm;
    consensus<-motif@consensus;
    alignList<-motif@alignList; 
    hits<-data.frame(do.call(rbind, lapply(alignList, attributes)))[, 1:8];
    hits<-data.frame(lapply(hits, unlist));
    list(pwm, consensus, hits); 
  };
  
  motifs<-lapply(motifList, retrieveMotif); 
  motifs;
}


# Reformat the motif hits from GADEM outputs, get the exact position and sequence of each hit
ReformatMotifHits<-function(hits, genome) {
  # hits      Data.frame, from the output of GADEM run
  # genome    BSgenome, genome sequence object
  
  for (i in 1:nrow(hits)) {
    if (hits[['strand']][i]=='') {
      hits[['seq']][i]<-'';
      if (hits[['pos']][i]>(hits[['start']][i]/2+hits[['end']][i]/2)) hits[['strand']][i]<-'+'
      else hits[['strand']][i]<-'-'
    }}
  
  seq<-as.vector(hits[['seq']]);
  n<-sapply(seq, nchar);
  
  str<-rep(1, nrow(hits));
  str[hits[['strand']]=='-']<- -1;
  
  chr<-as.vector(hits[['chr']]);
  
  # the first and the last bases of hits
  first<-hits[['start']]+hits[['pos']]-1;
  last<-first+str*(max(n)-1);
  
  # middle position of the seeds
  mid0<-hits[['start']]/2+hits[['end']]/2;
  
  hits[['start']]<-pmin(first, last);
  hits[['end']]<-pmax(first, last);
  
  # middle position of the hits
  mid1<-hits[['start']]/2+hits[['end']]/2;
  
  # relative position of the middle of the hits to the middle of the seed sequences
  hits[['pos']]<-str*(mid1-mid0)
  
  # if the sequence of the hits is mmotissing or incomplete, query to genome to get the whole sequence
  for (i in 1:length(n)) {
    if(n[i]<max(n)) {
      s<-subseq(genome[[chr[i]]], hits[['start']][i], hits[['end']][i]);
      if (str[i]==-1) s<-reverseComplement(s);
      seq[i]<-as.character(s)
    }
  }
  hits[['seq']]<-seq;
  
  
  hits;
}


# Reverse-complement hits of motif from GADEM output
ReverseCompHits<-function(hits) {
  # hits      Data.frame, from the output of GADEM run
  
  seq<-DNAStringSet(as.vector(hits[['seq']]));
  hits[['seq']]<-as.character(reverseComplement(seq));
  
  str<-x<-as.vector(hits[['strand']])
  x[str=='+']<-'-';
  x[str=='-']<-'+';
  
  hits[['strand']]<-x;
  
  hits;
}

