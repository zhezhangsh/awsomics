# Mapping between genome alias
GetGenomeAlias<-function(alias, type=c('code', 'short', 'taxid', 'ucsc', 'name', 'official', 'package')) {
    # alias     A genome alias to be mapped
    # type      The type of alias to output; see examples below:
    #           code: hsa, mmu, ...
    #           short: hs, mmu, ce, rn, ...
    #           taxid: 9606, 10090, ...
    #           ucsc: hg19, mm10, ce10, ...
    #           name: human, mouse, worm, ...
    #           official: Homo sapiens, Mus musulus, ...
    #           package: org.Hs.eg.db, ...
  
  GENOMES<-rbind(
    c('hsa', '9606', 'taxid'),
    c('hsa', 'hg38', 'ucsc'),
    c('hsa', 'Homo sapiens', 'official'),
    c('hsa', 'hsa', 'code'),
    c('hsa', 'Human', 'name'),
    c('hsa', 'org.Hs.eg.db', 'package'),
    c('hsa', 'hs', 'short'),
    
    c('mmu', '10090', 'taxid'),
    c('mmu', 'mm10', 'ucsc'),
    c('mmu', 'mmu', 'code'),
    c('mmu', 'Mouse', 'name'),
    c('mmu', 'Mus musculus', 'official'),
    c('mmu', 'org.Mm.eg.db', 'package'),
    c('mmu', 'mm', 'short'),
    
    c('rno', '10116', 'taxid'),
    c('rno', 'org.Rn.eg.db', 'package'),
    c('rno', 'Rat', 'name'),
    c('rno', 'Rattus norvegicus', 'official'),
    c('rno', 'rn5', 'ucsc'),
    c('rno', 'rno', 'code'),
    c('rno', 'rn', 'short'),
    
    c('cel', '6239', 'taxid'),
    c('cel', 'Caenorhabditis elegans', 'official'),
    c('cel', 'ce6', 'ucsc'),
    c('cel', 'cel', 'code'),
    c('cel', 'org.Ce.eg.db', 'package'),
    c('cel', 'Worm', 'name'),
    c('cel', 'ce', 'short'),
    
    c('aga', '180454', 'taxid'),
    c('aga', 'aga', 'code'),
    c('aga', 'anoGam1', 'ucsc'),
    c('aga', 'Anopheles gambiae', 'official'),
    c('aga', 'Anopheles', 'name'),
    c('aga', 'org.Ag.eg.db', 'package'),
    c('bta', '9913', 'taxid'),
    c('bta', 'Bos taurus', 'official'),
    c('bta', 'bosTau7', 'ucsc'),
    c('bta', 'Bovine', 'name'),
    c('bta', 'bta', 'code'),
    c('bta', 'org.Bt.eg.db', 'package'),
    c('cfa', '9615', 'taxid'),
    c('cfa', 'canFam3', 'ucsc'),
    c('cfa', 'Canine', 'name'),
    c('cfa', 'Canis familiaris', 'official'),
    c('cfa', 'cfa', 'code'),
    c('cfa', 'org.Cf.eg.db', 'package'),
    c('dme', '7227', 'taxid'),
    c('dme', 'dm3', 'ucsc'),
    c('dme', 'dme', 'code'),
    c('dme', 'Drosophila melanogaster', 'official'),
    c('dme', 'Fly', 'name'),
    c('dme', 'org.Dm.eg.db', 'package'),
    c('dre', '7955', 'taxid'),
    c('dre', 'Danio rerio', 'official'),
    c('dre', 'danRer7', 'ucsc'),
    c('dre', 'dre', 'code'),
    c('dre', 'org.Dr.eg.db', 'package'),
    c('dre', 'Zebrafish', 'name'),
    c('eco', '511145', 'taxid'),
    c('eco', 'E coli strain K12', 'name'),
    c('eco', 'eco', 'code'),
    c('eco', 'Escherichia coli', 'official'),
    c('eco', 'org.EcK12.eg.db', 'package'),
    c('ecs', '386585', 'taxid'),
    c('ecs', 'E coli strain Sakai', 'name'),
    c('ecs', 'ecs', 'code'),
    c('ecs', 'Escherichia coli', 'official'),
    c('ecs', 'org.EcSakai.eg.db', 'package'),
    c('gga', '9031', 'taxid'),
    c('gga', 'Chicken', 'name'),
    c('gga', 'galGal4', 'ucsc'),
    c('gga', 'Gallus gallus', 'official'),
    c('gga', 'gga', 'code'),
    c('gga', 'org.Gg.eg.db', 'package'),
    c('mcc', '9544', 'taxid'),
    c('mcc', 'Macaca mulatta', 'official'),
    c('mcc', 'mcc', 'code'),
    c('mcc', 'org.Mmu.eg.db', 'package'),
    c('mcc', 'rheMac3', 'ucsc'),
    c('mcc', 'Rhesus', 'name'),
    c('ptr', '9598', 'taxid'),
    c('ptr', 'Chimp', 'name'),
    c('ptr', 'org.Pt.eg.db', 'package'),
    c('ptr', 'Pan troglodytes', 'official'),
    c('ptr', 'panTro4', 'ucsc'),
    c('ptr', 'ptr', 'code'),
    c('ssc', '9823', 'taxid'),
    c('ssc', 'org.Ss.eg.db', 'package'),
    c('ssc', 'Pig', 'name'),
    c('ssc', 'ssc', 'code'),
    c('ssc', 'Sus scrofa', 'official'),
    c('ssc', 'susScr3', 'ucsc'),
    c('xla', '8355', 'taxid'),
    c('xla', 'org.Xl.eg.db', 'package'),
    c('xla', 'Xenopus laevis', 'official'),
    c('xla', 'Xenopus', 'name'),
    c('xla', 'xla', 'code')
  );
  
  colnames(GENOMES)<-c('code', 'value', 'type');
  
  v<-tolower(GENOMES[, 'value']);
  c<-tolower(GENOMES[, 'code']);
  
  ind<-which(v==tolower(alias));
  
  if (length(ind)==0) NA else {    
    x<-GENOMES[c %in% c[ind], , drop=FALSE];
    ind<-which(type[1] == tolower(x[, 'type']));
    if (length(ind)==0) NA else unique(x[ind, 2]);
  }
}
