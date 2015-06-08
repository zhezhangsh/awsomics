print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");

  ###################################################################################################
  ######################################## Get Search Results #######################################
  # this is the driver function of this App
  search.rslt<-reactive({ # create a reactive expression to be shared across UI elements 
    if (input$search.value == '') NA else {
      # Search options
      search.opt<-input$search.options;
      search.key<-original.search.key<-input$search.value;
      mask.p<-input$mask.p.values; 
      selected.genome<-input$select.genome;
      extend.length<-1000*max(0, input$extend.ends.value, na.rm=TRUE); 
      extend.side<-input$extend.end;
      restrict.type<-input$search.restrict;
      restrict.key<-input$search.restrict.to;
      range.phred<-input$max.phred.range;
      exclude.table<-input$exclude.table;
           
      #############################################################################################                
      # Tables, studies, or analysis to be excluded
      tab.out<-c();
      ana.out<-c();
      
      # specified restriction on search
      if (restrict.key != '') {
        restrict.key<-tolower(restrict.key);
        if (restrict.type == search.restrict.options[1]) { # restrict to keyword
          flg<-apply(desc.fields, 2, function(fld) agrepl(restrict.key, fld, ignore.case=TRUE));
          ana.out<-rownames(meta.analysis)[rowSums(flg)==0];
        } else if (restrict.type == search.restrict.options[2]) { # restrict to analysis
          ana.out<-setdiff(rownames(meta.analysis), restrict.key);
        } else if (restrict.type == search.restrict.options[3]) { # restrict to study
          ana.out<-rownames(meta.analysis)[!(meta.analysis[['Study_ID']] %in% restrict.key)];
        } else if (restrict.type == search.restrict.options[4]) { # restrict to database table
          tab.out<-setdiff(names(tbls), restrict.key);
          if (length(tab.out)>0) ana.out<-intersect(rownames(meta.analysis), unlist(lapply(tab.out, colnames), use.names=FALSE));
        }
      };
      
      # range of max phred
      rng.phred<-as.numeric(strsplit(range.phred, '[-: ]')[[1]]);
      min.max<-max(rng.phred[1], -Inf, na.rm=TRUE);
      max.max<-min(rng.phred[2], Inf, na.rm=TRUE);
      max.phred<-as.numeric(as.vector(meta.analysis$Max_Phred));
      ana.out<-c(ana.out, rownames(meta.analysis)[max.phred<min.max | max.phred>max.max]);
      
      # table explicitly excluded
      if (length(exclude.table) > 0) {
        tab.out<-c(tab.out, sapply(strsplit(as.character(exclude.table), ' '), function(x) x[1]));
        x<-unique(unlist(lapply(tab.out, colnames), use.names=FALSE));
        ana.out<-intersect(rownames(meta.analysis), c(ana.out, unlist(lapply(tab.out, function(x) colnames(tbls[[x]])), use.names=FALSE)));
      }
      
      #########################################################################################
      ##### Search a single SNP, don't need information about its position
      rslt<-'';
      chr<-''; 
      stt<-0; 
      end<-0;
      
      if (search.opt==search.options[1] & extend.length==0) {
        rslt<-SearchGwasDbById(search.key, tbls, genome=selected.genome, tables.out=unique(tab.out), analyses.out=unique(ana.out)); 
      } else {
      ##### All other searches will need to get the position first                
        # Get genomic range of searching, produce a message if range not available
        if (search.opt == search.options[1]) {        ###### search by SNP ID with end extension
          snp.pos<-SearchVariantPositionDbById(search.key, tbl.pos[[selected.genome]], as.GRanges=FALSE);
          #snp.pos<-SnpPosFromGwasDb(search.key, tbl.pos, as.GRanges = FALSE, genome=selected.genome);
          if (nrow(snp.pos) == 0) rslt<-'SNP ID not found in dbSNP' else { # ID not found in dbSNP database
            chr<-as.vector(snp.pos[[2]])[1];
            stt<-snp.pos[1,3] - extend.length; 
            end<-snp.pos[1,3] + extend.length; 
            if(is.na(stt[1]) | is.na(end[1])) rslt<-'SNP location no available';
          }
        } else if (search.opt == search.options[2]) { ###### search by genomic location
          pos<-strsplit(gsub(' ', '', search.key), '[:-]')[[1]]; 
          chr<-pos[1]; 
          stt<-as.numeric(pos[2]) - extend.length;
          end<-max(stt, as.numeric(pos[3]), na.rm=TRUE) + extend.length; 
          if(is.na(stt[1]) | is.na(end[1])) rslt<-'Format of genomic region not recognized';
        } else { ###### search by gene ID or symbol
          if (search.opt == search.options[4]) search.key<-Symbol2Entrez(search.key, sym2id, syn2id); 
          pos<-id2gene[[search.key]][['Location']][[selected.genome]];
          chr<-pos$Chromosome; 
          stt<-pos$From - extend.length; 
          end<-pos$To + extend.length; 
          str<-pos$Strand;
          if (extend.side == search.extend.options[4]) if (str == '+') end<-end-extend.length else stt<-stt+extend.length;
          if (extend.side == search.extend.options[5]) if (str == '-') end<-end-extend.length else stt<-stt+extend.length;
          if(is.na(stt[1]) | is.na(end[1])) rslt<-'Gene location no available';
        }
        
        # re-adjust to extending side
        if (extend.side == search.extend.options[2]) end<-end - extend.length else if (extend.side == search.extend.options[3]) stt<-stt + extend.length; 
        
        #################################################################################################################################
        rslt<-SearchGwasDbByLoci(chr, stt, end, tbls, tables.out=unique(tab.out), analyses.out=unique(ana.out), genome=selected.genome);#
        #################################################################################################################################
} 
      #############################################################################################
      
      # reset widgets
      updateSliderInput(session, 'enrich.minimum.phred', value=20);
      updateSliderInput(session, 'enrich.minimum.count', value=5);
      
      # reset QQ plot highlight
      p20<-rslt[rslt$Phred>=20, , drop=FALSE];
      p20<-p20[order(p20$Phred, decreasing = TRUE), , drop=FALSE];
      p20<-p20[!duplicated(p20$Analysis), , drop=FALSE];
      if (nrow(p20) == 0) chc<-list() else chc<-paste(p20$Analysis, ' (', p20$Phred, ')', sep='');
      updateCheckboxGroupInput(session, 'qq.highlight', choices=if (length(chc)>0) chc[1:min(10, length(chc))] else list());

      # analyses and studies the query was searching in
      ana.in<-rownames(meta.analysis)[!(rownames(meta.analysis) %in% ana.out)];
      std.in<-unique(as.vector(meta.analysis[ana.in, 'Study_ID']));

      rslt$Study<-as.vector(meta.analysis[rslt$Analysis, 'Study_ID']);
      rslt$PubMed<-as.vector(meta.analysis[rslt$Analysis, 'PubMed_ID']);
      rslt$PubMed[rslt$PubMed == '-1']<-'';
      
      rslt<-list(analysis=ana.in, study=std.in, phred=rslt)
      
      #######################################################################
      # Save search results
      fn<-paste(Sys.time(), '_chr', chr, '-', stt, '-', end, sep='');
      fn<-paste(fn, '_', search.opt, '=', original.search.key, '.rdata', sep='');
      fn<-gsub('[ :]', '-', fn);
      rslt.path<-paste(APP_HOME, 'results', sep='/'); 
      if (!file.exists(rslt.path)) dir.create(rslt.path);
      save(rslt, file=paste(rslt.path, fn, sep='/'));
      #######################################################################
      
      rslt;
    }     
  });  

  ###################################################################################################
  ######################################## "Search" tab #############################################
  # table title
  output$search.table.title<-renderUI({
    if (input$search.value == '' | input$check.refresh) list(h1('Welcome to Awsomics: GWAS Hub'), h6("beta version")) else 
      h2(paste('Query: ', input$search.options, ' = ', input$search.value, sep=''));
  });

  # Search table message
  output$search.table.message<-renderUI({
    msg<-'';
    if (input$check.refresh) msg<-"Look up your favorite SNP, gene, or genomic region:" else {
      rslt<-search.rslt();
      if (identical(rslt, NA)) 
        msg<-"Look up your favorite SNP, gene, or genomic region:" else if (is.character(rslt$phred)) 
          msg<-rslt$phred else if (nrow(rslt$phred) == 0)
            msg<-"No GWAS rsult was found.";
    }
    if (identical(msg, NA) | msg[[1]]=='') list(hr()) else list(hr(), h4(msg), hr());
  });

  # table body
  output$search.table <- DT::renderDataTable({
      if (!input$check.refresh) {
      rslt<-search.rslt();
      
      # re-formatting the result table
      # add an column (analysis name)
      if(identical(NA, rslt)) rslt<-NA else if (is.character(rslt$phred) | nrow(rslt$phred)==0) rslt<-NA else {
        rslt<-rslt$phred;
        rslt<-cbind(rslt, Analysis_Name=as.vector(meta.analysis[as.vector(rslt$Analysis), 'Name']));
        rslt$Rank<-getPhredRank(as.vector(rslt[, 'Analysis']), rslt[, 'Phred'], phred.top, meta.analysis);
         
        rslt<-rslt[order(rslt[, 'Phred'], decreasing=TRUE), ];
        url<-UrlUcscPosition(rslt[['Chromosome']], rslt[['Position']], genome=input$select.genome);
        rslt[['Position']]<-AddHref(paste(rslt[['Chromosome']], rslt[['Position']], sep=':'), url);
        rslt[['SNP']]<-AddHref(rslt[['SNP']], UrlDbsnpId(rslt[['SNP']]));
        rslt[['Analysis']]<-AddHref(rslt[['Analysis']], meta.analysis[as.vector(rslt[['Analysis']]), 'URL']);
        rslt[['Study']]<-AddHref(rslt[['Study']], meta.study[as.vector(rslt[['Study']]), 'URL']);
        rslt[['PubMed']]<-AddHref(rslt[['PubMed']], meta.pubmed[as.vector(rslt[['PubMed']]), 'URL']);
        
        rslt<-rslt[, rslt.cnm];

        if (length(input$mask.p.values) > 0) rslt[['Phred']]<-'[Masked]';
        
        has.result<-TRUE;
      }
    } else {
      # clear options
      updateCheckboxInput(session, inputId='check.refresh', value=FALSE);
      updateTextInput(session, inputId='search.value', value='');
      updateCheckboxInput(session, inputId='mask.p.values', value=FALSE);
      updateSelectInput(session, inputId='select.genome', choices=genome.versions);      
      updateSelectInput(session, inputId='extend.end', choices=search.extend.options);
      updateNumericInput(session, inputId='extend.ends.value', value=0);
      updateSelectInput(session, inputId='search.restrict', choices=search.restrict.options);
      updateTextInput(session, inputId='search.restrict.to',  value='');
      updateTextInput(session, inputId='max.phred.range',  value='30-300');      
      updateCheckboxGroupInput(session, inputId='exclude.table', selected=list());
      rslt<-NA;
    }
    if (!(class(rslt) %in% c('matrix', 'data.frame'))) {
      rslt<-data.frame('C1' = 'No search results');
      colnames(rslt)<-'';
    }

    rslt;
  }, options=list(
    autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 25), rownames=FALSE, escape=FALSE);
  ###################################################################################################
  
  ###################################################################################################
  ######################################## "Factoid" tab ############################################
  output$factoid.text <- renderUI({
    factoid.key<-input$factoid.key; # factoid.key: search key of factoid inputed by user
    factoid.opt<-input$factoid.options; # factoid.opt:type of entity selected by user to show
    title<-paste(sapply(strsplit(factoid.opt, ' '), function(o) o[1]), factoid.key, sep=': '); # page title
    
    ###############################################    
    # If search for a gene symbol or synonyms, convert to gene ID
    if (factoid.opt == factoid.types[5]) {
      factoid.opt<-factoid.types[4]; # change type to "Gene ID"
      factoid.key<-Symbol2Entrez(factoid.key, sym2id, syn2id);
    };
    ###############################################
    
    ###############################################
    if (factoid.key == '') { # no search key
      ###########
      HtmlKeyValue(names(factoid.summary), format(as.vector(factoid.summary), big.mark=','), 
                   n.br=2, separator1='', title='Total data in repository')
      ###########
    } else { 
      factoid.grp<-by.id[[factoid.opt]]; # pre-compiled data, with the same name as factoid types
      ind<-grep(paste('^', factoid.key, '$', sep=''), names(factoid.grp), ignore.case=TRUE); # search for key
      
      if (length(ind) > 0) {
        v<-factoid.grp[[ind[1]]]; # factoids to show
        if (factoid.opt == factoid.types[4]) v<-HtmlGeneFactoid(v, genome.versions[1]); # re-format gene location info
        
        ###########
        HtmlKeyValue(v, title=title, separator1=': ', separator2='; '); # format page
        ###########       
      } else {
        ###########
        HtmlKeyValue(c('Not found' = ''), title=title, separator1=''); # format page
        ###########
      } 
    } 
    ###############################################
  })
  ###################################################################################################
  
  ###################################################################################################
  ######################################## "Browse" tab #############################################
  output$browse.table.title<-renderUI({ h2(paste('All archived', input$browse.options)) });
  output$browse.table <- DT::renderDataTable({
    browse.tbls[[input$browse.options]];
  }, options=list(autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 25), rownames=FALSE, escape=FALSE);
  ###################################################################################################

  ###########################################################################################
  ###########################################################################################
  #********************************* SECONDARY ANALYSES ************************************#
  ###########################################################################################
  ###########################################################################################

  ###################################################################################################
  ######################################## "Summary" tab ############################################
  output$summary.table.title<-renderUI({
    rslt<-search.rslt();
    if(identical(NA, rslt)) h2("Summary of search results") else h2('Query:', paste(input$search.options, '=', input$search.value))
  });  
  output$summary.table <- DT::renderDataTable({
   meta.gwas.summarize.results(search.rslt(), input$summary.options, summary.options, input$summary.minimum.phred);
  }, options=list(autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 25), rownames=FALSE, escape=FALSE);
  
  ###################################################################################################
  ######################################## "QQplot" tab #############################################
  output$qqplot.title<-renderUI({
    rslt<-search.rslt();
    if(identical(NA, rslt)) h2("QQ plot of GWAS p values") else h2(paste('Query:', input$search.options, '=', input$search.value))
  });
  output$qqplot <- renderPlot({
    meta.gwas.plot.qq(search.rslt(), input$qq.highlight);  
  }, height=600, width=600);
  ###################################################################################################
  
  ###################################################################################################
  ######################################## "Manhattan" tab ##########################################
  output$manhattan.plot.title<-renderUI({
    rslt<-search.rslt();
    if(identical(NA, rslt)) h2("2D or 3D Manhattan plot of GWAS p values") else h2(paste('Query:', input$search.options, '=', input$search.value))
  });
  output$manhattan.plot <- renderPlot({
    meta.gwas.manhantan3d(search.rslt());
  }, height=600, width=600);
  ###################################################################################################

  ###################################################################################################
  ######################################## "Enrichment" tab #########################################
  output$enrichment.table.title<-renderUI({
    rslt<-search.rslt();
    if(identical(NA, rslt)) h3("Enrichment of keywords related to top hits (waiting for search results ...)") else h2(paste(input$search.options, input$search.value, sep=' = '))
  });  
  output$enrichment.table <- DT::renderDataTable({
   meta.gwas.enrichment(search.rslt(), input$enrich.minimum.phred, input$enrich.minimum.count, input$keyword.options, key.enrich);
  }, options=list(autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 25), rownames=FALSE, escape=FALSE);
  ###################################################################################################
  
})