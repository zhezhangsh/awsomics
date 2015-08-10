print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");
  
  ###################################################################################################
  # Load the collection data
  {
  load.coll0<-reactive({
    rid1<-input$home.table_rows_selected;
    rid2<-input$home.table_row_last_clicked;
    
    if(length(rid1)==0 | length(rid2)==0) NA else if (!(rid2 %in% rid1)) NA else {
      withProgress(geex.load.collection(rid2, GEX_HOME), 
                   message=paste("Loading data collection", rid2, '...'), 
                   detail="\nPlease wait until data is loaded to use GeEx.");
    } 
  });
  
  load.coll<-reactive({ 
      coll.loaded<-load.coll0();
      if (identical(coll.loaded, NA)) NA else {
        if(input$regroup.check) {
          withProgress(
            coll.loaded<-geex.regroup(coll.loaded, input$regroup.dataset_rows_selected, input$regroup.group_rows_selected), 
            message="Re-grouping samples in collection ...", detail="\nPlease wait until the re-grouping is done.");
          updateCheckboxInput(session, 'regroup.check', 'Uncheck box to restore default grouping');
        } else {
          coll.loaded$extra$regrouped<-FALSE;
          updateCheckboxInput(session, 'regroup.check', 'Check box to activate sample re-grouping');          
        }
             
        updateSelectInput(session, 'meta.options', choices=names(coll.loaded$browse_table));
        updateSelectInput(session, 'expr.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'pca.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'bar.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'geneset.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'filter.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'x.dataset', choices=coll.loaded$extra$longname$dataset);
        updateSelectInput(session, 'y.dataset', choices=coll.loaded$extra$longname$dataset);      
        updateSelectInput(session, 'comb.species', choices=unique(coll.loaded$gene$Species));
        
        output$bar.table <- DT::renderDataTable({ 
          geex.load.dataset(coll.loaded, coll.loaded$extra$longname$dataset[1])$anno; 
        }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);            
        output$coex.gene1 <- DT::renderDataTable({ 
          coll.loaded$browse_table$Gene[, 1:5]
        }, options = dt.options2, selection='single', rownames=FALSE, server=TRUE, escape = FALSE);   
        output$coex.gene2 <- DT::renderDataTable({ 
          coll.loaded$browse_table$Gene[, 1:5]
        }, options = dt.options2, selection='single', rownames=FALSE, server=TRUE, escape = FALSE);   

        coll.loaded;
      }
  });

  }
  ###################################################################################################
  ######################################## "Home" tab ###############################################
  # table title
  {
  output$home.title<-renderUI({ 
    list(h1('Welcome to Awsomics - GeEx'), 
         h4(HTML("<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer, beta version"))); });

  # Search table message
  output$home.message<-renderUI({
    if (identical(NA, load.coll())) list(h3("Click on a row to load the data collection")) else 
      list(h3(HTML(paste("Loaded data collection", geex.html.msg(load.coll()$selection)))))
  });

  output$home.table <- DT::renderDataTable({
    data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE)
  }, options=list(dom = 't'), selection='single', rownames=FALSE, escape=FALSE);
  }
  ##############################################################################################################
  
  ##############################################################################################################
  ######################################## "Data" menu #########################################################
  ##############################################################################################################

  ########################################## "Metadata" tab ####################################################      
  {  
  output$meta.title<-renderUI({ 
    list(h2("Metadata"), 
         HTML('Information about data sets, sample groups, samples, and genes.')) }); 

  output$meta.message<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(h3(msg.nocollection)) else {
      list(h3(HTML(paste("Data collection", geex.html.msg(input$select.collection)))), 
           h3(HTML(paste('Table ', geex.html.msg(input$meta.options), sep=''))))}})
  
  output$meta.table <- DT::renderDataTable({
    cll<-load.coll();
    if (!identical(cll, NA)) {
      tbl.nm<-input$meta.options;
      if (tbl.nm=='' | is.na(tbl.nm)) cll$browse_table[[1]] else cll$browse_table[[tbl.nm]];    
    } else geex.empty.matrix("Empty table")
  }, options = dt.options1, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);

  output$meta.detail <- renderUI({
    rid1<-input$meta.table_rows_selected;
    rid2<-input$meta.table_row_last_clicked; 
    if (length(rid1)>0 & length(rid2)>0) if (rid2[1] %in% rid1) {
      cll<-load.coll();
      t<-sapply(cll$metadata_by_id, function(x) rid2[1] %in% names(x));
      if (length(t[t])>0) HtmlKeyValue(cll$metadata_by_id[[which(t)[1]]][[rid2[1]]], separator2=' ');
    }
  })
  
  }
  
  ########################################## "Data set" tab ####################################################      
  {
  summarize.dataset<-reactive({  
    cll<-load.coll(); 
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {      
      ds.selected<-input$expr.dataset;
      if (ds.selected == '') geex.empty.matrix(msg.nodataset) else {
        ds<-geex.load.dataset(cll, ds.selected);
        grp<-input$expr.group;
        grp<-grp[grp %in% names(ds$group)];
        spe<-sub(' homolog$', '', input$expr.species);
        spe<-spe[spe %in% ds$anno$Species];
        if (length(grp) == 0) geex.empty.matrix("No selected group; must select one") else 
          if (length(spe) == 0) geex.empty.matrix("No selected species; must select one") else
            withProgress(geex.dataset.table(cll, ds.selected, grp, spe, input$expr.scale, expr_columns, GetRowStatTypes(), expr_anno_columns, c()), 
                         message="Calculating descriptive stats ...", detail="\nPlease wait for the full result table.");
      }
    }
  });
  
  observeEvent(input$expr.gs.source, {
    updateSelectizeInput(session, 'expr.gs.coll', choices=names(geneset[[input$expr.gs.source]]))});  
  observeEvent(input$expr.gs.coll, { 
    sp<-names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
    if (!identical(NA, load.coll())) {
      ds<-geex.load.dataset(load.coll(), input$expr.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'expr.gs.species', choices=sp); 
  });
  observeEvent(input$expr.gs.species, { 
    output$expr.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$expr.gs.clear, { 
    output$expr.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
    
  observeEvent(input$expr.dataset, {
    ds<-geex.load.dataset(load.coll(), input$expr.dataset);
    if (!identical(NA, ds)) { 
      updateCheckboxGroupInput(session, 'expr.group', choices=names(ds$group), selected=names(ds$group));
      sp<-tolower(unique(as.vector(ds$anno$Species)));
      sp<-sp[sp!='human'];
      if (length(sp)>0) sp<-c(sp, 'human homolog') else sp<-'human';
      updateCheckboxGroupInput(session, 'expr.species', choices=sp, selected=sp);
      sp0<-names(geneset[[input$expr.gs.source]][[input$expr.gs.coll]]);
      if (length(sp) == 1) sp0<-'human' else sp0<-unique(c(sp0[sp0 %in% sp], 'human')); 
      updateSelectizeInput(session, 'expr.gs.species', choices=sp0, selected=sp0[1]); 
      output$expr.message<-renderUI({
        list(h3(HTML(paste('Data set', geex.html.msg(ds$longname)))), br(), br()); });
    }
  });
  observeEvent(input$expr.select.all, {
    if (!input$expr.select.all) {
      updateCheckboxGroupInput(session, 'expr.column', NULL, 
                               choice=expr_columns, selected=expr_columns[1], inline=FALSE);            
      updateCheckboxGroupInput(session, 'expr.desc', 
                               choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno', 
                               choice=expr_anno_columns, 
                               selected=expr_anno_columns[c(1, length(expr_anno_columns))], inline=TRUE)   
    } else {
      updateCheckboxGroupInput(session, 'expr.column', NULL, 
                               choice=expr_columns, selected=expr_columns, inline=FALSE);        
      updateCheckboxGroupInput(session, 'expr.desc', 
                               choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno',
                               choice=expr_anno_columns, selected=expr_anno_columns, inline=TRUE)     
    }
  });
  
  output$expr.title<-renderUI({ 
    list(h2("Data set"), HTML('Gene expression data matrix and descriptive statistics.')) });   
  output$expr.message<-renderUI({ 
    if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else 
      if (input$expr.dataset=='') list(h3(msg.nodataset), br(), br()) });
  output$expr.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(NA, cll))  geex.empty.matrix(msg.nocollection) else {
      tbl<-summarize.dataset();
      if (colnames(tbl)[1] != 'ID') tbl else {
        cmn<-c(input$expr.anno, input$expr.desc);
        cs<-colnames(tbl);
        cs<-cs[!(cs %in% c('ID', expr_anno_columns, GetRowStatTypes()))];
        cnm.grp<-cs[grep('^G[0-9]', cs)];
        cnm.smp<-cs[grep('^S[0-9]', cs)];
        col<-input$expr.column;
        if (expr_columns[1] %in% col) cmn<-c(cmn, cnm.grp);
        if (expr_columns[2] %in% col) cmn<-c(cmn, cnm.smp);
        tbl<-tbl[, c('ID', cmn), drop=FALSE];
        rid<-CleanHtmlTags(input$expr.gs.table_rows_selected);
        rid<-rid[rid %in% rownames(geneset[[input$expr.gs.source]][[input$expr.gs.coll]][[input$expr.gs.species]])]; 
        if (length(rid) == 0) tbl else {
          gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$expr.gs.source), '_list.rds', sep=''))[rid];
          gn<-unique(unlist(gs, use.names=FALSE));
          gn<-gn[gn %in% rownames(tbl)];
          if (length(gn) == 0) geex.empty.matrix("No gene of selected gene set found in data set") else 
            tbl[gn, , drop=FALSE];
        }         
      }}
  }, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  
  }
  
  ########################################## "Gene-Data set" tab ###############################################      
  {
  combine.dataset<-reactive({   
    cll<-load.coll();
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {
      withProgress(geex.combined.table(cll, input$comb.species, input$comb.scale), 
                   message="Calculating descriptive stats ...", detail="\nPlease wait for the full result table.")
    }
  });

  observeEvent(input$comb.filter, {
    if (input$comb.filter) {
      cll<-load.coll();
      sp<-names(geneset[[1]][[1]]);
      sp<-union(sp[sp %in% cll$gene$Species], 'human');
      output$comb.ui <- renderUI({ wellPanel(fluidRow(
        column(3, br(),
               selectizeInput("comb.gs.source", "Source", 
                              names(geneset), selected=names(geneset)[1], width='80%'), 
               selectizeInput("comb.gs.coll", "Collection", 
                              names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
               selectizeInput("comb.gs.species", "Species", 
                              sp, selected='human', width='80%'), br(),
               column(2, h1(""), column(10, actionButton("comb.clear", 'Clear selection')))), 
        column(9, DT::dataTableOutput('comb.gs.table')))) }) 
    } else { output$comb.ui <- renderUI({br()}) }
  });
  
  observeEvent(input$comb.gs.source, { 
    updateSelectizeInput(session, 'comb.gs.coll', choices=names(geneset[[input$comb.gs.source]]))});  
  observeEvent(input$comb.gs.coll, { 
    cll<-load.coll();
    sp<-names(geneset[[input$comb.gs.source]][[input$comb.gs.coll]]);
    if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
    updateSelectizeInput(session, 'comb.gs.species', choices=sp, selected='human'); 
  });
  observeEvent(input$comb.gs.species, { 
    output$comb.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$comb.gs.clear, { 
    output$comb.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  
  output$comb.title<-renderUI({ 
    list(h2("Gene-Data set"), HTML('Summary of gene expression level across all data sets. Using relative expression percentiles is preferred if data sets were from different studies.')) });   
  output$comb.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(NA, cll))  geex.empty.matrix(msg.nocollection) else { 
      tbl<-combine.dataset(); 
      if (colnames(tbl)[1] != 'ID') tbl else {
        cmn<-c('ID', input$comb.anno, input$comb.desc, 'Mx-Mn', rownames(cll$metadata$Dataset));
        cmn<-cmn[cmn %in% colnames(tbl)]; 
        tbl<-tbl[, cmn, drop=FALSE]; 
        
        rid<-CleanHtmlTags(input$comb.gs.table_rows_selected);
        if (length(rid)==0)  tbl else {
          rid<-rid[rid %in% rownames(geneset[[input$comb.gs.source]][[input$comb.gs.coll]][[input$comb.gs.species]])]; 
          gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$comb.gs.source), '_list.rds', sep=''))[rid];  
          gn<-unique(unlist(gs, use.names=FALSE));
          gn<-gn[gn %in% rownames(tbl)];
          if (length(gn) == 0) geex.empty.matrix("No more genes after filtering") else tbl[gn, , drop=FALSE];
          }}}}, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  }
  
  ########################################## "Gene summary" tab ################################################      
  {  
  observeEvent(input$gene.filter, {
    if (input$gene.filter) {
      cll<-load.coll();
      sp<-names(geneset[[1]][[1]]);
      sp<-union(sp[sp %in% cll$gene$Species], 'human');
      output$gene.ui <- renderUI({ wellPanel(fluidRow(
        column(3, br(),
               selectizeInput("gene.gs.source", "Source", 
                              names(geneset), selected=names(geneset)[1], width='80%'), 
               selectizeInput("gene.gs.coll", "Collection", 
                              names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
               selectizeInput("gene.gs.species", "Species", sp, selected='human', width='80%'), br(),
               column(2, h1(""), column(10, actionButton("gene.clear", 'Clear selection')))), 
        column(9, DT::dataTableOutput('gene.gs.table')))) }) 
    } else { output$gene.ui <- renderUI({br()}) }
  });
  
  observeEvent(input$gene.gs.source, { 
    updateSelectizeInput(session, 'gene.gs.coll', choices=names(geneset[[input$gene.gs.source]]))});  
  observeEvent(input$gene.gs.coll, { 
    cll<-load.coll();
    sp<-names(geneset[[input$gene.gs.source]][[input$gene.gs.coll]]);
    if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
    updateSelectizeInput(session, 'gene.gs.species', choices=sp, selected='human'); 
  });
  observeEvent(input$gene.gs.species, { 
    output$gene.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$gene.clear, { 
    output$gene.gs.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$gene.gs.source, input$gene.gs.coll, input$gene.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  
  output$gene.title<-renderUI({ 
    list(h2("Single gene information"), 
         HTML('Summarize the descriptive statistics of a single gene across data sets.')) }); 
  
  output$gene.message<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(h3(msg.nocollection), br()) else {
      rid0<-input$gene.table_row_last_clicked; 
      rid1<-input$gene.table_rows_selected; 
      if (length(rid1)>0 & length(rid0)>0)  
        if (rid0 %in% rid1) geex.html.gene.name(rid0, TRUE, cll) else list(h3(msg.nogene), br()) else
          list(h3(msg.nogene), br())
    }});
  
  output$gene.table <- DT::renderDataTable({
    cll<-load.coll();
    if (!identical(cll, NA)) { 
      anno<-cll$gene;
      if (input$gene.filter) {
        rid<-CleanHtmlTags(input$gene.gs.table_rows_selected);
        if (length(rid)>0) {
          rid<-rid[rid %in% rownames(geneset[[input$gene.gs.source]][[input$gene.gs.coll]][[input$gene.gs.species]])]; 
          gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$gene.gs.source), '_list.rds', sep=''))[rid];  
          gn<-unique(unlist(gs, use.names=FALSE));
          anno<-anno[rownames(anno) %in% gn, , drop=FALSE];
        }
      }
      if (nrow(anno) == 0) { 
        if (length(rid)>0) geex.empty.matrix("No more genes after filtering") else 
          geex.empty.matrix("No gene information in data set")
      } else {
        data.frame(ID=AddHref(rownames(anno), UrlEntrezGene(rownames(anno))), 
                   Name=as.vector(anno$Symbol), Species=as.vector(anno$Species),
                   Type=sub('^protein-coding', 'coding', as.vector(anno$type_of_gene)), 
                   N_Set=anno$Num_Dataset, Synonyms=as.vector(anno$Synonyms), 
                   Description=as.vector(anno$description), stringsAsFactors=FALSE);
      }
    } else geex.empty.matrix("Empty table")
  }, options = dt.options2, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);
  
  output$gene.stat.table <- DT::renderDataTable({
    cll<-load.coll();
    rid0<-input$gene.table_row_last_clicked; 
    rid1<-input$gene.table_rows_selected; 
    if (length(rid1)>0 & length(rid0)>0)  if (rid0[1] %in% rid1) geex.summarize.gene(cll, rid0) else 
      geex.empty.matrix(msg.nogene) else
        geex.empty.matrix(msg.nogene); 
  }, options = dt.options1, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);  
  }
  
  ###################################################################################################
  ######################################## "Visualize" menu #########################################
  ###################################################################################################

  ########################################################################################
  ######################################## "PCA Plot" tab ################################
  {
  observeEvent(input$pca.dataset, {
    ds<-geex.load.dataset(load.coll(), input$pca.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'pca.group', choices=names(ds$group), selected=names(ds$group));
      updateSelectizeInput(session, 'pca.x', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC1');
      updateSelectizeInput(session, 'pca.y', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC2');
      
      sp0<-input$pca.geneset.species;
      sp<-names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
      ds<-geex.load.dataset(load.coll(), input$pca.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
      if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1];
      updateSelectizeInput(session, 'pca.geneset.species', choices=sp, selected=sp1);      
    }
  });
  observeEvent(input$pca.clear, {
    if (!identical(NA, cll<-load.coll()))
      output$pca.table <- DT::renderDataTable({
        cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], 
                            c('Name', 'Group'), drop=FALSE];
      }, options = dt.options3, escape = FALSE);
  });
  
  observeEvent(input$pca.geneset.source, { 
    updateSelectizeInput(session, 'pca.geneset.coll', choices=names(geneset[[input$pca.geneset.source]])); 
  });
  observeEvent(input$pca.geneset.coll, { 
    sp<-names(geneset[[input$pca.geneset.source]][[input$pca.geneset.coll]]);
    if (!identical(NA, load.coll())) {
      ds<-geex.load.dataset(load.coll(), input$pca.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'pca.geneset.species', choices=sp); 
  });
  observeEvent(input$pca.geneset.species, { 
    output$pca.geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$pca.geneset.clear, { 
    output$pca.geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$pca.geneset.source, input$pca.geneset.coll, input$pca.geneset.species);  
    }, options = dt.options2, rownames=FALSE, selection='single', server=TRUE, escape = FALSE)
  });
  
  output$pca.title<-renderUI({list(
    h2("Principal Components Analysis"), 
    HTML(paste('Unsupervised clustering of samples in the same data set by',  
         '<a href="https://en.wikipedia.org/wiki/Principal_component_analysis" target="_blank">PCA</a>'))) });  
  output$pca.message<-renderUI({ 
    if (identical(NA, load.coll())) list(h3(HTML(msg.nocollection)), br(), br())  else 
      list(h3(HTML(paste("Data set", geex.html.msg(input$pca.dataset)))), br(), br()) })
  output$pca.plot <- renderPlot({
    if (length(input$pca.geneset.table_rows_selected) > 0) {
      src<-input$pca.geneset.source;
      cll<-input$pca.geneset.coll;
      spe<-input$pca.geneset.species;
      rid<-CleanHtmlTags(input$pca.geneset.table_rows_selected);
      rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
      if (length(rid) > 0) gn.subset<-unique(unlist(readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid], use.names=FALSE)) else 
        gn.subset<-c();
    } else gn.subset<-NA;
    
    geex.plot.pca(load.coll(), input$pca.x, input$pca.y, input$pca.color, input$pca.dataset, input$pca.group, 
                  input$pca.table_rows_selected, gn.subset); }, height = 720, width = 960);
  
  output$pca.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(cll, NA)) geex.empty.matrix("Empty table") else 
      cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], 
                          c('Name', 'Group'), drop=FALSE];
  }, options = dt.options2, escape = FALSE);
  }
  
  ########################################################################################
  ######################################## "Bar Plot" tab ################################ 
  {
  select.gene <- reactive({
    cll<-load.coll();
    
    ds<-input$bar.dataset;
    rid<-input$bar.table_rows_selected;
    selected<-geex.select.gene(cll, ds, input$bar.group, rid);
    
    updateSelectizeInput(session, 'bar.dataset', choices=selected$dataset, selected=ds);
    
    if (length(rid)==0 | identical(cll, NA)) output$bar.dataset.message<-renderUI({ '' }) else {
      output$bar.dataset.message<-renderUI({
        if (setequal(rownames(cll$metadata$Dataset), selected$dataset)) '' else 
          list(HTML(geex.html.msg("Only data sets including all selected genes are listed;")), br(), HTML(geex.html.msg("Clear gene selection to list all data sets.")))
      }) 
    }     
    selected;
  });
  
  observeEvent(input$bar.dataset, {
    ds<-geex.load.dataset(load.coll(), input$bar.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'bar.group', choices=names(ds$group), selected=names(ds$group));   
      if (length(input$bar.table_rows_selected) == 0) output$bar.table <- DT::renderDataTable({ ds$anno; }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
    } else {
      output$bar.table <- DT::renderDataTable({ geex.empty.matrix(msg.nodataset) }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);
    }
  });

  observeEvent(input$bar.clear, {
    cll<-load.coll();
    ds<-input$bar.dataset;
    output$bar.table <- DT::renderDataTable({ 
      if (!identical(NA, cll)) geex.load.dataset(cll, ds)$anno else
        geex.empty.matrix(msg.nocollection);
    }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE); 
    updateSelectInput(session, 'bar.dataset', choices=cll$extra$longname$dataset, selected=ds);
  });
  
  observeEvent(input$bar.table_row_last_clicked, {
    ds<-geex.load.dataset(load.coll(), input$bar.dataset);
    rid<-CleanHtmlTags(input$bar.table_rows_selected);
    rid0<-CleanHtmlTags(input$bar.table_row_last_clicked);
    output$bar.table.message<-renderUI({ '' })
    if (length(rid)>0) if (rid[length(rid)]==rid0) if (!(rid0 %in% rownames(ds$anno))) {
      output$bar.table.message<-renderUI({
        list(HTML(geex.html.msg(paste("Gene <strong>", rid0, "</strong> is not included in data set ", " <strong>", ds$longname, '</strong>', sep=''))), br(),
             HTML(geex.html.msg("Clear gene selction to show all genes available in current data set")))
      });
    }
  })
  
  output$bar.title<-renderUI({ list(
    h2("Expression level of individual gene(s)"), 
    HTML("Make bar plot of individual genes to compare their expression levels to each other or between samples.")); });
  
  output$bar.message<-renderUI({ h3(select.gene()$message) });

  output$bar.stat <- DT::renderDataTable({ select.gene()$table; }, options = dt.options5, rownames=FALSE, server=TRUE, escape = FALSE);

  output$bar.plot <- renderPlot({ 
    ds<-geex.load.dataset(load.coll(), input$bar.dataset); 
    geex.plot.bar(ds, input$bar.stat_rows_all, input$bar.scale, input$bar.color, input$bar.group, input$bar.mean);  
  });
  
  ########################## Gene dictionary 
  observeEvent(input$lookup.option, {
    cll<-load.coll();
    if (input$lookup.option & !identical(NA, cll)) {
      output$lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
      output$lookup.key.ui<-renderUI({ textInput("bar.lookup.key", "", '') });
      output$lookup.species.ui<-renderUI({ 
        selectizeInput("bar.lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
      output$lookup.table.ui<-renderUI({ DT::dataTableOutput('bar.lookup.table') });
    } else { 
      updateTextInput(session, "bar.lookup.key", NULL, NULL);
      updateSelectizeInput(session, "bar.lookup.species", NULL, NULL, NULL);
      output$lookup.key.ui <- renderUI({br()});
      output$lookup.species.ui <- renderUI({br()}); 
      output$lookup.table.ui <- renderUI({br()}); 
    }
  });
  observeEvent(input$bar.lookup.key, {
    ky<-input$bar.lookup.key;
    output$bar.lookup.table <- DT::renderDataTable({
      if (ky!='' & !is.null(ky)) geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$bar.lookup.species, ky, GENE_HOME) else 
        geex.empty.matrix('No search key');
    }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  });
  
  ########################## 
  
  } # end of menu tab
  
  ########################################################################################
  ######################################## "Geneset" tab #################################
  {
  observeEvent(input$geneset.dataset, {
    ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'geneset.group', choices=names(ds$group), selected=names(ds$group));
      sp0<-input$geneset.species;
      sp<-names(geneset[[input$geneset.source]][[input$geneset.coll]]);
      ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
      if (sp0 %in% sp) sp1<-sp0 else sp1<-sp[1];
      updateSelectizeInput(session, 'geneset.species', choices=sp, selected=sp1);       
    }  
  });
  observeEvent(input$geneset.source, { 
    updateSelectizeInput(session, 'geneset.coll', choices=names(geneset[[input$geneset.source]])); 
  });
  observeEvent(input$geneset.coll, { 
    sp<-names(geneset[[input$geneset.source]][[input$geneset.coll]]);
    if (!identical(NA, load.coll())) {
      ds<-geex.load.dataset(load.coll(), input$geneset.dataset);
      ds.sp<-unique(as.vector(ds$anno$Species));
      sp<-sp[sp %in% ds.sp];
      sp<-c(sp[tolower(sp)!='human'], 'human');
    };
    updateSelectizeInput(session, 'geneset.species', choices=sp); 
  });
  observeEvent(input$geneset.species, { 
    output$geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
    }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
    output$geneset.stat <- DT::renderDataTable({ 
      geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$geneset.clear, { 
    output$geneset.table <- DT::renderDataTable({ 
      geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
    }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
    output$geneset.stat <- DT::renderDataTable({ 
      geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
  });
  
  output$geneset.title<-renderUI({ list(
    h2("Expression pattern of a gene set"),
    HTML('Compare and visualize overall expression levels of a gene set between sample groups.')) });
  output$geneset.message<-renderUI({
    if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else {
      rid0<-input$geneset.table_rows_selected;
      rid<-input$geneset.table_row_last_clicked;
      if (length(rid0)>0 & length(rid)>0) {
        if (rid0[length(rid0)]==rid) {
          rid<-CleanHtmlTags(input$geneset.table_rows_selected);
          nm<-as.vector(geneset[[input$geneset.source]][[input$geneset.coll]][[input$geneset.species]][rid[length(rid)], 'Name'])
          list(h3(HTML(paste('Gene set', geex.html.msg(nm)))), br(), br());
        } else list(h3(msg.nogeneset), br(), br());
      } else list(h3(msg.nogeneset), br(), br());
    }
  });
  
  output$geneset.plot <- renderPlot({
    rid0<-input$geneset.table_rows_selected;
    rid<-input$geneset.table_row_last_clicked;
    if (length(rid0)>0 & length(rid)>0) if (rid0[length(rid0)]==rid) {
      src<-input$geneset.source;
      cll<-input$geneset.coll;
      spe<-input$geneset.species;
      grp<-input$geneset.group;
      rid<-CleanHtmlTags(input$geneset.table_rows_selected);
      rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
      rid<-rid[length(rid)];
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid];
      if (length(gs) > 0) {
        names(gs)<-as.vector(geneset[[src]][[cll]][[spe]][names(gs), 'Name']);  
        out<-geex.plot.geneset(load.coll(), grp, input$geneset.type, input$geneset.scale, 
                               input$geneset.color, input$geneset.normalize, gs[[1]]);  
      }
    }
  });
  
  }
  
  ########################################################################################
  ######################################## "Compare two tab" tab #########################
  {
  observeEvent(input$x.dataset, {
    ch.x<-geex.longname.set2groups(load.coll(), input$x.dataset);
    updateSelectizeInput(session, 'x.group', choices=ch.x);
  });
  observeEvent(input$y.dataset, {
    ch.y<-geex.longname.set2groups(load.coll(), input$y.dataset);
    updateSelectizeInput(session, 'y.group', choices=ch.y, selected=ch.y[length(ch.y)]);
  });
  
  observeEvent(input$x.group, {
    output$two.message<-renderUI({ 
      list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ 
      geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])
    }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
    updateSelectizeInput(session, 'x.sample', 
                         choices=c('', geex.longname.group2samples(load.coll(), input$x.group)));
  });
  observeEvent(input$y.group, { 
    output$two.message<-renderUI({ 
      list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });    
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ 
      geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])
    }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
    updateSelectizeInput(session, 'y.sample', 
                         choices=c('', geex.longname.group2samples(load.coll(), input$y.group)));
  });
  observeEvent(input$x.sample, {
    if (input$x.sample != '') lab.x<-input$x.sample else lab.x<-input$x.group;
    if (input$y.sample != '') lab.y<-input$y.sample else lab.y<-input$y.group;
    output$two.message<-renderUI({ 
      list(h3(HTML(paste(geex.html.msg(lab.x), 'vs.', geex.html.msg(lab.y)))), br()); });
  });
  observeEvent(input$y.sample, {
    if (input$x.sample != '') lab.x<-input$x.sample else lab.x<-input$x.group;
    if (input$y.sample != '') lab.y<-input$y.sample else lab.y<-input$y.group;
    output$two.message<-renderUI({ 
      list(h3(HTML(paste(geex.html.msg(lab.x), 'vs.', geex.html.msg(lab.y)))), br()); });
  });
  
  observeEvent(input$two.source, { 
    updateSelectizeInput(session, 'two.coll', choices=names(geneset[[input$two.source]])); 
  });
  observeEvent(input$two.coll, { 
    sp<-names(geneset[[input$two.source]][[input$two.coll]]);
    if (!identical(NA, load.coll())) 
      sp<-unique(c(sp[sp %in% geex.get2group.species(load.coll(), input$x.group, input$y.group)], 'human'));
    updateSelectizeInput(session, 'two.species', choices=sp); 
  });
  observeEvent(input$two.species, { 
    output$two.table.geneset <- DT::renderDataTable({  geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
    output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$two.clear.geneset, { 
    output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
    output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix(msg.nogeneset); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$two.clear, { 
    output$two.table <- DT::renderDataTable({ 
      geex.get2group(load.coll(), input$x.group, input$y.group, input$two.select.species)
    }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
  });
  observeEvent(input$two.select.species, { 
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group);
    sp1<-names(geneset[[input$two.source]][[input$two.coll]]);
    if (!setequal(sp, sp1) | input$two.select.species!=input$two.species) {
      sp2<-unique(c(sp1[sp1 %in% sp], 'human'));
      sp3<-input$two.select.species;
      if (!(sp3 %in% sp2)) sp3<-'human'
      updateSelectizeInput(session, 'two.species', sp2, sp3); 
    }
    output$two.table<-DT::renderDataTable({ 
      geex.get2group(load.coll(), input$x.group, input$y.group, sp<-input$two.select.species)
    }, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
  });
  
  output$two.title<-renderUI({list(
    h2("Differential expression between two groups/samples"),
    HTML('Compare expression levels of any 2 groups (default) or samples at 3 levels: global, gene set, and single gene.'))});
  output$two.message<-renderUI({if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()); });
  output$two.table <- DT::renderDataTable({ geex.empty.matrix('Empty table'); });

  output$two.plot <- renderPlot({
    src<-input$two.source;
    cll<-input$two.coll;
    spe<-input$two.species; 
    rid<-CleanHtmlTags(input$two.table.geneset_rows_selected);
    rid<-rid[rid %in% rownames(geneset[[src]][[cll]][[spe]])]; 
    gs<-list();
    if (length(rid)==0) spe<-input$two.select.species else {
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(src), '_list.rds', sep=''))[rid];
      if (length(gs) > 0) names(gs)<-as.vector(geneset[[src]][[cll]][[spe]][names(gs), 'Name']);  
    }
    cnm.x<-input$x.group;
    cnm.y<-input$y.group;
    if (input$x.sample != '') cnm.x<-input$x.sample;
    if (input$y.sample != '') cnm.y<-input$y.sample; 
    if (cnm.x!='' & cnm.y!='') 
      out<-geex.plot.two(load.coll(), cnm.x, cnm.y, input$two.type, input$two.scale, input$two.color, 
                         spe, input$two.table_rows_selected, gs);  
    if (length(gs) == 0) output$two.geneset.stat<-DT::renderDataTable({ 
      geex.empty.matrix(msg.nogeneset); 
    }, options=list(dom='t'), selection='none') else
      output$two.geneset.stat<-DT::renderDataTable({ 
        out$geneset.stat 
    }, options=dt.options5, rownames=FALSE, selection='none', server=TRUE, escape = FALSE); 
  });
  }
  
  
  ########################################################################################
  ########################################## "Coexpression" tab ##########################      
  {
  coex.results<-reactive({ 
    cll<-load.coll();
    
    rid1s<-input$coex.gene1_rows_selected; 
    rid1<-input$coex.gene1_row_last_clicked;
    if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
    
    rid2s<-input$coex.gene2_rows_selected; 
    rid2<-input$coex.gene2_row_last_clicked;
    if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
    
    coex<-geex.coex.data(cll, id1, id2, input$coex.scale, input$coex.color);
    
    coex;
  });
  
  observeEvent(input$coex.lookup.option, {
    cll<-load.coll();
    if (input$coex.lookup.option & !identical(NA, cll)) {
      output$coex.lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
      output$coex.lookup.key.ui<-renderUI({ textInput("coex.lookup.key", "", '') });
      output$coex.lookup.species.ui<-renderUI({ 
        selectizeInput("coex.lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
      output$coex.lookup.table.ui<-renderUI({ DT::dataTableOutput('coex.lookup.table') });
    } else { 
      updateTextInput(session, "coex.lookup.key", NULL, NULL);
      updateSelectizeInput(session, "coex.lookup.species", NULL, NULL, NULL);
      output$coex.lookup.key.ui <- renderUI({br()});
      output$coex.lookup.species.ui <- renderUI({br()}); 
      output$coex.lookup.table.ui <- renderUI({br()}); 
    }
  }); 
  observeEvent(input$coex.lookup.key, {
    ky<-input$coex.lookup.key;
    output$coex.lookup.table <- DT::renderDataTable({
        if (ky!='' & !is.null(ky)) geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$coex.lookup.species, ky, GENE_HOME) else 
          geex.empty.matrix('No search key');
      }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE)   
  });
  
  observeEvent(input$coex.clear.ds, {
    output$coex.dataset <- DT::renderDataTable({  
      coex<-coex.results(); 
      if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
    }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
  });
  
  output$coex.title<-renderUI({ 
    list(h2("Gene-gene coexpression"), 
         HTML(c('Coexpression of two selected genes within and across data sets.', 
                'The APP <strong>CoEx</strong> will provide more functions for coexpression analysis.'))) 
  });
  
  output$coex.message<-renderUI({ 
    cll<-load.coll();
    
    rid1s<-input$coex.gene1_rows_selected; 
    rid1<-input$coex.gene1_row_last_clicked;
    if (length(rid1)>0 & length(rid1s)>0) if (rid1 %in% rid1s) id1<-CleanHtmlTags(rid1) else id1<-c() else id1<-c();
    
    rid2s<-input$coex.gene2_rows_selected; 
    rid2<-input$coex.gene2_row_last_clicked;
    if (length(rid2)>0 & length(rid2s)>0) if(rid2 %in% rid2s) id2<-CleanHtmlTags(rid2) else id2<-c() else id2<-c()
    
    if (length(id1)>0) nm1<-as.vector(cll$gene[id1, 'Symbol']) else nm1<-'';
    if (length(id2)>0) nm2<-as.vector(cll$gene[id2, 'Symbol']) else nm2<-'';
    
    if (nm1=='' & nm2=='') h3(HTML(geex.html.msg(msg.nogene))) else 
      h3(HTML(paste(geex.html.msg(nm1), 'vs.', geex.html.msg(nm2))));      
  });
  
  output$coex.dataset <- DT::renderDataTable({  
    coex<-coex.results(); 
    if (!coex$plot) geex.empty.matrix(coex$status) else coex$stat;
  }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);   
  
  output$coex.plot <- renderPlot({  
    coex<-coex.results();
    geex.coex.plot(coex, type=which(data.level.options==input$coex.type), input$coex.dataset_rows_selected);
  })
}

  ##############################################################################################################
  ######################################## "Advance" menu ######################################################
  ##############################################################################################################

  ########################################## "filter gene" tab ######################################################
  {
    filter.table <- reactive({ 
      withProgress(tbl<-geex.filter.table(load.coll(), input$filter.dataset, input$filter.group, input$filter.sample), 
                   message='Preparing gene filtering table ...', detail='\nPlease wait until full table appears below.');
      updateSliderInput(session, 'filter.absolute', value=c(0, 100));
      updateSliderInput(session, 'filter.relative', value=c(0, 100)); 
      ch<-c('Higher', 'Lower');
      if (tbl$is.empty) updateRadioButtons(session, 'filter.direction', label=ch) else {
        names(ch)<-paste(c('Higher', 'Lower'), 'in', tbl$id); 
        updateRadioButtons(session, 'filter.direction', NULL, choices=ch, selected=ch[1], inline=TRUE);
      }
         
      tbl;
    });
    
    observeEvent(input$filter.dataset, {
      grp<-geex.longname.set2groups(load.coll(), input$filter.dataset);
      updateSelectizeInput(session, 'filter.group', choices=c('', grp));
    });
    observeEvent(input$filter.group, {
      smp<-geex.longname.group2samples(load.coll(), input$filter.group);
      updateSelectizeInput(session, 'filter.sample', choices=c('', smp));
    });
    
    output$filter.title<-renderUI({ 
      list(h2("Filter gene"), 
           HTML(c('Use this page to identify genes whose absolute or relative expression is among the highest or lowest in a data set/group/sample.', 
                  'All values in the tables are percentiles of expression (100 = highest expression).',
                  'The selection could be based on absolute expression level, or relative difference comparing to other data sets/groups/samples',
                  'The APP <strong>DeEx</strong> will provide more functions for analyzing differential expression.',
                  'Use cases of this page include:')), br(),
           HTML(' --- <i>Select genes having the highest/lowest expression level in a data set;</i>'), br(),
           HTML(' --- <i>Find genes activated or silenced in a specific tissue, developmental stage, etc.</i>'), br(),
           HTML(' --- <i>If a sample was identified as an outlier, which genes contribute the most to its difference from other samples.</i>')) 
    });    
    output$filter.msg<-renderUI({ list(h3(HTML(filter.table()$message)), br()) })
    output$filter.table <- DT::renderDataTable({  
      tbl<-filter.table(); 
      if (tbl$is.empty) geex.empty.matrix(tbl$message) else {
        g<-tbl$gex;
        range.obs<-input$filter.absolute;
        c<-round(g[, tbl$id]);
        g<-g[c>=min(range.obs) & c<=max(range.obs) & !is.na(c), , drop=FALSE];
        if (nrow(g)>0 & (input$filter.relative[1]>0 | input$filter.relative[2]<100)) {
          if (tolower(input$filter.direction) == 'lower') c<- -1*round(g[, paste(tbl$id, 'Min', sep='_vs_')]) else
            c<-round(g[, paste(tbl$id, 'Max', sep='_vs_')]);
          range.rel<-input$filter.relative;
          g<-g[c>=min(range.rel) & c<=max(range.rel) & !is.na(c), , drop=FALSE];
        }
        if (nrow(g) == 0) geex.empty.matrix("No genes left after filtering") else g;
      }
    }, options = dt.options1, filter='bottom', rownames=FALSE, server=TRUE, escape = FALSE);       
  }


  ########################################## "Re-group" tab ####################################################      
  # Allow users to regroup samples into datasets and groups.
  {
  observeEvent(input$regroup.clear.ds, { 
    output$regroup.dataset<-DT::renderDataTable({
      cll<-load.coll0();
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else 
        geex.regroup.options(cll$metadata$Sample);
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
  });
  observeEvent(input$regroup.clear.grp, { 
    output$regroup.group<-DT::renderDataTable({
      cll<-load.coll0();
      if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
        geex.regroup.options(cll$metadata$Sample);
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
  });

  output$regroup.title<-renderUI({ 
    list(h2("Re-group samples"), HTML('Re-define data sets and groups based on sample features. Note that grouping samples from different studies into the same data set should always be avoided.')) 
  });
  output$regroup.grp.msg<-renderUI({
    rid<-input$regroup.group_rows_selected;
    if (length(rid)==0) h3(HTML("No feature(s) selected to define sample groups")) else 
      h3(HTML(paste("Define sample groups by", geex.html.msg(paste(rid, collapse=' & ')))));
  }); 
  output$regroup.ds.msg<-renderUI({
    rid<-input$regroup.dataset_rows_selected;
    if (length(rid)==0) h3(HTML("No feature(s) selected to define data sets")) else 
      h3(HTML(paste("Define data sets by", geex.html.msg(paste(rid, collapse=' & ')))));
  }); 
  output$regroup.dataset<-DT::renderDataTable({
    cll<-load.coll0();
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else {
      geex.regroup.options(cll$metadata$Sample);
    }
  }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
  output$regroup.group<-DT::renderDataTable({
    cll<-load.coll0(); 
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
      geex.regroup.options(cll$metadata$Sample);
  }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
  
  output$regroup.ds.tbl <- DT::renderDataTable({
    rid<-input$regroup.dataset_rows_selected;
    cll<-load.coll();
    if (length(rid)==0) 
      updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
      geex.regroup.outputs(cll$metadata$Sample, rid, c())$dataset;
  }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
  output$regroup.grp.tbl <- DT::renderDataTable({
    rid0<-input$regroup.dataset_rows_selected;
    rid<-input$regroup.group_rows_selected; 
    cll<-load.coll();
    if (length(rid0)==0 | length(rid)==0) 
      updateCheckboxInput(session, "regroup.check", 'Check box to activate sample re-grouping', value=FALSE);
    if (!identical(NA, cll) & length(rid0)==0) output$regroup.group<-DT::renderDataTable({
      geex.regroup.options(cll$metadata$Sample);
    }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);
    if (identical(NA, cll)) geex.empty.matrix(msg.nocollection) else
      geex.regroup.outputs(cll$metadata$Sample, rid0, rid)$group;
  }, options = dt.options3, filter='none', rownames=FALSE, escape = FALSE);  
  
  }

  ########################################## "Upload" tab ######################################################
  {
  output$upload.title<-renderUI({ 
    list(h2("Upload user-specific data"), 
         HTML('It is possible to upload your own data collection and data sets for them to be explored in GeEx. However, two issues need to be addressed first:'), br(),
         HTML(' --- <strong>Data format</strong>. GeEx has very specific requirements about how the data collection should be formatted.'), br(),
         HTML(' --- <strong>Data privacy</strong>. If the uploaded data sets were not published yet, they need to be destroyed after each session.'), br(),
         HTML('So, please contact us (zhangz@email.chop.edu) if you have such need, and we will give you further details on how to make it happen.s')) 
  });
  }

});

