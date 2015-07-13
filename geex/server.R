print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");
  
  ###################################################################################################
  # Load the collection data
  {
  load.coll<-reactive({ 
    if (input$select.collection == '') NA else {
      coll.loaded<-geex.load.collection(input$select.collection, GEX_HOME);

      updateSelectInput(session, 'meta.options', choices=names(coll.loaded$browse_table));
      updateSelectInput(session, 'expr.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'pca.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'bar.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'geneset.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'x.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'y.dataset', choices=coll.loaded$extra$longname$dataset);      
      updateSelectInput(session, 'comb.species', choices=unique(coll.loaded$gene$Species));
      
      output$bar.table <- DT::renderDataTable({ geex.load.dataset(coll.loaded, coll.loaded$extra$longname$dataset[1])$anno; }, 
                                              options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);     
      
      coll.loaded;
    }
  });
  }
  
  ###################################################################################################
  ######################################## "Home" tab #############################################
  # table title
  {
  output$home.title<-renderUI({ list(h1('Welcome to Awsomics: GeEx'), h4(HTML("<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer, demo version"))); });

  # Search table message
  output$home.message<-renderUI({ 
    if (identical(NA, load.coll())) list(h3("Transcriptome data collections"), br()) else 
      list(h3(HTML(paste("Loaded data collection", geex.html.msg(load.coll()$selection)))), br())
  });

  output$home.table <- DT::renderDataTable({
    data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE)
  }, options=list(dom = 't'), selection='none', rownames=FALSE, escape=FALSE);
  }
  ###################################################################################################
  
  ###################################################################################################
  ######################################## "Data" menu ##############################################
  ###################################################################################################  

  ########################################## "Metadata" tab ###############################################      
  {  
  output$meta.title<-renderUI({ list(h2("Metadata"), HTML('Information about data sets, sample groups, samples, and genes.')) }); 

  output$meta.message<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(h3(msg.nocollection)) else {
      list(h3(HTML(paste("Data collection", geex.html.msg(input$select.collection)))), h3(HTML(paste('Table ', geex.html.msg(input$meta.options), sep=''))))}})
  
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
  ########################################## "Data set" tab ###############################################      
  {
  summarize.dataset<-reactive({   
    cll<-load.coll();
    if (identical(NA, cll)) geex.empty.matrix("No loaded data collection") else {      
      ds.selected<-input$expr.dataset;
      if (ds.selected == '') geex.empty.matrix("No selected data set") else {
        ds<-geex.load.dataset(cll, ds.selected);
        grp<-input$expr.group;
        grp<-grp[grp %in% names(ds$group)];
        spe<-sub(' homolog$', '', input$expr.species);
        spe<-spe[spe %in% ds$anno$Species];
        if (length(grp) == 0) geex.empty.matrix("No selected group; must select one") else 
          if (length(spe) == 0) geex.empty.matrix("No selected species; must select one") else 
          geex.dataset.table(cll, ds.selected, grp, spe, input$expr.scale, expr_columns, GetRowStatTypes(), expr_anno_columns, c());
      }
    }
  });
  
  observeEvent(input$expr.gs.source, { updateSelectizeInput(session, 'expr.gs.coll', choices=names(geneset[[input$expr.gs.source]]))});  
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
    output$expr.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$expr.gs.clear, { 
    output$expr.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$expr.gs.source, input$expr.gs.coll, input$expr.gs.species);  
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
      output$expr.message<-renderUI({list(h3(HTML(paste('Data set', geex.html.msg(ds$longname)))), br(), br()); });
    }
  });
  observeEvent(input$expr.select.all, {
    if (!input$expr.select.all) {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns[1], inline=FALSE);            
      updateCheckboxGroupInput(session, 'expr.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno', choice=expr_anno_columns, selected=expr_anno_columns[c(1, length(expr_anno_columns))], inline=TRUE)   
    } else {
      updateCheckboxGroupInput(session, 'expr.column', NULL, choice=expr_columns, selected=expr_columns, inline=FALSE);        
      updateCheckboxGroupInput(session, 'expr.desc', choice=GetRowStatTypes(), selected=GetRowStatTypes(), inline=TRUE);
      updateCheckboxGroupInput(session, 'expr.anno', choice=expr_anno_columns, selected=expr_anno_columns, inline=TRUE)     
    }
  });
  
  output$expr.title<-renderUI({ list(h2("Data set"), HTML('Gene expression data matrix and descriptive statistics.')) });   
  output$expr.message<-renderUI({ if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else if (input$expr.dataset=='') list(h3(msg.nodataset), br(), br()) });
  output$expr.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(NA, cll))  geex.empty.matrix("No loaded data collection") else {
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
          if (length(gn) == 0) geex.empty.matrix("No gene of selected gene set found in data set") else tbl[gn, , drop=FALSE];
        }         
      }}
  }, options = dt.options1, filter='bottom', selection='none', rownames=FALSE, server=TRUE, escape = FALSE);
  
  }
  
  ########################################## "Gene-Data set" tab ###############################################      
  {
  combine.dataset<-reactive({   
    cll<-load.coll();
    if (identical(NA, cll)) geex.empty.matrix("No loaded data collection") else {
      geex.combined.table(cll, input$comb.species, input$comb.scale);
    }
  });

  observeEvent(input$comb.filter, {
    if (input$comb.filter) {
      cll<-load.coll();
      sp<-names(geneset[[1]][[1]]);
      sp<-union(sp[sp %in% cll$gene$Species], 'human');
      output$comb.ui <- renderUI({ wellPanel(fluidRow(
        column(3, br(),
               selectizeInput("comb.gs.source", "Source", names(geneset), selected=names(geneset)[1], width='80%'), 
               selectizeInput("comb.gs.coll", "Collection", names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
               selectizeInput("comb.gs.species", "Species", sp, selected='human', width='80%'), br(),
               column(2, h1(""), column(10, actionButton("comb.clear", 'Clear selection')))), 
        column(9, DT::dataTableOutput('comb.gs.table')))) }) 
    } else { output$comb.ui <- renderUI({br()}) }
  });
  
  observeEvent(input$comb.gs.source, { updateSelectizeInput(session, 'comb.gs.coll', choices=names(geneset[[input$comb.gs.source]]))});  
  observeEvent(input$comb.gs.coll, { 
    cll<-load.coll();
    sp<-names(geneset[[input$comb.gs.source]][[input$comb.gs.coll]]);
    if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
    updateSelectizeInput(session, 'comb.gs.species', choices=sp, selected='human'); 
  });
  observeEvent(input$comb.gs.species, { 
    output$comb.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$comb.gs.clear, { 
    output$comb.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  
  output$comb.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(NA, cll))  geex.empty.matrix("No loaded data collection") else { 
      tbl<-combine.dataset(); 
      if (colnames(tbl)[1] != 'ID') tbl else {
        cmn<-c('ID', input$comb.anno, 'N_Set', input$comb.desc, 'Mx-Mn', rownames(cll$metadata$Dataset));
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
  
  ########################################## "Gene summary" tab ###############################################      
  {  
  observeEvent(input$gene.filter, {
    if (input$gene.filter) {
      cll<-load.coll();
      sp<-names(geneset[[1]][[1]]);
      sp<-union(sp[sp %in% cll$gene$Species], 'human');
      output$gene.ui <- renderUI({ wellPanel(fluidRow(
        column(3, br(),
               selectizeInput("gene.gs.source", "Source", names(geneset), selected=names(geneset)[1], width='80%'), 
               selectizeInput("gene.gs.coll", "Collection", names(geneset[[1]]), selected=names(geneset[[1]])[1], width='80%'),
               selectizeInput("gene.gs.species", "Species", sp, selected='human', width='80%'), br(),
               column(2, h1(""), column(10, actionButton("gene.clear", 'Clear selection')))), 
        column(9, DT::dataTableOutput('gene.gs.table')))) }) 
      } else { output$gene.ui <- renderUI({br()}) }
  });
  
  observeEvent(input$gene.gs.source, { updateSelectizeInput(session, 'gene.gs.coll', choices=names(geneset[[input$comb.gs.source]]))});  
  observeEvent(input$gene.gs.coll, { 
    cll<-load.coll();
    sp<-names(geneset[[input$gene.gs.source]][[input$gene.gs.coll]]);
    if (!identical(NA, cll)) sp<-union(sp[sp %in% as.vector(cll$gene$Species)], 'human');
    updateSelectizeInput(session, 'gene.gs.species', choices=sp, selected='human'); 
  });
  observeEvent(input$comb.gs.species, { 
    output$comb.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});
  observeEvent(input$comb.clear, { 
    output$comb.gs.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$comb.gs.source, input$comb.gs.coll, input$comb.gs.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)});

  
  output$gene.title<-renderUI({ list(h2("Single gene information"), HTML('Summarize the descriptive statistics of a single gene across data sets.')) }); 
  
  output$gene.message<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(h3(msg.nocollection), br()) else {
      rid0<-input$gene.table_row_last_clicked; 
      rid1<-input$gene.table_rows_selected; 
      if (length(rid1)>0 & length(rid0)>0)  
        if (rid0[1]==rid1[length(rid1)]) geex.html.gene.name(rid0, TRUE, cll) else list(h3(msg.nogene), br()) else list(h3(msg.nogene), br())
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
        if (length(rid)>0) geex.empty.matrix("No more genes after filtering") else geex.empty.matrix("No gene information in data set")
      } else {
        data.frame(ID=AddHref(rownames(anno), UrlEntrezGene(rownames(anno))), Name=as.vector(anno$Symbol), Species=as.vector(anno$Species),
                   Type=sub('^protein-coding', 'coding', as.vector(anno$type_of_gene)), N_Set=anno$Num_Dataset, 
                   Synonyms=as.vector(anno$Synonyms), Description=as.vector(anno$description), stringsAsFactors=FALSE);
      }
    } else geex.empty.matrix("Empty table")
  }, options = dt.options2, filter='bottom', selection='single', rownames=FALSE, escape = FALSE);
  
  output$gene.stat.table <- DT::renderDataTable({
    cll<-load.coll();
    rid0<-input$gene.table_row_last_clicked; 
    rid1<-input$gene.table_rows_selected;
    if (length(rid1)>0 & length(rid0)>0)  if (rid0[1] %in% rid1) geex.summarize.gene(cll, rid0) else geex.empty.matrix("No selected gene") else
      geex.empty.matrix("No selected gene"); 
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
    }
  });
  
  output$pca.title<-renderUI({list(
    h2("Principal Component Analysis"), 
    HTML('Unsupervised clustering of samples in the same data set by <a href="https://en.wikipedia.org/wiki/Principal_component_analysis" target="_blank">PCA</a>')) });  
  output$pca.message<-renderUI({ if (identical(NA, load.coll())) list(h3(HTML(msg.nocollection)), br(), br())  else list(h3(HTML(paste("Data set", geex.html.msg(input$pca.dataset)))), br(), br()) })
  output$pca.plot <- renderPlot({ geex.plot.pca(load.coll(), input$pca.x, input$pca.y, input$pca.color, input$pca.dataset, input$pca.group, input$pca.table_rows_selected); }, height = 600, width = 800);
  output$pca.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(cll, NA)) geex.empty.matrix("Empty table") else cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], c('Name', 'Group'), drop=FALSE];
  }, options = dt.options3, escape = FALSE);
  }
  
  ########################################################################################
  ######################################## "Bar Plot" tab ################################ 
  {
  observeEvent(input$bar.dataset, {
    ds<-geex.load.dataset(load.coll(), input$bar.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'bar.group', choices=names(ds$group), selected=names(ds$group));   
    }  
    if (!input$bar.selected) {
      output$bar.table <- DT::renderDataTable({ geex.plot.bar.table(load.coll(), ds); }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);     
    } else {
      rid<-input$bar.table_rows_all;
      output$bar.table <- DT::renderDataTable({ geex.plot.bar.table2(ds, rid) }, options = dt.options3, selection='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    }});

  observeEvent(input$bar.selected, {
    if (!identical(NA, cll<-load.coll())) {
      if (!input$bar.selected) {
        ds<-geex.load.dataset(load.coll(), input$bar.dataset);
        isolate({ output$bar.table <- DT::renderDataTable({ geex.plot.bar.table(cll, ds);}, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE); });     
        updateSelectInput(session, 'bar.dataset', choices=cll$extra$longname$dataset, selected=ds$longname);
        if (!identical(NA, cll)) updateSelectInput(session, 'bar.dataset', choices=cll$extra$longname$dataset, selected=ds$longname);
      } else {
        ds<-geex.load.dataset(load.coll(), input$bar.dataset);
        rid<-input$bar.table_rows_selected;
        isolate({output$bar.table <- DT::renderDataTable({ geex.plot.bar.table2(ds, rid) }, options = dt.options3, selection='none', rownames=FALSE, server=TRUE, escape = FALSE); })     
        if (length(rid) > 0) {
          gex<-cll$gex_combined[[1]][CleanHtmlTags(rid), , drop=FALSE]; 
          gex<-gex[, grep('^D', colnames(gex)), drop=FALSE];
          dss<-colnames(gex)[!is.na(colMeans(gex))];
          if (!identical(NA, cll)) updateSelectInput(session, 'bar.dataset', choices=as.vector(cll$mapping$id2longname[dss]), selected=ds$longname);}}} 
  });
  
  observeEvent(input$bar.clear, {
    if (!identical(NA, cll<-load.coll())) {
      if (input$bar.selected) updateCheckboxInput(session, "bar.selected", value=FALSE) else {
        output$bar.table <- DT::renderDataTable({ geex.load.dataset(cll, input$bar.dataset)$anno; }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE); }; } 
  });
  
  observeEvent(input$lookup.key, {
    if (input$lookup.key!='' & !is.null(input$lookup.key)) {
      output$lookup.table <- DT::renderDataTable({
        geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$lookup.species, input$lookup.key, GENE_HOME); 
      }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE)   
    } else geex.empty.matrix('No search key');
  });
  
  observeEvent(input$lookup.option, {
    cll<-load.coll();
    if (input$lookup.option & !identical(NA, cll)) {
      output$lookup.table<-DT::renderDataTable({ geex.empty.matrix('No search key'); }, options=list(dom='t'))
      output$lookup.key.ui<-renderUI({ textInput("lookup.key", "", '') });
      output$lookup.species.ui<-renderUI({ selectizeInput("lookup.species", '', choices=unique(cll$gene$Species), selected='human') });
      output$lookup.table.ui<-renderUI({ DT::dataTableOutput('lookup.table') });
    } else { 
      updateTextInput(session, "lookup.key", NULL, NULL);
      updateSelectizeInput(session, "lookup.species", NULL, NULL, NULL);
      output$lookup.key.ui <- renderUI({br()});
      output$lookup.species.ui <- renderUI({br()}); 
      output$lookup.table.ui <- renderUI({br()}); 
    }
  }); 

  output$bar.title<-renderUI({ list(
    h2("Expression level of individual gene(s)"), 
    HTML("Compare expression levels between genes or/and sample groups.")); });
  output$bar.message<-renderUI({ 
    if (input$bar.selected) rid<-input$bar.table_rows_all else rid<-input$bar.table_rows_selected;
    geex.html.gene.name(rid, input$bar.selected, load.coll()); 
  });
  output$bar.plot <- renderPlot({  
    if (!input$bar.selected) rid<-input$bar.table_rows_selected else rid<-input$bar.table_rows_all;
    ds<-geex.load.dataset(load.coll(), input$bar.dataset); 
    geex.plot.bar(ds, rid, input$bar.selected, input$bar.scale, input$bar.color, input$bar.group, input$bar.mean);  
  });
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
    output$geneset.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
    }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
    output$geneset.stat <- DT::renderDataTable({ geex.empty.matrix('No selected gene set.'); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$geneset.clear, { 
    output$geneset.table <- DT::renderDataTable({ geex.geneset.table(geneset, input$geneset.source, input$geneset.coll, input$geneset.species);  
    }, options = dt.options3, rownames=FALSE, selection='single', server=TRUE, escape = FALSE) 
    output$geneset.stat <- DT::renderDataTable({ geex.empty.matrix('No selected gene set.'); }, options=list(dom='t'), selection='none');
  });
  
  output$geneset.title<-renderUI({ list(
    h2("Expression pattern of a gene set"),
    HTML('Compare and visualize overall expression levels of a gene set between sample groups.')) });
  output$geneset.message<-renderUI({
    if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()) else
      if (length(input$geneset.table_rows_selected) == 0) list(h3(msg.nogeneset), br(), br()) else {
        rid<-CleanHtmlTags(input$geneset.table_rows_selected);
        nm<-as.vector(geneset[[input$geneset.source]][[input$geneset.coll]][[input$geneset.species]][rid[length(rid)], 'Name'])
        list(h3(HTML(paste('Gene set', geex.html.msg(nm)))), br(), br());
      }
  });
  
  output$geneset.plot <- renderPlot({
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
      out<-geex.plot.geneset(load.coll(), grp, input$geneset.type, input$geneset.scale, input$geneset.color, input$geneset.normalize, gs[[1]]);  
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
    output$two.message<-renderUI({ list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])}, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
  });
  observeEvent(input$y.group, { 
    output$two.message<-renderUI({ list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br()); });    
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])}, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
  });
  
  observeEvent(input$two.source, { 
    updateSelectizeInput(session, 'two.coll', choices=names(geneset[[input$two.source]])); 
  });
  observeEvent(input$two.coll, { 
    sp<-names(geneset[[input$two.source]][[input$two.coll]]);
    if (!identical(NA, load.coll())) sp<-unique(c(sp[sp %in% geex.get2group.species(load.coll(), input$x.group, input$y.group)], 'human'));
    updateSelectizeInput(session, 'two.species', choices=sp); 
  });
  observeEvent(input$two.species, { 
    output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
                                                      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
    output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix('No selected gene set.'); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$two.clear.geneset, { 
    output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
    }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
    output$two.geneset.stat <- DT::renderDataTable({ geex.empty.matrix('No selected gene set.'); }, options=list(dom='t'), selection='none');
  });
  observeEvent(input$two.clear, { 
    output$two.table <- DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, input$two.select.species)}, 
                                            options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
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
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp<-input$two.select.species)}, 
                                          options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
  });
  
  output$two.title<-renderUI({list(
    h2("Differential expression between two groups"),
    HTML('Compare expression levels of 2 sample groups at 3 levels: global, gene set, and single gene.'))});
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
    xgrp<-input$x.group;
    ygrp<-input$y.group;
    if (xgrp!='' & ygrp!='') out<-geex.plot.two(load.coll(), xgrp, ygrp, input$two.type, input$two.scale, input$two.color, spe, input$two.table_rows_selected, gs);  
    if (length(gs) == 0) output$two.geneset.stat<-DT::renderDataTable({ geex.empty.matrix('No selected gene set.'); }, options=list(dom='t'), selection='none') else
      output$two.geneset.stat<-DT::renderDataTable({ out$geneset.stat }, options=dt.options5, rownames=FALSE, selection='none', server=TRUE, escape = FALSE); 
  });
  }
});


