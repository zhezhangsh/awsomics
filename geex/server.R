print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");
  
  ###################################################################################################
  # Load the collection data
  load.coll<-reactive({ 
    if (input$select.collection == '') NA else {
      coll.loaded<-geex.load.collection(input$select.collection, GEX_HOME);

      updateSelectInput(session, 'browse.options', choices=names(coll.loaded$browse_table));

      updateSelectInput(session, 'pca.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'bar.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'x.dataset', choices=coll.loaded$extra$longname$dataset);
      updateSelectInput(session, 'y.dataset', choices=coll.loaded$extra$longname$dataset);
             
      coll.loaded;
    }
  });
  
  ###################################################################################################
  ######################################## "Home" tab #############################################
  # table title
  output$home.title<-renderUI({
    list(h1('Welcome to Awsomics: GeEx'), h4(HTML("<u>G</u>ene <u>e</u>xpression <u>Ex</u>plorer, demo version")));
  });

  # Search table message
  output$home.message<-renderUI({ 
    if (identical(NA, load.coll())) list(br(), br(), h3("Transcriptome data collections"), br()) else 
      list(br(), br(), h3(HTML(paste("Loaded data collection", geex.html.msg(load.coll()$selection)))), br())
  });

  output$home.table <- DT::renderDataTable({
    data.frame(ID=rownames(coll), coll, stringsAsFactors=FALSE)
  }, options=list(dom = 't'), rownames=FALSE, escape=FALSE);
  ###################################################################################################
  
  ###################################################################################################
  ######################################## "Browse" tab #############################################
  output$browse.title<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(h3(msg.nocollection), br()) else {
      list(h3(HTML(paste('Data collection', geex.html.msg(load.coll()$selection)))), br());
    }      
  });
  
  output$browse.message<-renderUI({
    cll<-load.coll();
    if (identical(NA, cll)) list(br()) else list(cll$message, br(), br())
  })
  
  output$browse.table <- DT::renderDataTable({
    cll<-load.coll();
    if (!identical(cll, NA)) {
      tbl.nm<-input$browse.options;
      if (tbl.nm=='' | is.na(tbl.nm)) cll$browse_table[[1]] else cll$browse_table[[tbl.nm]];    
    } else geex.empty.matrix("Empty table")
  }, 
  options = dt.options1, filter='bottom', rownames=FALSE, escape = FALSE);

  ###################################################################################################
  ######################################## "Analysis" menu ##########################################
  ###################################################################################################

  ########################################################################################
  ######################################## "PCA Plot" tab ################################
  observeEvent(input$pca.dataset, {
    ds<-geex.load.dataset(load.coll(), input$pca.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'pca.group', choices=names(ds$group), selected=names(ds$group));
      updateSelectizeInput(session, 'pca.x', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC1');
      updateSelectizeInput(session, 'pca.y', choices=paste('PC', 1:ncol(ds$data[[1]]), sep=''), selected='PC2');
    }
  });
  
  output$pca.title<-renderUI({h2("Principal Component Analysis") });  
  output$pca.message<-renderUI({ if (identical(NA, load.coll())) list(h3(HTML(msg.nocollection)), br(), br())  else list(h3(HTML(paste("Data set", geex.html.msg(input$pca.dataset)))), br(), br()) })
  output$pca.plot <- renderPlot({ geex.plot.pca(load.coll(), input$pca.x, input$pca.y, input$pca.color, input$pca.dataset, input$pca.group, input$pca.table_rows_selected); }, height = 600, width = 800);
  output$pca.table <- DT::renderDataTable({
    cll<-load.coll();
    if (identical(cll, NA)) geex.empty.matrix("Empty table") else cll$metadata$Sample[cll$metadata$Sample$Dataset == strsplit(input$pca.dataset, ': ')[[1]][1], c('Name', 'Group'), drop=FALSE];
  }, options = dt.options3, escape = FALSE);

  ########################################################################################
  ######################################## "Bar Plot" tab ################################ 
  observeEvent(input$bar.dataset, {
    ds<-geex.load.dataset(load.coll(), input$bar.dataset);
    if (!identical(NA, ds)) {
      updateCheckboxGroupInput(session, 'bar.group', choices=names(ds$group), selected=names(ds$group));   
      updateSelectInput(session, 'lookup.species', choices=unique(ds$anno$Species), selected=ds$anno$Species[1])
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
          if (!identical(NA, cll)) updateSelectInput(session, 'bar.dataset', choices=as.vector(cll$mapping$id2longname[dss]), selected=ds$longname);
        }
      }
    } 
  });
  
  observeEvent(input$bar.clear, {
    if (!identical(NA, cll<-load.coll())) {
      if (input$bar.selected) updateCheckboxInput(session, "bar.selected", value=FALSE) else {
        output$bar.table <- DT::renderDataTable({ geex.load.dataset(cll, input$bar.dataset)$anno; }, options = dt.options3, rownames=FALSE, server=TRUE, escape = FALSE);             
      };
    } 
  });
  
  observeEvent(input$lookup.key, {
    output$lookup.table <- DT::renderDataTable({
      geex.lookup.gene(geex.load.dataset(load.coll(), input$bar.dataset)$anno, input$lookup.species, input$lookup.key, GENE_HOME); 
    }, options = dt.options4, selection='none', rownames=FALSE, server=TRUE, escape = FALSE)
  })
  
  output$bar.title<-renderUI({ h2("Expression level of individual gene(s)"); });
  output$bar.message<-renderUI({ 
    if (input$bar.selected) rid<-input$bar.table_rows_all else rid<-input$bar.table_rows_selected;
    geex.html.gene.name(rid, input$bar.selected, load.coll()); 
  });
  output$bar.plot <- renderPlot({  
    if (!input$bar.selected) rid<-input$bar.table_rows_selected else rid<-input$bar.table_rows_all;
    ds<-geex.load.dataset(load.coll(), input$bar.dataset); 
    geex.plot.bar(ds, rid, input$bar.selected, input$bar.scale, input$bar.color, input$bar.group, input$bar.mean);  
  });

  ###########################################################################################
  ######################################## "Compare two tab" tab ############################
  observeEvent(input$x.dataset, {
    ch.x<-geex.longname.set2groups(load.coll(), input$x.dataset);
    updateSelectizeInput(session, 'x.group', choices=ch.x);
  });
  observeEvent(input$y.dataset, {
    ch.y<-geex.longname.set2groups(load.coll(), input$y.dataset);
    updateSelectizeInput(session, 'y.group', choices=ch.y, selected=ch.y[length(ch.y)]);
  });
  
  observeEvent(input$x.group, {
    output$two.message<-renderUI({ list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br(), br()); });
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])}, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
  });
  observeEvent(input$y.group, { 
    output$two.message<-renderUI({ list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br(), br()); });    
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group); 
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp[1])}, options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
    updateSelectizeInput(session, "two.select.species", choices=sp);
  });
  
  observeEvent(input$two.source, { 
    updateSelectizeInput(session, 'two.coll', choices=names(geneset[[input$two.source]])); 
  });
  observeEvent(input$two.coll, { 
    sp<-names(geneset[[input$two.source]][[input$two.coll]]);
    if (!identical(NA, load.coll())) sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group);
    #sp0<-input$two.select.species;
    #if (sp0[1] %in% sp) sp1<-sp0 else sp1<-'human';
    updateSelectizeInput(session, 'two.species', choices=sp); 
  });
  observeEvent(input$two.species, { 
    output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  
                                                      }, options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE) 
  });
  observeEvent(input$two.clear.geneset, { 
    output$two.table.geneset <- DT::renderDataTable({ geex.geneset.table(geneset, input$two.source, input$two.coll, input$two.species);  }, 
                                                    options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE); 
  });
  observeEvent(input$two.clear, { 
    output$two.table <- DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, input$two.select.species)}, 
                                            options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
  });
  observeEvent(input$two.select.species, { 
    # if (input$two.select.species %in% names(geneset[[input$two.source]][[input$two.coll]])) sp<-input$two.select.species else sp<-input$two.species;
    sp<-geex.get2group.species(load.coll(), input$x.group, input$y.group);
    sp1<-names(geneset[[input$two.source]][[input$two.coll]]);
    if (!setequal(sp, sp1) | input$two.select.species!=input$two.species) {
      sp2<-sp1[sp1 %in% sp];
      sp3<-input$two.select.species;
      if (!(sp3 %in% sp2)) sp3<-'human'
      updateSelectizeInput(session, 'two.species', sp2, sp3); 
    }
    output$two.table<-DT::renderDataTable({ geex.get2group(load.coll(), input$x.group, input$y.group, sp<-input$two.select.species)}, 
                                          options=dt.options2, rownames=FALSE, filter='bottom', server=TRUE, escape = FALSE); 
  });
  
  
  output$two.title<-renderUI({h2("Differential expression between two groups") });
  output$two.message<-renderUI({if (identical(NA, load.coll())) list(h3(msg.nocollection), br(), br()); });
  output$two.table <- DT::renderDataTable({ geex.empty.matrix('Empty table'); });

  output$two.plot <- renderPlot({
    gs<-list();
    if (length(input$two.table.geneset_rows_selected) == 0) spe<-input$two.select.species else {
      spe<-input$two.species;
      id<-CleanHtmlTags(input$two.table.geneset_rows_selected); 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$two.source), '_list.rds', sep=''))[id];
      names(gs)<-as.vector(geneset[[input$two.source]][[input$two.coll]][[spe]][names(gs), 'Name']);
    }
    geex.plot.two(load.coll(), input$x.group, input$y.group, input$two.type, input$two.scale, spe, input$two.table_rows_selected, gs);  
  }, height = 640, width = 640);
});


