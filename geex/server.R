print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");
  
  ###################################################################################################
  # Load the collection data
  load.coll<-reactive({ 
    if (input$select.collection == '') NA else {
      coll.loaded<-geex.load.collection(input$select.collection, GEX_HOME);

      updateSelectInput(session, 'browse.options', choices=names(coll.loaded$browse_table));
      
      longname<-as.vector(coll.loaded$mapping$id2longname[rownames(coll.loaded$metadata$Dataset)]);
      updateSelectInput(session, 'pca.dataset', choices=longname);
      updateSelectInput(session, 'x.dataset', choices=longname);
      updateSelectInput(session, 'y.dataset', choices=longname);
      
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
    if (identical(NA, cll)) list(h3(msg.noload), br()) else {
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
    } else geex.empty.matrix()
  }, 
  options = list(autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 10), filter='bottom', rownames=FALSE, escape = FALSE);

  ###################################################################################################
  ######################################## "Visualization" menu #####################################
  ###################################################################################################

  ########################################################################################
  ######################################## "PCA Plot" tab ################################
  output$pca.title<-renderUI({h2("Principal Component Analysis") });
  
  output$pca.message<-renderUI({
    if (identical(NA, load.coll())) list(h3(msg.noload), br(), br()) else 
      list(h3(HTML(paste("Data set", geex.html.msg(input$pca.dataset)))), br(), br())
  });
  
  output$pca.plot <- renderPlot({
    geex.plot.pca(load.coll(), input$pca.dataset, input$pca.table_rows_selected);
  }, height = 480, width = 640);
  
  output$pca.table <- DT::renderDataTable({
    cll<-load.coll();
    if (!identical(cll, NA)) {
      ds.nm<-input$pca.dataset;
      smp<-cll$metadata$Sample;
      smp[smp$Dataset == strsplit(ds.nm, ': ')[[1]][1], c('Name', 'Group'), drop=FALSE];
    } else geex.empty.matrix("Empty table")
  }, options = list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 8), escape = FALSE);
  
  ########################################################################################
  ######################################## "Scatter Plot" tab ############################
  observe({
    ch.x<-geex.longname.set2groups(load.coll(), input$x.dataset);
    updateSelectizeInput(session, 'x.group', choices=ch.x);
    
    ch.y<-geex.longname.set2groups(load.coll(), input$y.dataset);
    updateSelectizeInput(session, 'y.group', choices=ch.y, selected=ch.y[length(ch.y)]);
  })

  
  output$scatter.title<-renderUI({h2("Compare gene expression level") });
  
  output$scatter.message<-renderUI({
    if (identical(NA, load.coll())) list(h3(msg.noload), br(), br()) else 
      list(h3(HTML(paste(geex.html.msg(input$x.group), 'vs.', geex.html.msg(input$y.group)))), br(), br())
  });
  
  output$scatter.table <- DT::renderDataTable({
    if (input$scatter.show.gene) geex.get2group(load.coll(), input$x.group, input$y.group) else geex.empty.matrix('Empty table');
  }, options = list(autoWidth = TRUE, caseInsensitve = TRUE, regex = TRUE, pageLength = 15), server=TRUE, filter='bottom', escape = FALSE);
  
  output$scatter.plot <- renderPlot({
    geex.plot.scatter(load.coll(), input$x.group, input$y.group, input$scatter.table_rows_selected);  
  }, height = 640, width = 640);
})

