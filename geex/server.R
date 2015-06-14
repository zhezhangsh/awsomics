print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");

  ###################################################################################################
  # Load the collection data
  load.coll<-reactive({ 
    if (input$select.collection == '') NA else {
      coll.loaded<-geex.load.collection(input$select.collection, GEX_HOME);
      
      coll.loaded$small_table<-list()
      ds<-coll.loaded$browse_table[['Data set']];
      rownames(ds)<-as.vector(ds$ID);
      ds<-ds[, colnames(ds) %in% c('Name', 'GEO', 'PubMed', 'Num_Gene', 'Num_Group', 'Num_Sample', 'Species', 'Tissue')];
      coll.loaded$small_table$dataset<-ds;
      ds.longname<-paste(rownames(ds), ds$Name, sep=': ');
      
      updateSelectInput(session, 'browse.options', choices=names(coll.loaded$browse_table));
      updateSelectInput(session, 'pca.dataset', choices=c('', ds.longname));
      
      
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
      list(br(), br(), h3(HTML(paste("Loaded data collection <font color='#8888FF'>", load.coll()$selection, "</font>", sep=''))), br())
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
      list(h3(HTML(paste('Data collection <font color="#8888FF">', load.coll()$selection, '</font>', sep=''))), br());
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
    } else matrix('', nr=1, nc=01, dimnames=list('', 'Empty table'))
  }, 
  options = list(autoWidth = FALSE, caseInsensitve = TRUE, regex = TRUE, pageLength = 10), filter='bottom', rownames=FALSE, escape = FALSE);

  ###################################################################################################
  ######################################## "Visualization" menu #####################################
  ###################################################################################################

  ########################################################################################
  ######################################## "PCA" tab #####################################
  output$pca.title<-renderUI({h2("Principal Component Analysis") });
  
  output$pca.message<-renderUI({
    if (identical(NA, load.coll())) list(h3(msg.noload), br(), br()) else 
      list(h3(HTML(paste("Data set <font color='#8888FF'>", input$pca.dataset, '</font>', sep=''))), br(), br())
  });
  
  output$pca.plot <- renderPlot({
    if (input$pca.dataset != '') plot(1:20, pch=1:20)
  }, height=600, width=600);
})

