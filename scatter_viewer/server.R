source('makeScatterplot.R',  local=TRUE);

shinyServer(function(input, output, session) {
  session.data <- reactiveValues(dir = 'temp', data = NULL, metadata = NULL);
  
  observeEvent(input$upload.data, {
    if (is.null(input$upload.data)) session.data$data <- NULL else {
      uploaded <- input$upload.data; 
      if (file.exists(uploaded$datapath)) {
        fn.mtrx  <- paste(session.data$dir, uploaded$name, sep='/');
        file.copy(uploaded$datapath, fn.mtrx); 
        session.data$data <- as.matrix(ImportTable(fn.mtrx)); 
        
        data <- session.data$data;
        axis <- 1:ncol(data); 
        if (is.null(colnames(data))) colnames(data) <- paste('Column', 1:ncol(data), sep='_');
        names(axis) <- colnames(data); 
        updateSelectizeInput(session, 'axis.x', choices=axis, selected='1'); 
        updateSelectizeInput(session, 'axis.y', choices=axis, selected='2'); 
      } else session.data$data <- NULL
    };
  });
  
  observeEvent(input$upload.metadata, {
    if (is.null(input$upload.metadata)) session.data$metadata <- NULL else {
      uploaded <- input$upload.metadata; 
      if (file.exists(uploaded$datapath)) {
        fn.mtrx  <- paste(session.data$dir, uploaded$name, sep='/');
        file.copy(uploaded$datapath, fn.mtrx); 
        session.data$metadata <- as.matrix(ImportTable(fn.mtrx)); 
        
        anno <- session.data$metadata;
        choi <- 0:ncol(anno); 
        names(choi) <- as.list(c('None', colnames(anno)));
        
        updateRadioButtons(session, 'primary', choices = choi, selected='1', inline=FALSE);
        updateRadioButtons(session, 'secondary', choices = choi, inline=FALSE);
      } else session.data$metadata <- NULL
    };
  });
  
  # Highlight primary
  observeEvent(input$primary, {
    if (!is.null(session.data$metadata)) {
      a <- as.character(as.vector(session.data$metadata[, as.integer(input$primary)]));
      u <- c('None', unique(a)); 
      c <- (1:length(u)) - 1;
      names(c) <- u;
      updateSelectizeInput(session, 'highlight.primary', choices=c); 
    }
  });
  
  # Highlight secondary
  observeEvent(input$secondary, {
    if (!is.null(session.data$metadata)) {
      a <- as.character(as.vector(session.data$metadata[, as.integer(input$secondary)]));
      u <- c('None', unique(a)); 
      c <- (1:length(u)) - 1;
      names(c) <- u;
      updateSelectizeInput(session, 'highlight.secondary', choices=c); 
    }
  });
  
  # Download plot
  output$download.button <- downloadHandler(
    filename = function() { paste('scatterplot', input$download.type, sep='\\.'); },
    content  = function(file) {
      typ <- input$download.type;
      if (!is.null(typ) & typ=='pdf') pdf(file, width=9.6, height = 6.4) else
        if (!is.null(typ) & typ=='png') png(file, width=9.6, height=6.4, unit='in', res=300) else
          if (!is.null(typ) & typ=='jpeg') jpeg(file, width=9.6, height=6.4, unit='in', res=300) else
            if (!is.null(typ) & typ=='tiff') tiff(file, width=9.6, height=6.4, unit='in', res=300);
      makeScatterPlot(input, output, session, session.data); 
      dev.off();
    }
  );
  
  output$scatterplot <- renderPlot({
    makeScatterPlot(input, output, session, session.data); 
  }, width=960, height=640)

})
