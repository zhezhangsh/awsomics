server_compare <- function(input, output, session, session.data) {
  
  session.dir  <- paste(APP_HOME, 'log', session$token, sep='/');
  if (!file.exists(session.dir)) dir.create(session.dir, recursive = TRUE);
  
  #######################################################################################################
  ######################################## "Comparison" tab #############################################
  # Plot1
  output$compare.show.plot1 <- renderPlot({
    rnaseq2g.plot.global(session.data$result, input$compare.select.table1, input$compare.select.plot1);
  });
  # Plot2
  output$compare.show.plot2 <- renderPlot({
    rnaseq2g.plot.global(session.data$result, input$compare.select.table2, input$compare.select.plot2);
  });
  
  # Concordant change of plot type
  observeEvent(input$compare.select.plot1, {
    updateSelectInput(session, 'compare.select.plot2', label = "Select plot", selected = input$compare.select.plot1);
  }); 
  observeEvent(input$compare.select.plot2, {
    updateSelectInput(session, 'compare.select.plot1', label = "Select plot", selected = input$compare.select.plot2);
  }); 
  
  # P value ranking table
  output$compare.table.pv <- DT::renderDataTable({
    res <- session.data$result;
    md1 <- input$compare.select.table1; 
    md2 <- input$compare.select.table2; 
    if (is.null(res) | md1=='' | md2=='') NULL else rnaseq2g.rank.pvalue(res, c(md1, md2), DeRNAseqMs);
  }, options = dt.options4, selection = 'none', rownames=TRUE);
  
  output$compare.plot.pv <- renderPlot({ 
    rnaseq2g.plot.pvalue(session.data$result, input$compare.select.table1, input$compare.select.table2, 
                         input$compare.pv.type, DeRNAseqMs);
  });
  
  # Single gene table
  output$compare.single.table <- DT::renderDataTable({
    gid <- input$compare.single.id; 
    res <- session.data$result; 
    if (is.null(res)) {
      output$compare.single.msg <- renderUI(HTML('<font color="darkblue";>No results available.</font>'));
      NULL;
    } else if (gid == '') {
      output$compare.single.msg <- renderUI(HTML('<font color="darkblue";>Enter gene ID.</font>'));
      NULL;
    } else if (!(gid %in% rownames(res$output[[1]]))) {
      output$compare.single.msg <- renderUI(HTML('<font color="darkblue";>Gene not found: </font>', gid));
      NULL;
    } else {
      output$compare.single.msg <- renderUI(HTML('<font color="darkblue";>Gene found: </font>', gid)); 
      tbl <- t(sapply(res[[2]], function(res) res[gid, ])); 
      cnm <- c('Method', colnames(tbl));
      tbl <- data.frame(Method=DeRNAseqMs[rownames(tbl), 1], FormatNumeric(tbl), stringsAsFactors = FALSE);
      colnames(tbl) <- cnm; 
      tbl
    }
  }, options = dt.options4, selection = 'none', rownames=FALSE);
  
  # Single gene barplot
  output$compare.single.plot <- renderPlot({
    rnaseq2g.plot.single(session.data$result, input$compare.single.id, input$compare.single.type);
  });
  
  session.data;
}
