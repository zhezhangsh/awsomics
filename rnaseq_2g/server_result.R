server_result <- function(input, output, session, session.data) {
  
  session.dir  <- paste(APP_HOME, 'log', session$token, sep='/');
  if (!file.exists(session.dir)) dir.create(session.dir, recursive = TRUE);
  
  ###################################################################################################
  ######################################## "Result" tab #############################################  
  observeEvent(input$result.previous.load, {
    id <- input$result.previous.id;
    fn <- paste(APP_HOME, 'log', id, 'results.rds', sep='/');
    if (!file.exists(fn)) msg <- paste('<font color="darkblue";>', 'Analysis not found on server:', '</font>', id) else {
      session.data$result <- res <- readRDS(fn); 
      
      msg <- paste('Results loaded from server have', nrow(res$input$original), 'genes,', ncol(res$input$original), 
                   'samples, and', length(res$output), 'methods.');
      
      tbl <- as.list(names(res[[2]])); 
      names(tbl) <- DeRNAseqMs[names(res[[2]]), 1];
      updateSelectInput(session, 'result.select.table', label = "Select method", choices = tbl); 
      updateSelectInput(session, 'compare.select.table1', label = "Select method", choices = tbl, selected = tbl[[1]]);
      updateSelectInput(session, 'compare.select.table2', label = "Select method", choices = tbl, selected = tbl[[length(tbl)]]);
    }
    
    output$result.previous.info <- renderUI(HTML("&nbsp&nbsp&nbsp&nbsp", msg));
  });
  
  # Upload previous results from local file
  observeEvent(input$result.previous.upload, {
    if (is.null(input$result.previous.upload)) {
      msg <- '<font color="darkblue";>No file uploaded.</font>';
    } else {
      uploaded <- input$result.previous.upload; 
      if (file.exists(uploaded$datapath)) {
        fn.res  <- paste(session.dir, uploaded$name, sep='/');
        file.copy(uploaded$datapath, fn.res); 
        
        res <- ImportR(fn.res); 
        if (identical(NA, res)) msg <- paste('<font color="darkblue";>Fail to upload file:</font>', uploaded$name) else {
          msg <- rnaseq2g.validate.result(res); # validate uploaded file
          if (identical(NA, msg)) {
            session.data$result <- res; 
            msg <- paste('<font color="darkblue";>Results loaded from local file have', nrow(res$input$original), 'genes,', 
                         ncol(res$input$original), 'samples, and', length(res$output), 'methods.</font>');
            tbl <- as.list(names(res[[2]])); 
            names(tbl) <- DeRNAseqMs[names(res[[2]]), 1];
            updateSelectInput(session, 'result.select.table', label = "Select method", choices = tbl); 
            updateSelectInput(session, 'compare.select.table1', label = "Select method", choices = tbl, selected = tbl[[1]]);
            updateSelectInput(session, 'compare.select.table2', label = "Select method", choices = tbl, selected = tbl[[length(tbl)]]);
          } else msg <- paste('<font color="darkblue";>', msg, '</font>', sep=''); 
        }
      } else {
        msg <- paste('<font color="darkblue";>File to upload not found:</font>', uploaded$name);
      } 
    };
    output$result.previous.info <- renderUI(HTML("&nbsp&nbsp&nbsp&nbsp", msg));
  });
  
  # Show stat table
  output$result.show.table <- DT::renderDataTable({
    res <- session.data$result;
    if (is.null(res) | input$result.select.table=='') NULL else {
      tbl <- res[[2]][[input$result.select.table]];
      pv  <- as.numeric(input$result.filter.p);
      fc  <- as.numeric(input$result.filter.fc);
      tbl <- tbl[tbl[, 5]<=pv & abs(tbl[,4])>=fc, , drop=FALSE]
      tbl <- FormatNumeric(tbl[order(tbl[, 5]), , drop=FALSE]);
      tbl;
    }
  }, options = dt.options3, selection = 'none', rownames=TRUE);
  
  # Download results single table
  output$result.download.current <- downloadHandler(
    filename = function() {
      fmt <- input$result.download.format;
      mtd <- DeRNAseqMs[input$result.select.table, 1]; 
      if (fmt == 'R') paste(mtd, '.Rdata', sep='') else 
        if (fmt == 'Text') paste(mtd, '.txt', sep='') else 
          paste(mtd, '.xls', sep='')
    },  
    content  = function(file) { 
      res <- session.data$result;
      if (!is.null(res)) {
        tbl <- res[[2]][[input$result.select.table]];
        pv  <- as.numeric(input$result.filter.p);
        fc  <- as.numeric(input$result.filter.fc);
        tbl <- tbl[tbl[, 5]<=pv & abs(tbl[,4])>=fc, , drop=FALSE];
        fmt <- input$result.download.format;
        if (fmt == 'Excel') WriteXLS::WriteXLS(data.frame(tbl), ExcelFileName=file, SheetNames=input$result.select.table, row.names = TRUE) else 
          if (fmt == 'Text') write.table(tbl, file, sep='\t', quote = FALSE) else 
            if (fmt == 'R') save(tbl, file=file);
      }
    } 
  ); # END of download single table
  
  # Download all results
  output$result.download.all <- downloadHandler(
    filename = function() {
      fmt <- input$result.download.format;
      if (fmt == 'R') 'results.Rdata' else if (fmt == 'Text') 'results.txt' else 'results.xls' 
    },  
    content  = function(file) {
      res <- session.data$result;
      if (!is.null(res)) {
        fmt <- input$result.download.format;
        if (fmt == 'R') save(res, file=file) else
          if (fmt == 'Text') {
            tbl <- do.call('rbind', res[[2]]); 
            tbl <- data.frame(Gene=unlist(lapply(res[[2]], rownames), use.names=FALSE), tbl, 
                              Method=rep(names(res[[2]]), sapply(res[[2]], nrow)), 
                              stringsAsFactors = FALSE);
            write.table(tbl, file, sep = '\t', quote = FALSE, row.names = FALSE);
          } else
            if (fmt == 'Excel') {
              tbls <- res[[1]][1:2]; 
              tbls$normalized.count  <- res[[1]]$normalized$count;
              tbls$normalized.logged <- res[[1]]$normalized$logged;
              tbls <- c(tbls, res[[2]]);
              tbls <- lapply(tbls, as.data.frame);
              WriteXLS::WriteXLS(tbls, ExcelFileName=file, SheetNames=names(tbls), row.names = TRUE);
            }
      }
    } 
  ); # END of download all results
  
  # Plots
  output$result.show.plot <- renderPlot({
    rnaseq2g.plot.global(session.data$result, input$result.select.table, input$result.select.plot);
  });
  
  # Download plot
  output$result.download.plot <- downloadHandler(
    filename = function() {
      nm <- c('1'='MA', '2'='Volcano', '3'='Pvalue', '4'='FDR')[input$result.select.plot];
      paste(nm, input$result.download.plottype, sep='\\.');
    },
    content  = function(file) {
      typ <- input$result.download.plottype;
      if (!is.null(typ) & typ=='pdf') pdf(file, width=4.8, height = 6) else 
        if (!is.null(typ) & typ=='png') png(file, width=4.8, height=6, unit='in', res=300) else
          if (!is.null(typ) & typ=='jpeg') jpeg(file, width=4.8, height=6, unit='in', res=300) else
            if (!is.null(typ) & typ=='tiff') tiff(file, width=4.8, height=6, unit='in', res=300);
      rnaseq2g.plot.global(session.data$result, input$result.select.table, input$result.select.plot);
      dev.off();
    }
  );
  
  session.data;
}
