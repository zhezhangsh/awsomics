server_analysis <- function(input, output, session, session.data) {
  
  session.dir  <- paste(APP_HOME, 'log', session$token, sep='/');
  if (!file.exists(session.dir)) dir.create(session.dir, recursive = TRUE);
  
  ###################################################################################################
  ######################################## "Analysis" tab ###########################################  
  
  ###################################################################################################
  # Analysis, Step 1
  
  # Load data matrix
  observeEvent(input$analysis.step1.upload, {
    if (is.null(input$analysis.step1.upload)) session.data$matrix <- NULL else {
      uploaded <- input$analysis.step1.upload; 
      if (file.exists(uploaded$datapath)) {
        fn.mtrx  <- paste(session.dir, uploaded$name, sep='/');
        file.copy(uploaded$datapath, fn.mtrx); 
        session.data$matrix <- as.matrix(ImportTable(fn.mtrx)); 
      } else session.data <- NULL
    }
    
    # Table header
    output$analysis.step1.table <- DT::renderDataTable({
      if (is.null(session.data$matrix)) NULL else session.data$matrix[1:min(3, nrow(session.data$matrix)), , drop=FALSE];
    }, options = dt.options1, selection = 'none');
    
    # Table dimension
    output$analysis.step1.size <- renderUI({
      if (is.null(session.data$matrix)) '' else 
        list(h6(HTML(paste('The full matrix has', nrow(session.data$matrix), 'rows (genes) and', ncol(session.data$matrix), 'columns (samples).'))))
    });
  });
  
  # Download sample data
  output$analysis.step1.txt <- downloadHandler(
    filename = function() 'count_example.txt',  
    content  = function(file) write.table(count_example, file, sep='\t', quote = FALSE)
  );
  output$analysis.step1.csv <- downloadHandler(
    filename = function() 'count_example.csv',  
    content  = function(file) write.csv(count_example, file)
  );
  output$analysis.step1.rds <- downloadHandler(
    filename = function() 'count_example.rds',  
    content  = function(file) saveRDS(count_example, file)
  );
  output$analysis.step1.rda <- downloadHandler(
    filename = function() 'count_example.rda',  
    content  = function(file) save(count_example, file=file)
  );
  output$analysis.step1.rdata <- downloadHandler(
    filename = function() 'count_example.rdata',  
    content  = function(file) save(count_example, file=file)
  );
  output$analysis.step1.xls <- downloadHandler(
    filename = function() 'count_example.xls',  
    content  = function(file) WriteXLS::WriteXLS(data.frame(count_example), ExcelFileName=file, SheetNames='count_example', row.names = TRUE)
  );
  output$analysis.step1.xlsx <- downloadHandler(
    filename = function() 'count_example.xlsx',  
    content  = function(file) WriteXLS::WriteXLS(data.frame(count_example), ExcelFileName=file, SheetNames='count_example', row.names = TRUE)
  );
  output$analysis.step1.html <- downloadHandler(
    filename = function() 'count_example.html',  
    content  = function(file) print(xtable::xtable(count_example), type='html', file=file)
  );
  
  ###################################################################################################
  # Analysis, Step 2
  # Load sample grouping
  observeEvent(input$analysis.step2.group, {
    if (is.null(input$analysis.step2.group)) NULL else {
      uploaded <- input$analysis.step2.group; 
      
      if (file.exists(uploaded$datapath)) {
        fn.grps  <- paste(session.dir, uploaded$name, sep='/');
        file.copy(uploaded$datapath, fn.grps);
        grps <- ImportList(fn.grps); 
        updateTextInput(session, 'analysis.step2.groupA', label='', value=names(grps)[1]);
        updateTextInput(session, 'analysis.step2.groupB', label='', value=names(grps)[2]);
        updateTextInput(session, 'analysis.step2.sampleA', label='', value=paste(grps[[1]], collapse=';'));
        updateTextInput(session, 'analysis.step2.sampleB', label='', value=paste(grps[[2]], collapse=';'));
      } else NULL
    }
  });
  
  # Download sample groups
  output$analysis.step2.txt <- downloadHandler(
    filename = function() 'group_example.txt',  
    content  = function(file) {
      lns <- sapply(names(group_example), function(nm) 
        paste(nm, paste(group_example[[nm]], collapse='\t'), sep='\t'));
      writeLines(lns, file);
    }
  );
  output$analysis.step2.csv <- downloadHandler(
    filename = function() 'group_example.csv',  
    content  = function(file) {
      lns <- sapply(names(group_example), function(nm) 
        paste(nm, paste(group_example[[nm]], collapse=','), sep=','));
      writeLines(lns, file);
    }
  );
  output$analysis.step2.rds <- downloadHandler(
    filename = function() 'group_example.rds',  
    content  = function(file) saveRDS(group_example, file)
  );
  output$analysis.step2.rdata <- downloadHandler(
    filename = function() 'group_example.rdata',  
    content  = function(file) save(group_example, file=file)
  );
  output$analysis.step2.xls <- downloadHandler(
    filename = function() 'group_example.xls',  
    content  = function(file) {
      tbl <- rbind(group_example[[1]], group_example[[2]]);
      rownames(tbl) <- names(group_example); 
      WriteXLS::WriteXLS(data.frame(tbl), ExcelFileName=file, SheetNames='group_example', 
                         row.names = TRUE, col.names = FALSE);
    }
  );
  output$analysis.step2.xlsx <- downloadHandler(
    filename = function() 'group_example.xlsx',  
    content  = function(file) {
      tbl <- rbind(group_example[[1]], group_example[[2]]);
      rownames(tbl) <- names(group_example); 
      WriteXLS::WriteXLS(data.frame(tbl), ExcelFileName=file, SheetNames='group_example', 
                         row.names = TRUE, col.names = FALSE);
    }
  );
  ###################################################################################################
  # Analysis, Step 3
  # Select method(s)  
  observeEvent(input$analysis.step3.group, {
    sel <- which(method.group == input$analysis.step3.group) - 2; 
    if (sel >= 0 & !is.na(sel) & !is.null(sel)) mth <- DeRNAseqMethods(sel) else mth <- NULL; 
    
    updateCheckboxGroupInput(session, 'analysis.step3.selection', label = 'Select DE methods:', 
                             choices = DeRNAseqMs[[1]], selected = DeRNAseqMs[mth, 1], inline = TRUE); 
  });
  
  observeEvent(input$analysis.step3.button, {session.data$show <- 1 - session.data$show;});
  output$analysis.step3.panel1 <- renderUI({
    if (session.data$show == 0) l <- 'Show method description' else l <- 'Hide method description';
    actionLink('analysis.step3.button', label = l);
  }); 
  output$analysis.step3.panel2 <- renderUI({
    if (session.data$show == 0) br() else DT::dataTableOutput('analysis.step3.table', width='100%')
  });
  output$analysis.step3.table <- DT::renderDataTable({
    tbl <- DeRNAseqMs[, 1:8];
    #tbl[, 1] <- AddHref(tbl[, 1], DeRNAseqMs[, 10]); 
    tbl;
  }, options = dt.options2, selection = 'none',  rownames = FALSE);
  
  ###################################################################################################
  # Analysis, Run
  output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>Click to run DE analysis:</font><font color="green";>', 
                                               session$token, '</font>(save analysis ID for future reference)'));
  
  observeEvent(input$analysis.run, {
    
    # Collect inputs and parameters
    mtrx <- session.data$matrix;
    mthd <- input$analysis.step3.selection; 
    smp1 <- strsplit(input$analysis.step2.sampleA, '[\t ;,]')[[1]];
    smp2 <- strsplit(input$analysis.step2.sampleB, '[\t ;,]')[[1]];
    grps <- list(smp1, smp2);
    pair <- input$analysis.step2.paired; 
    miss <- input$analysis.step2.missing;
    mnct <- input$analysis.step2.filter;
    norm <- c(input$analysis.step2.norm1, input$analysis.step2.norm2); 
    
    # Run DE
    if (is.null(mtrx)) {
      msg <- 'No read count matrix loaded, please finish step 1'; 
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      session.data$result <- NULL;      
    } else if (length(smp1)==0 | length(smp2)==0) {
      if (length(smp1) == 0) msg <- 'No samples in group 1, please finish step 2' else 
        msg <- 'No samples in group 2, please finish step 2'
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      session.data$result <- NULL;
    } else if (is.null(mthd)) {
      msg <- 'No DE methods selected, please finish step 3'; 
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      session.data$result <- NULL;   
    } else {
      names(grps) <- c(input$analysis.step2.groupA, input$analysis.step2.groupB);
      if (pair == 'Paired') pair <- TRUE else pair <- FALSE;
      grps <- lapply(grps, function(g) g[g %in% colnames(mtrx)]);
      nums <- sapply(grps, length); 
      mthd <- rownames(DeRNAseqMs)[DeRNAseqMs[[1]] %in% mthd];
      
      # Filtering
      if (miss == 'Remove gene') mtrx <- mtrx[!is.na(rowSums(mtrx)), , drop=FALSE] else mtrx[is.na(mtrx)] <- 0;
      mtrx <- mtrx[rowSums(mtrx) >= mnct, , drop=FALSE];
      
      # Something else is wrong
      if (nrow(mtrx) < 20) { # arbitrary number
        msg <- 'Not enough genes left after filtering, require at least 20 to run DE analysis';
      } else if (nums[1]<2 | nums[2] <2) {
        msg <- 'Not enough samples to run DE analysis, require at least 2 in both groups';
      } else if (pair & nums[1]!=nums[2]) {
        msg <- paste('Paired test selected, but groups have unequal number of samples:', nums[1], 'vs.', nums[2]); 
      } else {# everything is fine
        msg <- NA;
      }
      
      # RUN !!!!
      if (!is.na(msg)) {
        output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
        session.data$result <- NULL;
      } else {
        session.data$result <- tryCatch({
          ########################################################################################################
          DeRNAseq(mtrx, grps, pair, mthd, mnct, ncls, TRUE, norm[1], norm[2], force.norm = TRUE);
          ########################################################################################################
        }, error = function(e) {
          output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
          NULL;
        });
      }
    } # end of else
    
    if (!is.null(session.data$result)) {
      msg <- paste('DE analysis <font color="green"><u>', session$token, '</u></font> is done, use the "Result" page to browse results.'); 
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      saveRDS(session.data$result, paste(session.dir, 'results.rds', sep='/')); # save on server
    }
    
    # Update result page
    res <- session.data$result;
    if (is.null(res)) {
      updateSelectInput(session, "result.select.table", label = "Select table", choices=list());
      updateSelectInput(session, "compare.select.table1", label = "Select table", choices=list());
      updateSelectInput(session, "compare.select.table2", label = "Select table", choices=list());
    }  else {
      tbl <- as.list(names(res[[2]])); 
      names(tbl) <- DeRNAseqMs[names(res[[2]]), 1];
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      updateSelectInput(session, "result.select.table", label = "Select table", choices=tbl);
      updateSelectInput(session, "compare.select.table1", label = "Select table", choices=tbl, selected = tbl[[1]]);
      updateSelectInput(session, "compare.select.table2", label = "Select table", choices=tbl, selected = tbl[[length(tbl)]]);
    }
  });
  
  session.data;
}