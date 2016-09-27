rnaseq2g.run.de <- function(input, output, session, session.data) {
  
  session.dir  <- paste(APP_HOME, 'log', session$token, sep='/');
  if (!file.exists(session.dir)) dir.create(session.dir, recursive = TRUE);
  
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
    } else {
      msg <- NA;
    }
    
    # RUN !!!!
    if (!is.na(msg)) {
      output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
      session.data$result <- NULL;
    } else {
      session.data$result <- tryCatch({
        DeRNAseq(mtrx, grps, pair, mthd, mnct, ncls, TRUE, norm[1], norm[2], force.norm = TRUE);
      }, error = function(e) {
        output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
        NULL;
      });
    }
  } # end of else
  
  if (!is.null(session.data$result)) {
    msg <- paste('DE analysis <font color="green"><u>', session$token, '</u></font> is done, use the "Result" page to browse results.'); 
    output$analysis.run.message <- renderUI(HTML('<font color="darkblue";>', msg, '</font>'));
    saveRDS(session.data$result, paste(session.dir, 'results.rds', sep='/'));
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
  
  session.data
}