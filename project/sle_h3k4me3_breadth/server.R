print("starting server");

shinyServer(function(input, output, session) {
  print("new visitor");

  ########################################## "FPKM" tab ####################################################      
  fpkm.table<-reactive({
    if (input$fpkm.scale == fpkm.scale.options[2]) {
      t<-round(exp(rna$fpkm*log(2)), 3);
    } else if (input$fpkm.scale == fpkm.scale.options[3]) {
      t<-rna$fpkm;
      rk<-apply(t, 2, function(x) rank(x, ties.method='min'));
      t<-round(100*rk/nrow(t), 2);
    } else t<-round(rna$fpkm, 4);
    
    t<-cbind(rna$datatable[rownames(t), , drop=FALSE], t);
    
    t;
  });
  output$fpkm.title<-renderUI({ 
    list(h2("Expression level by RNA-seq"), 
         HTML(c('Gene expression levels in primary monocyte of controls and SLE patients were measured by RNA-seq. ', 
                'Data were normalized by calculating FPKM. ',
                'Use the panels below to change data scale or select one or more gene sets to show:'))) });
  observeEvent(input$fpkm.scale, { 
    t<-fpkm.table();
    
    rid<-CleanHtmlTags(input$fpkm.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$fpkm.gs.source]][[input$fpkm.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$fpkm.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    
    output$fpkm.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  observeEvent(input$fpkm.gs.source, { 
    output$fpkm.table <- DT::renderDataTable(fpkm.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    updateSelectizeInput(session, 'fpkm.gs.coll', choices=names(geneset[[input$fpkm.gs.source]]), select=names(geneset[[input$fpkm.gs.source]])[1])});  
  observeEvent(input$fpkm.gs.coll, { 
    output$fpkm.table <- DT::renderDataTable(fpkm.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$fpkm.gs.table <- DT::renderDataTable(geneset.table(geneset, input$fpkm.gs.source, input$fpkm.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$fpkm.gs.clear, { 
    output$fpkm.table <- DT::renderDataTable(fpkm.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$fpkm.gs.table <- DT::renderDataTable(geneset.table(geneset, input$fpkm.gs.source, input$fpkm.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });  
  observeEvent(input$fpkm.gs.table_rows_selected, {
    t<-fpkm.table();
    rid<-CleanHtmlTags(input$fpkm.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$fpkm.gs.source]][[input$fpkm.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$fpkm.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    output$fpkm.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });

  ########################################## "Cov0" tab ####################################################      
  cov0.table<-reactive({
    rng<-input$cov0.range;
    t<-cov0[, as.character(seq(rng[1], rng[2], 50))];
    t<-cbind(tss$datatable[rownames(t), 1:6, drop=FALSE], round(t, 4));
    
    t;
  });
  output$cov0.title<-renderUI({ 
    list(h2("H3K4me3 at TSSs in control samples"), 
         HTML(c('Average H3K4me3 sequencing coverage was obtained from 6 control samples after normalizing to background. ', 
                'Data were in Log2 scale, so the difference of 1.0 is equivalent to 2 fold change. ',
                'Columns correspond to relative location to TSSs (minus is upstream). ',
                'Use the panels below to change range or select one or more gene sets to show:'))) });
  observeEvent(input$cov0.range, { 
    t<-cov0.table();
    
    rid<-CleanHtmlTags(input$cov0.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$cov0.gs.source]][[input$cov0.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$cov0.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[tss$table$Gene_ID %in% gn, , drop=FALSE];
    }
    output$cov0.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  observeEvent(input$cov0.gs.source, { 
    output$cov0.table <- DT::renderDataTable(cov0.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    updateSelectizeInput(session, 'cov0.gs.coll', choices=names(geneset[[input$cov0.gs.source]]), select=names(geneset[[input$cov0.gs.source]])[1])});  
  observeEvent(input$cov0.gs.coll, { 
    output$cov0.table <- DT::renderDataTable(cov0.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$cov0.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov0.gs.source, input$cov0.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$cov0.gs.clear, { 
    output$cov0.table <- DT::renderDataTable(cov0.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$cov0.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov0.gs.source, input$cov0.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });  
  observeEvent(input$cov0.gs.table_rows_selected, {
    t<-cov0.table();
    rid<-CleanHtmlTags(input$cov0.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$cov0.gs.source]][[input$cov0.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$cov0.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    output$cov0.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });

  ########################################## "Cov1" tab ####################################################      
  cov1.table<-reactive({
    rng<-input$cov1.range;
    t<-cov0[, as.character(seq(rng[1], rng[2], 50))];
    t<-cbind(tss$datatable[rownames(t), 1:6, drop=FALSE], round(t, 4));
    
    t;
  });
  output$cov1.title<-renderUI({ 
    list(h2("H3K4me3 at TSSs in SLE patients"), 
         HTML(c('Average H3K4me3 sequencing coverage was obtained from 6 SLE patients after normalizing to background. ', 
                'Data were in Log2 scale, so the difference of 1.0 is equivalent to 2 fold change. ',
                'Columns correspond to relative location to TSSs (minus is upstream). ',
                'Use the panels below to change range or select one or more gene sets to show:'))) });
  observeEvent(input$cov1.range, { 
    t<-cov1.table();
    
    rid<-CleanHtmlTags(input$cov1.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$cov1.gs.source]][[input$cov1.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$cov1.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[tss$table$Gene_ID %in% gn, , drop=FALSE];
    }
    output$cov1.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  observeEvent(input$cov1.gs.source, { 
    output$cov1.table <- DT::renderDataTable(cov1.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    updateSelectizeInput(session, 'cov1.gs.coll', choices=names(geneset[[input$cov1.gs.source]]), select=names(geneset[[input$cov1.gs.source]])[1])});  
  observeEvent(input$cov1.gs.coll, { 
    output$cov1.table <- DT::renderDataTable(cov1.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$cov1.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov1.gs.source, input$cov1.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$cov1.gs.clear, { 
    output$cov1.table <- DT::renderDataTable(cov1.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$cov1.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov1.gs.source, input$cov1.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });  
  observeEvent(input$cov1.gs.table_rows_selected, {
    t<-cov1.table();
    rid<-CleanHtmlTags(input$cov1.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$cov1.gs.source]][[input$cov1.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$cov1.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    output$cov1.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  
  ########################################## "DEG" tab ####################################################      
  deg.table<-reactive({
    t<-rna$stat;
    t<-cbind(rna$datatable[rownames(t),], t[, -5]);
    t;
  });
  output$deg.title<-renderUI({ 
    list(h2("Differential gene expression: control vs. SLE"), 
         HTML(c('The table below reports differential expression of each gene between control and SLE samples. ', 
                'The results were obtained from normlized FPKM using edgeR method (a negative binomila implementation). ',
                'Column description:',
                '<ul>',
                '<li>"Name", "ID", "Tyep", "Description": Gene annotation</li>',
                '<li>"Mean_Count_Control", "Mean_Count_SLE", "FPKM_Control", "FPKM_SLE": Group average of read count and FPKM</li>', 
                '<li>"Log2FC", "Fold_Change", "PValue", "FDR": log2-ratio, ratio/fold change, p value and false discovery rate</li>',
                '</ul>'))) });
  observeEvent(input$deg.gs.source, { 
    output$deg.table <- DT::renderDataTable(deg.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    updateSelectizeInput(session, 'deg.gs.coll', choices=names(geneset[[input$deg.gs.source]]), select=names(geneset[[input$deg.gs.source]])[1])});  
  observeEvent(input$deg.gs.coll, { 
    output$deg.table <- DT::renderDataTable(deg.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$deg.gs.table <- DT::renderDataTable(geneset.table(geneset, input$deg.gs.source, input$deg.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$deg.gs.clear, { 
    output$deg.table <- DT::renderDataTable(deg.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$deg.gs.table <- DT::renderDataTable(geneset.table(geneset, input$deg.gs.source, input$deg.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });  
  observeEvent(input$deg.gs.table_rows_selected, {
    t<-deg.table();
    rid<-CleanHtmlTags(input$deg.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$deg.gs.source]][[input$deg.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$deg.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    output$deg.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  
  ########################################## "K4" tab ####################################################      
  k4.table<-reactive({
    t<-tss$stat;
    t[, 3:6]<-round(t[, 3:6], 4)
    t<-cbind(tss$datatable[rownames(t),1:6], t);
    t;
  });
  output$k4.title<-renderUI({ 
    list(h2("H3K4me3 breadth and change in SLE"), 
         HTML(c('The table below reports H3K4me3 peak breadth at TSSs and H3K4me3 changes around TSSs in SLE. ', 
                'The results were obtained from average sequencing depth in 6 control and 6 SLE samples. ',
                'Sequencing depth is in log2 scale, so a difference of 1.0 represents 2 fold change. ',
                'Columns description:',
                '<ul>',
                '<li>Columns 1-6: TSS annotation, location and downstream gene</li>',
                '<li>"H3K4me3_Control", "H3K4me3_SLE": H3K4me3 peak breadth at TSS, whether H3K4me3 was expanded up- or down-stream, or both.</li>', 
                '<li>"Mean_TSS", "Mean_Upstream", "Mean_Downstream": average sequencing depth at TSS (-250 to +250bp), upstream (-400 to -500bp), and downstream (+600 to +700bp).</li>',
                '<li>"Change_TSS", "Change_Upstream", "Change_Downstream": percentage change of H3K4me3 in SLE at 3 difference locations.</li>',
                '<li>"RNA_Log2FC", "RNA_PValue", "RNA_FDR": differential expression of downstream gene according to RNA-seq results.</li>',
                '</ul>'))) });
  observeEvent(input$k4.gs.source, { 
    output$k4.table <- DT::renderDataTable(k4.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    updateSelectizeInput(session, 'k4.gs.coll', choices=names(geneset[[input$k4.gs.source]]), select=names(geneset[[input$k4.gs.source]])[1])});  
  observeEvent(input$k4.gs.coll, { 
    output$k4.table <- DT::renderDataTable(k4.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$k4.gs.table <- DT::renderDataTable(geneset.table(geneset, input$k4.gs.source, input$k4.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$k4.gs.clear, { 
    output$k4.table <- DT::renderDataTable(k4.table(), options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
    output$k4.gs.table <- DT::renderDataTable(geneset.table(geneset, input$k4.gs.source, input$k4.gs.coll, 'human'), options = dt.options2, rownames=FALSE, server=TRUE, escape = FALSE)
  });  
  observeEvent(input$k4.gs.table_rows_selected, {
    t<-k4.table();
    rid<-CleanHtmlTags(input$k4.gs.table_rows_selected);    
    if (length(rid) > 0) {
      rid<-rid[rid %in% rownames(geneset[[input$k4.gs.source]][[input$k4.gs.coll]][['human']])]; 
      gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$k4.gs.source), '_list.rds', sep=''))[rid];  
      gn<-unique(unlist(gs, use.names=FALSE));
      t<-t[rna$anno$ID %in% gn, , drop=FALSE];
    };
    output$k4.table <- DT::renderDataTable(t, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  });
  
  ########################################## "ORA" tab ####################################################      
  output$ora.table <- DT::renderDataTable({
    t<-tss$ora[[input$ora.select]][[1]];
    t;
  }, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  output$ora.title<-renderUI({
    list(h2("Gene set over-representation related to H3K4me3 breadth"), 
         HTML(c('Predefined gene sets were collected from difference sources (BioSystems, KEGG, MSigDb, etc.). ', 
                'Enrichment of genes with each H3K4me3 pattern was tested using all genes with detectable H3K4me3 at their TSS as background. ',
                'Columns description:',
                '<ul>',
                '<li>Columns 1-4: Gene set annotation (source, name, etc.). </li>',
                '<li>"N_Set": Total number of genes in the gene set and background. </li>',
                '<li>"N_Within", "Percent": Number and percentage of genes in the gene set and the input set.</li>', 
                '<li>"Odds_Ratio", "P_HyperGeo", "FDR_BH", "FWER": Statistic results from hypergeomatric test, FWER-familywise error rate.</li>',
                '</ul>')));
  });
  
  ########################################## "TFBS" tab ####################################################      
  output$tfbs.table <- DT::renderDataTable({
    t<-pwm$enrich;
    anno<-pwm$anno[rownames(t), ];
    data.frame(ID=rownames(anno), anno, t, stringsAsFactors = FALSE);
  }, options = dt.options1, filter='bottom', select='none', rownames=FALSE, server=TRUE, escape = FALSE);     
  output$tfbs.title<-renderUI({
    list(h2("TFBS related to H3K4me3 peak breadth"), 
       HTML(c('Potential TFBS (transcription factor binding site) was searched within -1kb to 1kb around all TSSs. ', 
              'Frequency of TFBS of each H3K4me3 peak pattern was calculated as number of TFBSs per TSS. ',
              'Relative enrichment was calculated by calculating relative frequency over TSSs without H3K4me3. ',
              'PWMs of 2414 motifs were obtained from 4 sources: ENCODE, Jasper, TRANSFAC, and UniPROBE. ',
              'Columns description:',
              '<ul>',
              '<li>Columns 1-5: PWM annotation. </li>',
              '<li>"GC": proportion of GC in PWM. </li>',
              '<li>"N": Total number of potential TFBSs around all TSSs.</li>', 
              '<li>"Narrow", "Upstream", "Downstream", "Both", "Unclassified", "No H3K4me3": TFBS enrichment of TSSs with each H3K4me3 peak pattern.</li>',
               '</ul>')));
  });

  ########################################## "Plot cov" tab ####################################################      
  output$cov.title<-renderUI({
    list(h2("Plot H3K4me4 peaks"), 
         HTML(c('Plot sequencing depth of H3K4me3 at one or a set of TSSs. The depth was the normalized average of 6 control or 6 SLE samples.', 
                'By default, the plotted data is in log2 scale, so a difference of 1.0 is the same as 2 fold change. ',
                'Plot options:',
                '<ul>',
                '<li>"Plot unit": plot individual TSS or all TSSs mapped to a gene set. </li>',
                '<li>"Plot value": plot control and SLE data separately, or their difference (SLE - control). </li>',
                '<li>"Plot type": plot type (landscape vs. barplot).</li>', 
                '<li>"Plot scale": plot data in log2 scale (default), or unlogged scale.</li>',
                '<li>"Plot range": plot range around TSS (-10kb to +10kb).</li>',
                '</ul>')));
  });
  output$cov.tss.table <- DT::renderDataTable({
    t<-tss$datatable[, c(1, 5, 6, 2, 3, 4, 7, 8), drop=FALSE];
    t<-t[order(t[, 7]), , drop=FALSE];
  }, options = dt.options2, filter='bottom', select='single', rownames=FALSE, server=TRUE, escape = FALSE);     
  observeEvent(input$cov.gs.source, { 
    updateSelectizeInput(session, 'cov.gs.coll', choices=names(geneset[[input$cov.gs.source]]), select=names(geneset[[input$cov.gs.source]])[1])});  
  observeEvent(input$cov.gs.coll, { 
    output$cov.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov.gs.source, input$cov.gs.coll, 'human'), 
                                               options = dt.options2, select='single', rownames=FALSE, server=TRUE, escape = FALSE)
  });
  observeEvent(input$cov.gs.clear, { 
    output$cov.gs.table <- DT::renderDataTable(geneset.table(geneset, input$cov.gs.source, input$cov.gs.coll, 'human'), 
                                               options = dt.options2, select='single', rownames=FALSE, server=TRUE, escape = FALSE)
  }); 
  output$cov.plot<-renderPlot({ 
    if (input$cov.choice1 == plot.cov.choice1[1]) {
      rid0<-input$cov.tss.table_rows_selected; 
      rid1<-input$cov.tss.table_row_last_clicked; 
      if (length(rid1)>0) if (rid1 %in% rid0) rid<-rid1 else rid<-c() else rid<-c();
    } else {
      rid0<-CleanHtmlTags(input$cov.gs.table_rows_selected);  
      rid1<-CleanHtmlTags(input$cov.gs.table_row_last_clicked); 
      if (length(rid1)>0) if (rid1 %in% rid0) {
        rid<-rid1;
        rid<-rid[rid %in% rownames(geneset[[input$cov.gs.source]][[input$cov.gs.coll]][['human']])]; 
        gs<-readRDS(paste(GENESET_HOME, '/', tolower(input$cov.gs.source), '_list.rds', sep=''))[rid];  
        gn<-unique(unlist(gs, use.names=FALSE));
        rid<-rownames(tss$table)[tss$table[, 'Gene_ID'] %in% gn];
      } else rid<-c() else rid<-c();
    }
    plot.cov(tss$cov, rid, input$cov.choice1, input$cov.choice2, input$cov.choice3, input$cov.choice4, input$cov.range);
  }, height = 480, width=960);
  
  ########################################## "Plot Venn" tab ####################################################      
  output$venn.title<-renderUI({
    list(h2("Differential expression vs. differential H3K4me3"), 
         HTML(c('Plot Venn diagram to show overlapping between genes with differenital expression and differential H3K4me3. ', 
                'Genes were selected using their statistics from both data sets. ',
                'Statistical significance of overlapping was calculated by Fisher test, using all genes with valid measurements in both data sets as background. ',
                'Gene selection options:',
                '<ul>',
                '<li>RNA-seq data: expression level as in log2FPKM and percentile and differential expression as in og2-ratio, p value, and FDR. </li>',
                '<li>H3K4me3 data: sequencing depth as in log2(depth) and percentile and differential H3K4me3 as in percent change, within one of three regions (TSS, upstream, and downstream) around TSS. </li>',
                '<li>"N": Total number of potential TFBSs around all TSSs.</li>', 
                '<li>"Narrow", "Upstream", "Downstream", "Both", "Unclassified", "No H3K4me3": TFBS enrichment of TSSs with each H3K4me3 peak pattern.</li>',
                '</ul>')));
  });
  output$venn<-renderPlot({
    t1<-stat.rna;
    t1<-t1[t1[,1]>=input$venn.rna.pct[1] & t1[,1]<=input$venn.rna.pct[2], , drop=FALSE];
    t1<-t1[t1[,2]>=input$venn.rna.fpkm[1] & t1[,2]<=input$venn.rna.fpkm[2], , drop=FALSE];
    t1<-t1[t1[,3]>=input$venn.rna.l2r[1] & t1[,3]<=input$venn.rna.l2r[2], , drop=FALSE];
    t1<-t1[t1[,4]>=input$venn.rna.p[1] & t1[,4]<=input$venn.rna.p[2], , drop=FALSE];
    t1<-t1[t1[,5]>=input$venn.rna.fdr[1] & t1[,5]<=input$venn.rna.fdr[2], , drop=FALSE];
    
    ind<-which(venn.tss.choice == input$venn.tss.region);
    t2<-stat.tss[, seq(ind, 9, 3)]; print(dim(t2));
    t2<-t2[t2[,1]>=input$venn.tss.pct[1] & t2[,1]<=input$venn.tss.pct[2], , drop=FALSE]; print(dim(t2));
    t2<-t2[t2[,2]>=input$venn.tss.cov[1] & t2[,2]<=input$venn.tss.cov[2], , drop=FALSE]; print(dim(t2));
    t2<-t2[t2[,3]>=input$venn.tss.chg[1] & t2[,3]<=input$venn.tss.chg[2], , drop=FALSE]; print(dim(t2));
    
    output$venn.table <- DT::renderDataTable({
      t<-t2[rownames(t2) %in% rownames(t1), , drop=FALSE];
      data.frame(stringsAsFactors = FALSE, Name=rownames(t), RNA_log2ratio=t1[rownames(t), 3], RNA_p=t1[rownames(t), 4], H3K4m3_pct_chg=t[, 3]);
    }, options = dt.options2, filter='bottom', select='single', rownames=FALSE, server=TRUE, escape = FALSE);     
    
    awsomics::PlotVenn(unique(rownames(t1)), unique(rownames(t2)), c('Expression', 'H3K4me3'), gene.u);
  }, height=480, width=640);
    
  ########################################## "Plot PWM" tab ####################################################      
  pwm.rid<-reactive({
    rid0<-input$pwm.table_rows_selected; 
    rid1<-input$pwm.table_row_last_clicked; 
    if (length(rid1>0)) if (rid1 %in% rid0) rid<-rid1 else rid<-c() else rid<-c();
    rid;
  });
  output$pwm.title<-renderUI({
    list(h2("Plot TFBS frequency"), 
         HTML(c('Plot frequency of matches to each PWM (point weighted matrix) at TSS.', 
                'TSSs were grouped based on H3K4me3 peak breadth. ',
                'Plot options:',
                '<ul>',
                '<li>"Select H3K4me3 pattern": H3K4me3 peak patterns, <No H3K4me3> means no detectable H3K4me3 higher than background. </li>',
                '<li>"Plot relative enrichment": Plot relative enrichment to <No H3K4me3> if checked. </li>',
                '</ul>')));
  });
  output$pwm.table <- DT::renderDataTable({
    t<-pwm$enrich;
    anno<-pwm$anno[rownames(t), ];
    data.frame(ID=rownames(anno), anno, t, stringsAsFactors = FALSE);
  }, options = dt.options2, filter='bottom', select='single', rownames=FALSE, server=TRUE, escape = FALSE);     
  output$pwm.logo<-renderPlot({
    rid<-pwm.rid();
    if (length(rid) == 0) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main="No PWM selected") else {
      seqLogo::seqLogo(pwm$pwm[[rid[1]]]);
    }
  }, height = 480);
  output$pwm.enrich<-renderPlot({
    rid<-pwm.rid();
    if (length(rid) == 0) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main="No PWM selected") else {
      grp<-input$pwm.select;
      if (length(grp) == 0) plot(0, xaxt='n', yaxt='n', type='n', xlab='', ylab='', axes=FALSE, main="Select a TSS subset to plot") else {
        d<-pwm$freq[[rid[1]]];
        if (input$pwm.relative) d<-apply(d, 2, function(x) x/d[,6]);
        par(mar=c(5,8,2,2));    
        plot(0, type='n', yaxs='i', ylim=c(0, 1.2*max(d, na.rm=TRUE)), xlim=c(-1000, 1000), xaxs='i', xlab='Distance to TSS', ylab='TFBS per TSS', cex.lab=2);
        title(main=paste(rid[1], pwm$anno[rid[1], 'Name'], pwm$anno[rid[1], 'Consensus']), cex.main=1.5);
        for (i in 1:length(grp)) lines(seq(-1000, 1000, 50), d[, grp[i]], lwd=2, col=plot.pwm.col[grp[i]]);
        if (length(grp) > 1) legend(-1000, 1.2*max(d, na.rm=TRUE), legend=grp, bty='n', lwd=2, cex=1.5, col=plot.pwm.col[grp]);
      }
    }
  }, height = 480)
});

