####################################
####### sle_h3k4me3_breadth ########
####################################

print("loading UI");
print(system.time(source("/zhangz/awsomics/project/sle_h3k4me3_breadth/preload.R")));

shinyUI(
  
  navbarPage(
    id="h3k4me3", collapsible = TRUE, title='H3K4me3 breadth',
    ############################################################################## 
    ################################# "About" tab ################################
   #tabPanel("About", h1('<Working version manuscript>'), uiOutput('about')),
   #tabPanel("About", fluidRow(h1('<Working version manuscript>'), uiOutput('about'))),
   tabPanel("Home", id = 'tab.home', wellPanel(style = "background-color: #ffffff;", h4('<working version manuscript>'),  includeMarkdown('index.md'))), 
  ############################################################################### 
  ################################ "Data" menu ##################################     
  {
    navbarMenu(
      "Data",
      ####################
      
      ############################################################################
      ############################# "RNA-seq" tab ###############################
      tabPanel(
        "RNA-seq, FPKM", htmlOutput('fpkm.title'), br(),
        wellPanel( fluidRow(
          column(3, 
                 h4("Select data scale"), 
                 selectInput("fpkm.scale", NULL, fpkm.scale.options),
                 h4("Select gene subset(s)"),
                 selectizeInput("fpkm.gs.source", h5('Source'), names(geneset), selected=names(geneset)[1]),
                 selectizeInput("fpkm.gs.coll", h5('Collection'), names(geneset[[1]]),selected=names(geneset[[1]])[1]),
                 actionButton("fpkm.gs.clear", 'Clear selection')),
          column(9, wellPanel(DT::dataTableOutput('fpkm.gs.table'))))), 
        DT::dataTableOutput('fpkm.table')
      ),
      tabPanel(
        "H3K4me3, Control", htmlOutput('cov0.title'), br(),
        wellPanel( fluidRow(
          column(3, 
                 h4("Distance to TSS"), 
                 sliderInput("cov0.range", NULL, -10000, 10000, c(-2000, 2000), step=500, ticks=FALSE),
                 h4("Select gene subset(s)"),
                 selectizeInput("cov0.gs.source", h5('Source'), names(geneset), selected=names(geneset)[1]),
                 selectizeInput("cov0.gs.coll", h5('Collection'), names(geneset[[1]]),selected=names(geneset[[1]])[1]),
                 actionButton("cov0.gs.clear", 'Clear selection')),
          column(9, wellPanel(DT::dataTableOutput('cov0.gs.table'))))), 
        DT::dataTableOutput('cov0.table')
      ),
      tabPanel(
        "H3K4me3, SLE", htmlOutput('cov1.title'), br(),
        wellPanel( fluidRow(
          column(3, 
                 h4("Distance to TSS"), 
                 sliderInput("cov1.range", NULL, -10000, 10000, c(-2000, 2000), step=500, ticks=FALSE),
                 h4("Select gene subset(s)"),
                 selectizeInput("cov1.gs.source", h5('Source'), names(geneset), selected=names(geneset)[1]),
                 selectizeInput("cov1.gs.coll", h5('Collection'), names(geneset[[1]]),selected=names(geneset[[1]])[1]),
                 actionButton("cov1.gs.clear", 'Clear selection')),
          column(9, wellPanel(DT::dataTableOutput('cov1.gs.table'))))), 
        DT::dataTableOutput('cov1.table')
      )
    )
  },
  
  ############################################################################### 
  ################################ "Result" menu ################################     
  {
  navbarMenu(
    "Result",
    ####################
    
    ############################################################################
    ############################# "DED" tab ###############################
    tabPanel(
      "Differential expression", htmlOutput('deg.title'), br(),
      wellPanel( fluidRow(
        column(3, 
               h4("Select gene subset(s)"),
               selectizeInput("deg.gs.source", h5('Source'), names(geneset), selected=names(geneset)[1]),
               selectizeInput("deg.gs.coll", h5('Collection'), names(geneset[[1]]),selected=names(geneset[[1]])[1]),
               actionButton("deg.gs.clear", 'Clear selection')),
        column(9, wellPanel(DT::dataTableOutput('deg.gs.table'))))), 
      DT::dataTableOutput('deg.table')      
    ), 
    
    ############################################################################
    ############################# "k4" tab #####################################
    tabPanel(
      "H3K4me3 pattern", htmlOutput('k4.title'), br(),
      wellPanel( fluidRow(
        column(3, 
               h4("Select gene subset(s)"),
               selectizeInput("k4.gs.source", h5('Source'), names(geneset), selected=names(geneset)[1]),
               selectizeInput("k4.gs.coll", h5('Collection'), names(geneset[[1]]),selected=names(geneset[[1]])[1]),
               actionButton("k4.gs.clear", 'Clear selection')),
        column(9, wellPanel(DT::dataTableOutput('k4.gs.table'))))), 
      DT::dataTableOutput('k4.table')      
    ), 
    
    ############################################################################
    ############################# "ORA" tab ####################################
    tabPanel(
      "Gene set over-representation", htmlOutput('ora.title'), br(),
      wellPanel(fluidRow(column(4, selectizeInput('ora.select', h5('Select H3K4me3 peak pattern'), names(tss$ora), selected=names(tss$ora)[1])))), 
      DT::dataTableOutput('ora.table')      
    ),
    
    ############################################################################
    ############################# "TFBS" tab ###################################
    tabPanel(
      "TFBS enrichment", htmlOutput('tfbs.title'), br(),
      DT::dataTableOutput('tfbs.table')      
    )  
  )
  },
  
  ############################################################################### 
  ################################ "Visualization" menu #########################  
  {
  navbarMenu(
    "Visualization",
    ####################
    
    ############################################################################
    ############################# "cov" tab ####################################
    tabPanel(
      "H3K4me3 sequencing depth", htmlOutput('cov.title'), br(),
      wellPanel( fluidRow(
        column(6, wellPanel(
          h3('Select a TSS'),
          DT::dataTableOutput('cov.tss.table'))),
        column(6, wellPanel(
          h3('Select a gene set'),
          fluidRow(
            column(4, selectizeInput("cov.gs.source", NULL, names(geneset), selected=names(geneset)[1])),
            column(4, selectizeInput("cov.gs.coll", NULL, names(geneset[[1]]),selected=names(geneset[[1]])[1])),
            column(1, br()),
            column(3, actionButton("cov.gs.clear", 'Clear selection'))),
          DT::dataTableOutput('cov.gs.table'))),
        column(12, wellPanel( fluidRow(
          column(2, radioButtons("cov.choice1", "Select plot unit", plot.cov.choice1)),
          column(2, radioButtons("cov.choice2", "Select plot value", plot.cov.choice2)),
          column(2, radioButtons("cov.choice3", "Select plot type", plot.cov.choice3)),
          column(2, radioButtons("cov.choice4", "Select plot scale", plot.cov.choice4)),
          column(4, sliderInput("cov.range", "Select plot range", -10000, 10000, c(-2000, 2000), step=500, ticks=FALSE))))))), 
      plotOutput('cov.plot', width='75%')
    ),
    
    ############################################################################
    ############################# "venn" tab ###################################
    tabPanel(
      "Expression-H3K4me3 overlapping", htmlOutput('venn.title'), br(),
      wellPanel( fluidRow(
        column(6, wellPanel(
          h4('Select genes based on expression data'),
          sliderInput('venn.rna.fpkm', 'Expression level in log2FPKM', -7, 18, c(-7, 18), 1, TRUE), 
          sliderInput('venn.rna.pct', 'Expression level in percentile (%)', 0, 100, c(0, 100), 5, TRUE),
          sliderInput('venn.rna.l2r', 'Differential expression, log2-ratio', -8, 8, c(-8, 8), 0.2, FALSE),
          sliderInput('venn.rna.p', 'Differential expression, p value', 0, 1, c(0, 1), 0.01, FALSE),
          sliderInput('venn.rna.fdr', 'Differential expression, FDR', 0, 1, c(0, 1), 0.01, FALSE)          
        )),
        column(6, wellPanel(
          h4('Select genes based on H3K4me3'),
          selectizeInput('venn.tss.region', 'Select region around TSS', venn.tss.choice, venn.tss.choice[1], width='50%'),
          sliderInput('venn.tss.cov', 'H3K4me3 level in log2(depth)', -0.25, 2, c(-0.25, 2), 0.05, TRUE), 
          sliderInput('venn.tss.pct', 'H3K4me3 level in percentile (%)', 0, 100, c(0, 100), 5, TRUE),
          sliderInput('venn.tss.chg', 'Differential H3K4me3, percentage change (%)', -25, 80, c(-25, 80), 5, TRUE))))),
      fluidRow(
        column(8, plotOutput('venn', width='100%')), 
        column(4, wellPanel( h3('Overlapping genes'), DT::dataTableOutput('venn.table'))))
    ),
    
    ############################################################################
    ############################# "pwm" tab ####################################
    tabPanel(
      "TFBS frequency", htmlOutput('pwm.title'), br(),
      wellPanel( fluidRow(
        column(3, 
               wellPanel(checkboxGroupInput('pwm.select', h4('Select H3K4me3 pattern'), plot.pwm.choice, selected=plot.pwm.choice[c(1, 4, 6)])),
               wellPanel(checkboxInput('pwm.relative', h4('Plot relative enrichment'), FALSE))),
        column(9, wellPanel(h4('Select a PWM'), DT::dataTableOutput('pwm.table'))))),
      fluidRow(
        column(7, plotOutput('pwm.enrich', width='100%')),
        column(5, plotOutput('pwm.logo', width='100%')))
    )
  )
  }
  ) # end of navbarPage
) # end of shinyUI


