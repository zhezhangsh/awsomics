#####################
####### GeEx ########
#####################

print("loading UI");
print(system.time(source("/zhangz/awsomics/geex/preload.R")));

shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),
  
  navbarPage(
    id="GeEx", collapsible = TRUE, title='GeEx',
    
    ############################################################################ 
    ################################ "Home" tab ################################ 
    tabPanel( 
      "Home", htmlOutput('home.title'), hr(), 
      htmlOutput('home.message'), br(),
      DT::dataTableOutput('home.table')
    ),
 
  ############################################################################### 
  ################################ "Data" menu ##################################     
  {
    navbarMenu(
      "Data",
      ####################
      
      ############################################################################
      ############################# "Metadata" tab ###############################
      tabPanel(
        "Metadata", htmlOutput('meta.title'), br(), 
        wellPanel(h4("Select metadata table"), selectInput("meta.options", NULL, c(), width='150px')), 
        htmlOutput('meta.message'), br(), 
        DT::dataTableOutput('meta.table'), br(), br(),
        wellPanel(htmlOutput('meta.detail'))
      ), # end of tabPanel
      ############################################################################
      ############################# "Data set" tab ###############################
      tabPanel(
        "Data set summary", htmlOutput('expr.title'), br(), 
        wellPanel(fluidRow(
          column(3, wellPanel(
            selectizeInput("expr.dataset", "Select data set", c(), width='100%'),
            checkboxGroupInput("expr.group", label='Select group(s)', choices=c()),
            checkboxGroupInput("expr.species", label='Select species', choices=c()),
            selectizeInput("expr.scale", label='Select data scale', choices=c('logged', 'unlogged', 'percentile'), 
                           selected='logged', width='80%'))),
          column(3, wellPanel(
            h5('Select table column(s)'),
            checkboxGroupInput('expr.column', NULL, choice=expr_columns, selected=expr_columns[1], inline=FALSE),            
            checkboxGroupInput('expr.desc', h5('-Descriptive statistics'), choices=GetRowStatTypes(), 
                               selected=GetRowStatTypes(), inline=TRUE),
            checkboxGroupInput('expr.anno', h5('-Gene annotation'), choices=expr_anno_columns, 
                               selected=expr_anno_columns[c(1, length(expr_anno_columns))], inline=TRUE), br(), 
            checkboxInput('expr.select.all', HTML(geex.html.msg('Select all')), FALSE))),
          column(6, wellPanel(h5('Select gene subset(s)'), 
            fluidRow(
              column(4, selectizeInput("expr.gs.source", "Source", names(geneset), 
                                       selected=names(geneset)[1])),
              column(4, selectizeInput("expr.gs.coll", "Collection", names(geneset[[1]]), 
                                       selected=names(geneset[[1]])[1])),
              column(4, selectizeInput("expr.gs.species", "Species", names(geneset[[1]][[1]]), 
                                       selected=names(geneset[[1]][[1]])[1])),
              DT::dataTableOutput('expr.gs.table', width='100%'),
              actionButton("expr.gs.clear", 'Clear selection')))))), 
        htmlOutput('expr.message'),
        DT::dataTableOutput('expr.table')
      ), # end of tabPanel

      ############################################################################
      ############################# "Gene summary" tab ###########################
      tabPanel(
        "Gene summary", htmlOutput('gene.title'), br(), 
        wellPanel(
          h4("Select the gene to show"), 
          fluidRow(column(2, checkboxInput('gene.filter', HTML(geex.html.msg('Filter by gene set')), value=FALSE)), 
                   column(10, uiOutput("gene.ui"))),
          wellPanel(DT::dataTableOutput('gene.table'))),
        htmlOutput('gene.message'), br(), 
        DT::dataTableOutput('gene.stat.table')
      ), # end of tabPanel
      
      ############################################################################
      ############################# "Gene-data set " tab #########################
      tabPanel(
        "Gene-data set", htmlOutput('comb.title'), br(), 
        wellPanel(
          fluidRow(column(2, checkboxInput('comb.filter', HTML(geex.html.msg('Filter by gene set')), value=FALSE)), 
                   column(10, uiOutput("comb.ui"))), br(),
          fluidRow( 
            column(4, wellPanel(
              fluidRow(column(5, h5('Select species')), 
                       column(6, selectizeInput("comb.species", NULL, choices='human'))),
              fluidRow(column(5, h5('Select data scale')), 
                       column(6, selectizeInput("comb.scale", NULL, choices=c('logged', 'unlogged', 'percentile'), 
                                                selected='percentile'))))),
            column(4, wellPanel(
              checkboxGroupInput('comb.desc', 'Select descriptive statistics', choices=GetRowStatTypes(), 
                                 selected=GetRowStatTypes()[1:(length(GetRowStatTypes())-1)], inline=TRUE))),
            column(4, wellPanel(
              checkboxGroupInput('comb.anno', 'Select gene annotation', choices=expr_anno_columns, 
                                 selected=expr_anno_columns[1], inline=TRUE))))),
        DT::dataTableOutput('comb.table')
      ) # end of tabPanel
    ) # end of navbarMenu
  },
  
  ######################################################################################## 
  ################################ "Visualization" menu ##################################       
  {
    ####################
    navbarMenu(
      "Visualization",
      ####################
   
      #######################################################################
      ############################# "PCA" tab ###############################
      tabPanel(
        "Principal components analysis", htmlOutput('pca.title'), hr(),
        fluidRow(
            column(8, 
                   wellPanel( fluidRow (
                     column(4, wellPanel(
                       h5("Select principal components"),
                       column(6, selectizeInput("pca.x", NULL, c())),
                       column(6, selectizeInput("pca.y", NULL, c())),
                       h5("Select color range"),
                       selectizeInput("pca.color", NULL, GetColorTypes()))),
                     column(4, wellPanel(selectizeInput("pca.dataset", "Select data set", c(), width='100%'))),
                     column(4, wellPanel(checkboxGroupInput("pca.group", label='Select group(s)', choices=c()))))),
                   htmlOutput('pca.message'), br(),
                   plotOutput('pca.plot')),
            column(4, 
                   wellPanel(
                     h4('Select gene subset(s) to re-run PCA'),
                     column(3, h5("Source")), 
                     column(8, selectizeInput("pca.geneset.source", NULL, names(geneset), selected=names(geneset)[1], 
                                              options=list('caption-side'='bottom'))),
                     column(3, h5("Collection")), 
                     column(8, selectizeInput("pca.geneset.coll", NULL, names(geneset[[1]]), 
                                              selected=names(geneset[[1]])[1])),
                     column(3, h5("Species")), 
                     column(8, selectizeInput("pca.geneset.species", NULL, names(geneset[[1]][[1]]), 
                                              selected=names(geneset[[1]][[1]])[1])),
                     DT::dataTableOutput('pca.geneset.table', width='100%'), 
                     actionButton("pca.geneset.clear", 'Clear selection')),
                   wellPanel(
                     h5('Select sample(s) to highlight'),
                     DT::dataTableOutput('pca.table'),
                     actionButton("pca.clear", 'Clear selection'))))   
      ), # end of tabPanel     

      ############################################################################## 
      ############################# "Barplot" tab ##################################
      tabPanel(
        "Single gene expression", htmlOutput('bar.title'), hr(),
        fluidRow(
          column(8, 
                 wellPanel(fluidRow(
                   column(4, wellPanel(
                     selectizeInput("bar.scale", "Select data scale", c('logged', 'unlogged', 'percentile')),
                     selectizeInput("bar.color", "Select color range", GetColorTypes()))),
                   column(4, wellPanel(
                     selectizeInput("bar.dataset", "Select data set", c(), width='100%'),
                     uiOutput('bar.dataset.message'))),
                   column(4, wellPanel(
                     checkboxGroupInput("bar.group", label='Select group(s)', choices=c()), 
                     checkboxInput("bar.mean", HTML(geex.html.msg("Plot group mean"))))))), 
                 htmlOutput('bar.message'),
                 plotOutput('bar.plot', width='100%'),
                 DT::dataTableOutput('bar.stat'), br()),
          column(4, 
                 wellPanel(
                   h3('Select gene(s)'),
                   uiOutput('bar.table.message'),
                   DT::dataTableOutput('bar.table'), 
                   actionButton("bar.clear", 'Clear selection'),
                   fluidRow(
                     column(6,
                            checkboxInput('lookup.option', HTML(geex.html.msg("Look up gene in dictionary")), FALSE),
                            uiOutput("lookup.key.ui"),
                            uiOutput("lookup.species.ui")
                     ),
                     column(6, uiOutput("lookup.table.ui")
                     )))))
          ), # end of tabPanel     

      
      ############################################################################## 
      ############################### "Gene set" tab ###############################
      tabPanel(
        "Gene set expression", htmlOutput('geneset.title'), hr(),
        fluidRow(
          column(8, wellPanel( fluidRow(
            column(4, wellPanel( 
              h4("Select plot"),
              fluidRow(column(4, h5("Plot\ntype")), 
                       column(8, selectizeInput("geneset.type", NULL, PlotMatrixGroupTypes()))),             
              fluidRow(column(4, h5("Data\nscale")), 
                       column(8, selectizeInput("geneset.scale", NULL, c('logged', 'unlogged', 'percentile')))),
              fluidRow(column(4, h5("Color\nrange")), 
                       column(8, selectizeInput("geneset.color", NULL, GetColorTypes()))))),    
            column(4, wellPanel( 
              h4("Select data set"), 
              selectizeInput("geneset.dataset", NULL, c(), width='100%'),
              checkboxInput("geneset.normalize", HTML(geex.html.msg("Normalize across samples")), TRUE))),
            column(4, wellPanel( h4('Select group(s)'), 
                                 checkboxGroupInput("geneset.group", NULL, label=, choices=c())))), 
            column(12, br(), htmlOutput('geneset.message')),
            column(12, plotOutput('geneset.plot', width='640px', height='640px')))),
          column(4,
                 wellPanel(
                   h4('Select one gene set'),
                   column(3, h5("Source")), 
                   column(8, selectizeInput("geneset.source", NULL, names(geneset), selected=names(geneset)[1], 
                                            options=list('caption-side'='bottom'))),
                   column(3, h5("Collection")), 
                   column(8, selectizeInput("geneset.coll", NULL, names(geneset[[1]]), 
                                            selected=names(geneset[[1]])[1])),
                   column(3, h5("Species")), 
                   column(8, selectizeInput("geneset.species", NULL, names(geneset[[1]][[1]]), 
                                            selected=names(geneset[[1]][[1]])[1])),
                   DT::dataTableOutput('geneset.table', width='100%'), 
                   actionButton("geneset.clear", 'Clear selection'))))
      ), # end of tabPanel    
      
      ############################################################################## 
      ############################# "Compare" tab ###############################
      tabPanel(
        "Differential expression", htmlOutput('two.title'), hr(),
        fluidRow(
          column(8, wellPanel( fluidRow(
            column(4, wellPanel( 
              h4("Select plot"),
              fluidRow(column(4, h5("Plot\ntype")), 
                       column(8, selectizeInput("two.type", NULL, PlotPairDiffTypes()))),             
              fluidRow(column(4, h5("Data\nscale")), 
                       column(8, selectizeInput("two.scale", NULL, c('logged', 'unlogged', 'percentile')))),
              fluidRow(column(4, h5("Color\nrange")), 
                       column(8, selectizeInput("two.color", NULL, GetColorTypes()))),    
              fluidRow(column(4, h5("Use\nspecies")), 
                       column(8, selectizeInput("two.select.species", NULL, c()))))),
            column(4, wellPanel(
              h4('Select X-axis'),
              selectizeInput("x.dataset", "Data set", c(), width='100%'),
              selectizeInput("x.group", "Sample group", c(), width='100%'), 
              selectizeInput("x.sample", "Single sample", c(), width='100%'))),
            column(4, wellPanel(
              h4('Select Y-axis'),
              selectizeInput("y.dataset", "Data set", c(), width='100%'),
              selectizeInput("y.group", "Sample group", c(), width='100%'),
              selectizeInput("y.sample", "Single sample", c(), width='100%'))))), 
            column(12, htmlOutput('two.message')),
            column(12, plotOutput('two.plot', width='640px', height='640px')),
            column(12, wellPanel(DT::dataTableOutput('two.geneset.stat', width='90%')))),
          column(4,
                 wellPanel(
                   h4('Highlight gene(s)'),
                   DT::dataTableOutput('two.table', width='100%'), 
                   actionButton("two.clear", 'Clear selection')), 
                 wellPanel(
                   h4('Highlight gene set(s)'),
                   column(3, h5("Source")), 
                   column(8, selectizeInput("two.source", NULL, names(geneset), 
                                            selected=names(geneset)[1], options=list('caption-side'='bottom'))),
                   column(3, h5("Collection")), 
                   column(8, selectizeInput("two.coll", NULL, names(geneset[[1]]), 
                                            selected=names(geneset[[1]])[1])),
                   column(3, h5("Species")), 
                   column(8, selectizeInput("two.species", NULL, names(geneset[[1]][[1]]), 
                                            selected=names(geneset[[1]][[1]])[1])),
                   DT::dataTableOutput('two.table.geneset', width='100%'), 
                   actionButton("two.clear.geneset", 'Clear selection'))))
      ), # end of tabPanel    
      
      ############################################################################
      ############################# "Coexpression" tab ###########################
      tabPanel(
        "Two gene coexpression", htmlOutput('coex.title'), br(),
        fluidRow(
          column(8, 
                 wellPanel( fluidRow(
                   column(4, wellPanel(
                     selectizeInput('coex.type', 'Select plot unit', rev(data.level.options), width='60%'),
                     selectizeInput("coex.scale", 'Select data scale', c('logged', 'unlogged', 'percentile'),  width='60%'),
                     selectizeInput("coex.color", 'Select color range', GetColorTypes(),  width='60%'),
                     
                     fluidRow(
                       column(4,
                              checkboxInput('coex.lookup.option', HTML(geex.html.msg("Look up gene in dictionary")), FALSE),
                              uiOutput("coex.lookup.key.ui"),
                              uiOutput("coex.lookup.species.ui")                       
                       ), column(8, uiOutput("coex.lookup.table.ui")))
                   )),
                   column(4, wellPanel(h4("Select gene 1"), DT::dataTableOutput('coex.gene1', width='100%'))),
                   column(4, wellPanel(h4("Select gene 2"), DT::dataTableOutput('coex.gene2', width='100%'))))),
                 column(12, fluidRow(
                   column(12, htmlOutput('coex.message')),
                   column(12, plotOutput('coex.plot', width='640px', height='640px'))))),
          column(4, wellPanel(
            h4("Highlight data set(s)"), 
            DT::dataTableOutput('coex.dataset', width='100%'),
            actionButton('coex.clear.ds', 'Clear selection')))))
      
      #####################################################################################################
      #####################################################################################################
            
    ) # end of navbarMenu
  }, 
 
  ######################################################################################## 
  ################################ "Advance" menu ########################################       
  {
  ####################
  navbarMenu(
    "Advance",
    ####################
    
    ############################################################################
    ############################# "Filter genes" tab ###########################
    tabPanel(
      "Filter genes", htmlOutput('filter.title'), br(),
      wellPanel( 
        fluidRow( 
        column(6, wellPanel( 
          h3('Specify comparison'),
          selectizeInput('filter.dataset', 'Select data set vs. other data sets', c(), width='75%'),
          selectizeInput('filter.group', 'Select group vs. other groups in the same data set', c(), width='75%'),
          selectizeInput('filter.sample', 'Select sample vs. other samples in the same data set', c(), width='75%')
        )),
        column(6, wellPanel( 
          h3('Specify cutoffs'),
          sliderInput('filter.absolute', 'Range of absolute expression level in percentile', 0, 100, c(0, 100), 1),
          sliderInput('filter.relative', 'Range of relative difference in percentile', 0, 100, c(0, 100), 1),
          radioButtons('filter.direction', NULL, c('Higher', 'Lower'), inline=TRUE)
        ))
      )),
      htmlOutput('filter.msg'), 
      DT::dataTableOutput('filter.table')
    ),
    
    ############################################################################
    ############################# "Re-group" tab ###############################
    tabPanel(
      "Re-group samples", htmlOutput('regroup.title'), br(),
      wellPanel(checkboxInput('regroup.check', 'Check box to activate sample re-grouping', FALSE)),
      fluidRow(
        column(6, 
               htmlOutput('regroup.ds.msg'), 
               DT::dataTableOutput('regroup.ds.tbl')), 
        column(6, wellPanel(
          h4("Step 1: select sample feature(s) to define data set"), 
          DT::dataTableOutput('regroup.dataset'), 
          actionButton('regroup.clear.ds', 'Clear selection')))),
      hr(),
      fluidRow(
        column(6, 
               htmlOutput('regroup.grp.msg'), 
               DT::dataTableOutput('regroup.grp.tbl')),
        column(6, wellPanel(
          h4("Step 2: select sample feature(s) to define group"), 
          DT::dataTableOutput('regroup.group'), 
          actionButton('regroup.clear.grp', 'Clear selection'))))
    ),
    
    ############################################################################
    ############################# "User data" tab ##############################
    tabPanel("Upload user data", htmlOutput('upload.title')))
  },
  
  ############################################################################## 
  ################################# "About" tab ################################
  # "About" tab
  tab.home<-tabPanel("About", id = 'tab.home',
                     wellPanel(style = "background-color: #ffffff;", includeMarkdown('./intro.Rmd')))
  
  ) # end of navbarPage
)) # end of shinyUI
