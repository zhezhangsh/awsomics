#####################
####### GeEx ########
#####################


print("loading UI");
print(system.time(source("/zhangz/awsomics/geex/preload.R")));

shinyUI(
  navbarPage(
    id="GeEx", collapsible = TRUE, title='GeEx',
    
    ############################################################################## 
    ################################ "Home" tab ################################ 
    tabPanel(
      "Home", htmlOutput('home.title'),  htmlOutput('home.message'),                     
      DT::dataTableOutput('home.table'),
      ######################################### Search panel ######################################### 
      absolutePanel(
        top = 60, right = 10, width=275, draggable = TRUE, style = "opacity: 1", wellPanel(
          selectInput("select.collection", "Select & load a collection:", width='100%', c('', select.collection))
          #, submitButton("Load", icon=icon('level-down'))
        )
      ) # end of absolutePanel
      ################################################################################################             
    ),
    
    ############################################################################## 
    ################################# "Introduction" tab ################################# 
    # "Introduction" tab
    tab.home<-tabPanel(
      "Introduction", id = 'tab.home',
      wellPanel(style = "background-color: #ffffff;", 
                helpText(h2("Introduction")),
                helpText("This is a demo of GeEx, Gene expression Explorer"),
                helpText("GeEx provides access, analysis, and visualization of collections of transcriptome data sets, and functions to "))
    ),
    
    ############################################################################## 
    ################################ "Browse" tab ################################     
    tabPanel(
      "Browse", htmlOutput('browse.title'), htmlOutput('browse.message'),
      DT::dataTableOutput('browse.table'),
      absolutePanel(
        top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1", wellPanel(
          selectInput("browse.options", "Select table to show:", c())
          #, submitButton("List", icon=icon('list'))
      )) # end of absolutePanel  
    ),

    ####################
    navbarMenu(
      "Analysis",
      ####################
   
      #######################################################################
      ############################# "PCA" tab ###############################
      tabPanel(
        "Principal components analysis", htmlOutput('pca.title'), hr(),
        fluidRow(
            column(8, 
                   fluidRow(
                     column(4, wellPanel(
                       h5("Select principal components"),
                       column(6, selectizeInput("pca.x", NULL, c())),
                       column(6, selectizeInput("pca.y", NULL, c())),
                       h5("Select colors"),
                       selectizeInput("pca.color", NULL, GetColorTypes()))),
                     column(4, wellPanel(selectizeInput("pca.dataset", "Select data set", c(), width='100%'))),
                     column(4, wellPanel(checkboxGroupInput("pca.group", label='Select group(s)', choices=c())))),
                   hr(),
                   htmlOutput('pca.message'),
                   plotOutput('pca.plot')),
            column(4, wellPanel(
              h5('Select sample(s) to highlight'),
              DT::dataTableOutput('pca.table'))))   
      ), # end of tabPanel     
      

      ############################################################################## 
      ############################# "Barplot" tab ###############################
      tabPanel(
        "Single gene expression", htmlOutput('bar.title'), hr(),
        fluidRow(
          column(8, 
                 fluidRow(
                   column(4, wellPanel(
                     selectizeInput("bar.scale", "Select data scale", c('logged', 'unlogged', 'percentile')),
                     selectizeInput("bar.color", "Select colors", GetColorTypes()))),
                   column(4, wellPanel(
                     selectizeInput("bar.dataset", "Select data set", c(), width='100%'))),
                   column(4, wellPanel(
                     checkboxGroupInput("bar.group", label='Select group(s)', choices=c()), div(), 
                     checkboxInput("bar.mean", HTML(geex.html.msg("Plot group mean")))))),
                 hr(),
                 htmlOutput('bar.message'),
                 plotOutput('bar.plot', width='100%')),
          column(4, 
                 wellPanel(
                   h3('Select gene(s)'),
                   checkboxInput("bar.selected", HTML(geex.html.msg("Show selected only"))),
                   DT::dataTableOutput('bar.table'), 
                   actionButton("bar.clear", 'Clear selection')), 
                 wellPanel(
                   h4('Look up gene:'),
                   fluidRow(
                     column(6, 
                            textInput("lookup.key", "", ''),
                            selectizeInput("lookup.species", '', choices=c())),
                     column(6, DT::dataTableOutput('lookup.table')))))
                   
                   
          )), # end of tabPanel     
      
      ############################################################################## 
      ############################# "Compare" tab ###############################
      tabPanel(
        "2-group differential expression", htmlOutput('two.title'), hr(),
        fluidRow(
          column(8, fluidRow(
            column(4, wellPanel(
              selectizeInput("two.type", "Select plot type", PlotPairDiffType()),
              selectizeInput("two.scale", "Select data scale", c('logged', 'unlogged', 'percentile')),
              selectizeInput("two.select.species", "Select species", c()))),
            column(4, wellPanel(
              h4('X-axis'),
              selectizeInput("x.dataset", "Data set", c(), width='100%'),
              selectizeInput("x.group", "Sample group", c(), width='100%'))),
            column(4, wellPanel(
              h4('Y-axis'),
              selectizeInput("y.dataset", "Data set", c(), width='100%'),
              selectizeInput("y.group", "Sample group", c(), width='100%')))),
            hr(),
            htmlOutput('two.message'),
            plotOutput('two.plot', width='100%')),
          column(4,
                 wellPanel(
                   h4('Highlight gene(s)'),
                   DT::dataTableOutput('two.table', width='100%'), 
                   actionButton("two.clear", 'Clear selection')), 
                 wellPanel(
                   h4('Highlight gene set(s)'),
                   column(3, h5("Source:")), 
                   column(8, selectizeInput("two.source", NULL, names(geneset), selected=names(geneset)[1], options=list('caption-side'='bottom'))),
                   column(3, h5("Collection:")), 
                   column(8, selectizeInput("two.coll", NULL, names(geneset[[1]]), selected=names(geneset[[1]])[1])),
                   column(3, h5("Species:")), 
                   column(8, selectizeInput("two.species", NULL, names(geneset[[1]][[1]]), selected=names(geneset[[1]][[1]])[1])),
                   DT::dataTableOutput('two.table.geneset', width='100%'), 
                   actionButton("two.clear.geneset", 'Clear selection'))))
      ) # end of tabPanel    
      
      #############################################################################################################################
      #############################################################################################################################
            
    ) # end of navbarMenu
  ) # end of navbarPage
) # end of shinyUI


