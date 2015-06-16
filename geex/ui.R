#####################
####### GeEx ########
#####################


print("loading UI");
print(system.time(source("/zhangz/awsomics_dev/geex/geex/preload.R")));

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
      wellPanel(style = "background-color: #ffffff;", helpText("Introduction"))
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

      
      ############################################################################## 
      ############################# "PCA" tab ###############################
      tabPanel(
        "Principal components analysis", htmlOutput('pca.title'), htmlOutput('pca.message'),
        fluidRow(
            column(8, plotOutput('pca.plot', width='100%', height='100%')),
            column(4, wellPanel(
              selectizeInput("pca.dataset", "Select a data set:", c(), width='100%'),
              h5('Sample list:'),
              DT::dataTableOutput('pca.table'))))   
      ), # end of tabPanel     
      
      ############################################################################## 
      ############################# "Scatter" tab ###############################
      tabPanel(
        "Compare sample groups", htmlOutput('scatter.title'), htmlOutput('scatter.message'),
        fluidRow(
          column(8, fluidRow(
            column(6, wellPanel(
              h3('X-axis'),
              selectizeInput("x.dataset", "Data set", c(), width='100%'),
              selectizeInput("x.group", "Sample group", c(), width='100%'))),
            column(6, wellPanel(
              h3('Y-axis'),
              selectizeInput("y.dataset", "Data set", c(), width='100%'),
              selectizeInput("y.group", "Sample group", c(), width='100%'))),
            column(12, plotOutput('scatter.plot', width='100%'))
            )),
          column(4, wellPanel(
            checkboxInput('scatter.show.gene', 'Show genes', FALSE),
            DT::dataTableOutput('scatter.table'))))
      ), # end of tabPanel    
      
      ############################################################################## 
      ############################# "Barplot" tab ###############################
      tabPanel(
        "Gene expression level", htmlOutput('bar.title'), htmlOutput('bar.message'),
        fluidRow(
          column(6, plotOutput('bar.plot', width='100%')),
          column(3, 
                 wellPanel(
                   selectizeInput("bar.scale", "Scale", c('logged', 'unlogged', 'percentile'), width='100%'),
                   checkboxInput("bar.mean", "Show group mean")),
                 wellPanel(
                   selectizeInput("bar.dataset", "Select a data set:", c(), width='100%'),
                   checkboxGroupInput("bar.group", label='', choices=c()))),
          column(3, wellPanel(DT::dataTableOutput('bar.table'))))       
      ) # end of tabPanel     
      
    ) # end of navbarMenu
  ) # end of navbarPage
) # end of shinyUI


