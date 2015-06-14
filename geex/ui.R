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
      "Visualization",
      ####################
      
      ############################################################################## 
      ############################# "PCA" tab ###############################
      tabPanel(
        "PCA", htmlOutput('pca.title'), htmlOutput('pca.message'),
        plotOutput('pca.plot'),
        
        absolutePanel(
          top = 60, right = 10, width=360, draggable = TRUE, style = "opacity: 1", wellPanel(
            selectizeInput("pca.dataset", "Select a data set:", c(), width='100%'),
            checkboxGroupInput("pca.group", "", c()))
        )       
      ) # end of tabPanel                   
    ) # end of navbarMenu
  ) # end of navbarPage
) # end of shinyUI


