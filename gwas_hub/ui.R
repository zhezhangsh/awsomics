print("loading UI");

print(system.time(source("/zhangz/awsomics/meta_gwas/preload.R")));

shinyUI(navbarPage("GWAS Hub", collapsible = TRUE, title='GWAS Hub',
                   
            ############################################################################## 
            ################################ "Search" tab ################################ 
            tabPanel("Search", 
                     htmlOutput('search.table.title'),
                     htmlOutput('search.table.message'),                     
                     DT::dataTableOutput('search.table'),
                     
                     ######################################### Search panel ######################################### 
                     absolutePanel(top = 60, right = 10, width=275, draggable = TRUE, style = "opacity: 1",            
                                   wellPanel(
                                     #################### Basic search options ###################
                                     textInput(inputId="search.value", label="", value=""), # input search key
                                     selectInput("search.options", "", width=225, search.options),
                                     submitButton("Search", icon=icon('search')),
                                     
                                     ################ Extra search options ####################### 
                                     # search options
                                     hr(),
                                     h3('Options'),   
                                     h5("Extend region (kilobases)"),
                                     #verbatimTextOutput('verbatim'),
                                     numericInput("extend.ends.value", "", 0, 0, 1000, 5), # extend search region at either side
                                     selectInput("extend.end", "", width=225, search.extend.options), # select side   
                                     selectInput("select.genome", h5("Genome version"), width=225, genome.versions), # select side   
                                     selectInput("search.restrict", h5("Limit search to"), width=250, search.restrict.options),                                  
                                     textInput("search.restrict.to", "", ""),                        
                                     textInput("max.phred.range", h5("Max Phred of analyses"), "30-300"), # Range of max phred scores of an analysis to be included
                                     checkboxGroupInput("exclude.table", h5("Exclude database table(s)"), exclude.tables),
                                     checkboxGroupInput("mask.p.values", h5("Hide p values?"), list('Yes')), # whether to mask p values in outputs                        
                                     submitButton("Search", icon=icon('search')),
                                     
                                     hr(),
                                     checkboxInput("check.refresh", h5("Clear all")),
                                     submitButton("Refresh", icon=icon('refresh')),
                                     
                                     h5('')
                                   )
                     ) # end of absolutePanel
                     ################################################################################################   
            ),
            ##############################################################################                 
                   
            ############################################################################## 
            ################################# "Introduction" tab ################################# 
            # "Introduction" tab
            tab.home<-tabPanel("Introduction", id = 'tab.home',
                  wellPanel(style = "background-color: #ffffff;", includeMarkdown('./intro.Rmd'))
            ),
            ############################################################################## 
            
            ############################################################################## 
            ################################ "Factoid" tab ############################### 
            tabPanel("Factoid", 
                     wellPanel(htmlOutput("factoid.text"), style = "background-color: #ffffff;"),
                     absolutePanel(top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1",                      
                                   wellPanel(
                                     selectInput("factoid.options", "", width=200, factoid.types), # select search type
                                     textInput(inputId="factoid.key", label="", value=""), # input search key
                                     submitButton("Search", icon=icon('search'))
                                   )
                     )
            ),
            ############################################################################## 
            
            ############################################################################## 
            ################################# "Browse" tab ############################### 
            tabPanel("Browse",
                    htmlOutput('browse.table.title'),
                    DT::dataTableOutput('browse.table'),
                    absolutePanel(top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1",                        
                                  wellPanel(
                                    #style = "background-color: #ffffee;",
                                    selectInput("browse.options", "", names(browse.tbls)),
                                    submitButton("List", icon=icon('list'))
                                  )                 
                    ) # end of absolutePanel                     
            ),
            ############################################################################## 
            
          ####################
          navbarMenu("Secondary analyses", #
          ####################
          
            ############################################################################## 
            ############################# "Summary" tab ###############################
            tabPanel("Summary",
                   htmlOutput('summary.table.title'),
                   DT::dataTableOutput('summary.table'),
                   absolutePanel(top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1",                        
                                 wellPanel(
                                   selectInput("summary.options", "", width=200, summary.options), # select search type
                                   sliderInput('summary.minimum.phred', h5('Minimum Phred'), 0, 60, 20, step=1),
                                   submitButton("Refresh", icon=icon('refresh'))
                                 )                 
                   ) # end of absolutePanel                     
            ),
            ############################################################################## 
          
            ############################################################################## 
            ############################# "QQ plot" tab ############################### 
            tabPanel("QQ plot", width=400, height=400,
                     htmlOutput('qqplot.title'),
                     plotOutput('qqplot'),
 
                     absolutePanel(top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1",                        
                                   wellPanel(
                                     checkboxGroupInput("qq.highlight", h5("Highlight (Max Phred):"), list()),
                                     submitButton("Refresh", icon=icon('refresh'))
                                   )                 
                     ) # end of absolutePanel     
            ),
            ############################################################################## 
          
          ############################################################################## 
          ############################# "Manhattan plot" tab ############################### 
          tabPanel("Manhattan", 
                   htmlOutput('manhattan.plot.title'),
                   plotOutput('manhattan.plot', width='85%', height=400) 
          ),
          ############################################################################## 

          ############################################################################## 
          ############################# "Bicluster plot" tab ############################### 
#           tabPanel("Bicluster", 
#                    htmlOutput('bicluster.plot.title'),
#                    plotOutput('bicluster.plot', width='85%', height=400) 
#           ),
          ############################################################################## 
               
          ############################################################################## 
          ############################# "Enrichment" tab ###############################
          tabPanel("Enrichment",
                   htmlOutput('enrichment.table.title'),
                   DT::dataTableOutput('enrichment.table'),
                   absolutePanel(top = 60, right = 10, width=240, draggable = TRUE, style = "opacity: 1",                        
                                 wellPanel(
                                   selectInput("keyword.options", "", width=200, names(key.enrich)), # select search type
                                   sliderInput('enrich.minimum.phred', h5('Minimum Phred'), 0, 60, 20, step=1),
                                   sliderInput('enrich.minimum.count', h5('Minimum occurance'), 1, 25, 5, step=1),
                                   submitButton("Refresh", icon=icon('refresh'))
                                 )                 
                   ) # end of absolutePanel                     
          )
          ############################################################################## 
          
           
        ) # end of menu "Secondary analysis"    
) ) # end of shinyUI


