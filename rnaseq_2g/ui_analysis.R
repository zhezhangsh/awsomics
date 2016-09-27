#########################################################################################################
tabPanel(
  "Analysis", 
  titlePanel(h1(HTML('Welcome to <i><u>RNA-seq 2G</u></i>'))), 
  h4(HTML('<font color="grey">An online portal for analysis of 2-group differential expression</font>')),
  hr(),
  h2(HTML('<font color="orange"><center>Set up your analysis with step 1-2-3</center></font>')),
  
  #########################################################################
  #***** STEP 1 ******#
  wellPanel(
    h4(HTML('Step 1. Upload a read count matrix')),
    p(HTML(" - Rows and columns are correspondingly genes and samples.")), 
    div(style="display: inline-block;", HTML(" - Accept ")),
    div(style="display: inline-block;", downloadLink('analysis.step1.txt', '.txt, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.csv', '.csv, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.rds', '.rds, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.rdata', '.rdata, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.rda', '.rda, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.xlsx', '.xlsx, ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.xls', '.xls, ')),
    div(style="display: inline-block;", HTML('or ')),
    div(style="display: inline-block;", downloadLink('analysis.step1.html', '.html ')),
    div(style="display: inline-block;", HTML('file.')),
    div(style="display: inline-block;", HTML("(Click links to download sample data)")),
    p(),
    
    fluidRow(column(6, fileInput(inputId = 'analysis.step1.upload', label = NULL))),
    p(),
    
    h5(HTML('Loaded data:')),
    wellPanel(
      DT::dataTableOutput('analysis.step1.table', width='100%'),  
      htmlOutput('analysis.step1.size'),
      style='background-color: #FFFFFF'
    )  
  ),    
  
  #########################################################################
  #***** STEP 2 ******#
  # groups, paired, missing value, normalization
  wellPanel(
    h4(HTML('Step 2. Set analysis parameters')),
    p(HTML(" - Sample names must match column names in matrx.")), 
    p(HTML(" - Paired test requires the same number of samples in both group.")), 
    p(HTML(" - Paired test requires the paired samples to be in the same order.")), 
    p(HTML(" - Genes with total read counts less than the minimum will be excluded from the results.")), 
    p(HTML(" - Normalization will only applied to methods without their own internal normalization.")),         
    
    wellPanel(fluidRow(
      column(
        4, 
        h4(HTML('Name the groups and samples or load a file:')),
        div(style="display: inline-block;", HTML(" - Accept ")),
        div(style="display: inline-block;", downloadLink('analysis.step2.txt', '.txt, ')),
        div(style="display: inline-block;", downloadLink('analysis.step2.csv', 'csv, ')),
        div(style="display: inline-block;", downloadLink('analysis.step2.rds', '.rds, ')),
        div(style="display: inline-block;", downloadLink('analysis.step2.rdata', '.rdata, ')),
        div(style="display: inline-block;", downloadLink('analysis.step2.xls', '.xls, ')),
        div(style="display: inline-block;", HTML('or ')),
        div(style="display: inline-block;", downloadLink('analysis.step2.xlsx', '.xlsx ')),
        div(style="display: inline-block;", HTML('file.')), 
        p(),
        fileInput(inputId = 'analysis.step2.group', label = NULL)
      ),
      column(
        8, 
        fluidRow(
          column(3, textInput(inputId = 'analysis.step2.groupA',  label='Control group', value = 'Control', width = '80%')),
          column(9, textInput(inputId = 'analysis.step2.sampleA', label='Control samples', value = '', width = '90%'))
        ),
        fluidRow(
          column(3, textInput(inputId = 'analysis.step2.groupB',  label='Case group', value = 'Case', width = '80%')),
          column(9, textInput(inputId = 'analysis.step2.sampleB', label='Control samples', value = '', width = '90%'))
        )
      )
    )),
    p(),
    
    fluidRow(
      column(
        6, 
        fluidRow(
          column(6, radioButtons('analysis.step2.paired', label = 'Paired test', choices = c('Unpaired', 'Paired'), inline = TRUE)),
          column(6, selectizeInput('analysis.step2.norm1', label = 'Normalize count', choices = norm.count, width = '80%'))
        ),
        fluidRow(
          column(6, radioButtons('analysis.step2.missing', label = 'Missing value', choices = c('Remove gene', 'Replace with 0'), inline = TRUE)),
          column(6, selectizeInput('analysis.step2.norm2', label = 'Normalize logged', choices = norm.logged, width = '80%'))
        )
      ),
      column(6, br(), sliderInput('analysis.step2.filter', label = 'Minimal read count', 0, 60, 6, 1, width = '90%'))
    )
  ), # End of step2 panel
  
  #########################################################################
  #***** STEP 3 ******#
  # groups, paired, missing value, normalization
  wellPanel(
    h4(HTML('Step 3. Choose one or multiple methods')),
    p(HTML(" - The default methods take about 1 minute to finish.")), 
    p(HTML(" - Please be patient if you choose any slow methods.")), 
    
    #uiOutput('analysis.step3.sel'),
    wellPanel(
      checkboxGroupInput('analysis.step3.selection', label = 'Select DE methods:', choices = DeRNAseqMs[[1]], 
                         selected = NULL, inline = TRUE),
      hr(),
      radioButtons('analysis.step3.group', 'Or pick a method group:', method.group, selected = NULL, inline = TRUE, width = '90%')
    ),
    
    uiOutput('analysis.step3.panel1'),
    uiOutput('analysis.step3.panel2')
  ),
  
  #########################################################################
  p(),
  wellPanel(fluidRow(
    div(style="display: inline-block;", HTML("&nbsp&nbsp&nbsp")),
    div(style="display: inline-block;", actionButton("analysis.run", 'Run DE analysis', width='100%', icon("paper-plane"), style=button.style)),
    div(style="display: inline-block;", HTML("&nbsp&nbsp&nbsp")),
    div(style="display: inline-block;", htmlOutput("analysis.run.message"))
  ))
)