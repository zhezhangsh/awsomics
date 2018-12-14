##########################
#### Scatter Viewer ######
##########################
require(RoCA);

shinyUI(
  pageWithSidebar(
    headerPanel('Scatterplot viewer'),
    
    sidebarPanel(
      fileInput('upload.data', label = 'Upload data file'),
      fluidRow(
        column(6, selectizeInput('axis.x', 'Select X-axis', choices = NULL)),
        column(6, selectizeInput('axis.y', 'Select Y-axis', choices = NULL))
      ),
      
      hr(),
      
      fileInput('upload.metadata', label = 'Upload metadata file'),
      
      radioButtons(inputId = 'primary', label = 'Select primary feature (Color):', choices=list(), selected=as.character()),
      div(style="display: inline-block;", h6(HTML('Highlight'))),
      div(style="display: inline-block; width: 160px", selectizeInput('highlight.primary', NULL, choices = NULL)),
      div(style="display: inline-block;", h4(HTML("&nbsp&nbsp"))),
      div(style="display: inline-block;", checkboxInput('only.primary', 'only', FALSE)),
      
      radioButtons(inputId = 'secondary', label = 'Select secondary feature (Shape):', choices=list(), selected=as.character()),
      div(style="display: inline-block;", h6(HTML('Highlight'))),
      div(style="display: inline-block; width: 160px;", selectizeInput('highlight.secondary', NULL, choices = NULL)),
      div(style="display: inline-block;", h4(HTML("&nbsp&nbsp"))),
      div(style="display: inline-block;", checkboxInput('only.secondary', 'only', FALSE))
    ),
    
    mainPanel(
      conditionalPanel(
        condition = 'input["highlight.primary"] != ""',
        div(style="display: inline-block;", 
            radioButtons('download.type', label = NULL, choices = c('pdf', 'png', 'tiff', 'jpeg'), selected = 'pdf', inline=TRUE)),
        div(style="display: inline-block;", h4(HTML("&nbsp&nbsp"))),
        div(style="display: inline-block;", 
            tags$head(tags$style(".dB1{float:center;} .dB1{background-color: tomato;} .dB1{border-color: black;} .dB1{height: 80%;}")),
            downloadButton('download.button', label = 'Download plot', class = 'dB1'))
      ),
      conditionalPanel(condition = 'input["highlight.primary"] != ""', plotOutput('scatterplot'))
    )
  )
)
