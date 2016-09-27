#########################################################################################################    
tabPanel(
  "Result",
  titlePanel(h2(HTML('<font color="orange"><center>Browse DE results</center></font>'))), 
  wellPanel(
    fluidRow(
      h3(HTML("&nbsp&nbsp&nbspLoad previous results")),
      div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp&nbspLoad from server&nbsp&nbsp"))),
      div(style="display: inline-block;", textInput("result.previous.id", NULL, value = "Enter analysis ID ...")),
      div(style="display: inline-block;", actionButton("result.previous.load", 'Load', icon("upload"), style=button.style)),
      div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbspOr Upload an <i>.rds</i> file &nbsp"))),
      div(style="display: inline-block;", fileInput(inputId = "result.previous.upload", label = NULL)),
      htmlOutput("result.previous.info")
    )
  ),
  fluidRow( 
    column(
      8,
      wellPanel(
        h3(HTML("Show and filter results")),
        wellPanel(fluidRow(
          div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", selectInput("result.select.table", label = "Select method", choices=list(), width='120px')),
          div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", selectInput("result.filter.p", label = 'P value', choices = choices.pv, width='80px')),
          div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", selectInput("result.filter.fc", label = 'Fold change', choices = choices.fc, width='100px')),   
          div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", h4(HTML('<center>Download</center>')),
              radioButtons('result.download.format', NULL, c('R', 'Text', 'Excel'), inline=TRUE)),
          div(style="display: inline-block;", h4(HTML("&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"))),
          div(style="display: inline-block;", 
              tags$head(tags$style(".dB{float:center;} .dB{background-color: #337ab7;} .dB{border-color: #2e6da4;} .dB{color: #ffffff} .dB{height: 80%;}")),
              downloadButton('result.download.current', label = 'This table', class = 'dB'), downloadButton('result.download.all', label = 'All results', class = 'dB'))
        )),
        wellPanel(fluidRow(column(12, DT::dataTableOutput('result.show.table', width='100%'))))
      )
    ), # End of showing table panel
    column(
      4,
      wellPanel(
        h3(HTML("Plot statistics")),
        wellPanel(fluidRow(
          column(4, selectInput('result.select.plot', 'Select plot', plot.type, selected = 1)),
          column(
            8, align='center',
            tags$head(tags$style(".dB{float:center;} .dB{background-color: #337ab7;} .dB{border-color: #2e6da4;} .dB{color: #ffffff}")),
            downloadButton('result.download.plot', label = 'Download plot', class = 'dB'), p(),
            radioButtons('result.download.plottype', label = NULL, choices = c('pdf', 'png', 'jpeg', 'tiff'), selected = 'pdf', inline=TRUE)
          )
        )),
        plotOutput('result.show.plot', width = '100%', height = '480px')
      )
    ) # End of showing plot panel
  )
)