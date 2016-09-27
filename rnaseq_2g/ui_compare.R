#########################################################################################################    
tabPanel(
  "Comparison",
  titlePanel(h2(HTML('<font color="orange"><center>Compare methods</center></font>'))), 
  wellPanel(fluidRow(
    h3(HTML('&nbsp&nbsp&nbspCompare global pattern between 2 methods')),
    column(
      6, 
      wellPanel(fluidRow(
        column(6, selectInput("compare.select.table1", label = "Select method", choices = list(), width = '75%')),
        column(6, selectInput('compare.select.plot1', 'Select plot', plot.type, selected = 1, width = '75%'))
      )),
      plotOutput('compare.show.plot1', width = '100%', height = '480px')
    ),
    column(
      6, 
      wellPanel(fluidRow(
        column(6, selectInput("compare.select.table2", label = "Select method", choices = list(), width = '75%')),
        column(6, selectInput('compare.select.plot2', 'Select plot', plot.type, selected = 1, width = '75%'))
      )),
      plotOutput('compare.show.plot2', width = '100%', height = '480px')
    )
  )),
  wellPanel(fluidRow(
    h3(HTML('&nbsp&nbsp&nbspCompare p values and their ranking between 2 methods')),
    column(
      7,
      wellPanel(
        DT::dataTableOutput('compare.table.pv', width='100%')
      )
    ),
    column(
      5,
      fluidRow(
        column(12, align='center', radioButtons('compare.pv.type', NULL, list('Quantile-quantile'='1', 'P value ranking'='2'), selected = '1', inline = TRUE))
      ),
      plotOutput('compare.plot.pv', width = '100%', height = '640px')
    )
  )),
  wellPanel(fluidRow(
    h3(HTML("&nbsp&nbsp&nbspCompare results of a single gene between methods")),
    column(
      7, 
      wellPanel(
        fluidRow(
          div(style="display:inline-block;", HTML("&nbsp;&nbsp;&nbsp;")),
          div(style="display:inline-block; outline:100px;", textInput("compare.single.id", NULL, value = "")),
          div(style="display:inline-block;", HTML("&nbsp;&nbsp;&nbsp;")),
          div(style="display:inline-block", htmlOutput("compare.single.msg")),
          wellPanel(DT::dataTableOutput('compare.single.table', width='100%'))
        )
      )
    ),
    column(
      5,
      fluidRow(
        column(
          12, align='center', 
          radioButtons('compare.single.type', NULL, list('Original count'='1', 'Normalized count'='2', 'Log2-transformed'='3'), 
                       selected = '2', inline = TRUE))
      ),
      plotOutput('compare.single.plot', width = '100%', height = '480px')
    )
  ))
)
