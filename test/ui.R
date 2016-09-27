library("shiny")

# ui
shinyUI(
fluidPage(
  tags$h1("Table with checkboxes"),
  tableOutput(outputId = "table"),
  br(),
  verbatimTextOutput(outputId = "out")
)
)
