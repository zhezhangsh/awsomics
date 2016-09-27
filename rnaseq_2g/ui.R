##########################
####### RNAseq-2G ########
##########################

print("loading UI");

source("/zhangz/awsomics/rnaseq_2g/preload.R");

shinyUI(
  
  navbarPage(
    title = "RNA-seq 2G",
    id    = "main_menu",
    theme = shinytheme("united"),
    
    source('ui_analysis.R', local=TRUE)$value, 
    source('ui_result.R', local=TRUE)$value, 
    source('ui_compare.R', local=TRUE)$value,
    tabPanel(
      "Manual", 
      wellPanel(includeHTML("html/index.html"), style = "background-color: #EEEEEE;")
    )    
  )

) # end of shinyUI


