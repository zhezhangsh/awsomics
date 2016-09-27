library("shiny")
library(DEGandMore);
data(DeMethodMeta)
mymcars <- DeMethodMeta;

# server
shinyServer (function(input, output) {
  output$table <- renderFlexTable({
    # Create checkboxes
    mymtcars$Name <- paste0('<label><input type="checkbox" id="car', seq_along(rownames(mymtcars)), '"> <span>', rownames(mymtcars), '</span></label>')
    mymtcars <- mymtcars[c("Name", names(mtcars))] # Put col 'Name' in the first place
    ft <- vanilla.table(mymtcars) # convert to FlexTable objet
    ft[, "Name", to = "header"] <- parLeft() # left align checkboxes
    ft[, "Name"] <- parLeft() # left align header
    return(ft)
  })
  # the inputs created are in input$car1, input$car2, ...
  output$out <- renderPrint({
    # results
    res <- unlist(lapply(1:nrow(mymtcars), function(i) input[[paste0("car", i)]]))
    print(res)
    if (any(res)) {
      print(rownames(mymtcars)[res])
    }
  })
})