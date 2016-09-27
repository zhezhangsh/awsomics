print("starting server");

source('server_analysis.R', local=TRUE);
source('server_result.R',   local=TRUE);
source('server_compare.R',  local=TRUE);

shinyServer(function(input, output, session) {
  cat("new visitor: ", session$token, '\n');
  
  session.data <- reactiveValues(matrix=NULL, result=NULL, show=0);
  session.dir  <- paste(APP_HOME, 'log', session$token, sep='/');
  if (!file.exists(session.dir)) dir.create(session.dir, recursive = TRUE);
  
  session.data <- server_analysis(input, output, session, session.data);
  session.data <- server_result(input, output, session, session.data);
  session.data <- server_compare(input, output, session, session.data);
});

