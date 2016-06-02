
#' GUI.help
#' 
#' @description  GUI.help
#' @import shiny   
GUI.help <-
  function() {
    # export 
    appDir <- system.file( "GUI",package="pMineR")
    if(!nzchar(appDir)) {
      stop("Could not find Shiny directory. Try re-installing 'DVHmetrics'.", call.=FALSE)
    }
    shinyApp(ui = client.test, server = server.test)
  }