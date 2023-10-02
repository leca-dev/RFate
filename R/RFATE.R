### HEADER #####################################################################
##' @title Shiny application to apply \code{RFate} functions and run 
##' \code{FATE} simulation
##' 
##' @name RFATE
##'
##' @author Maya Guéguen
##' 
##' 
##' @description This \code{shiny} application allows to use all 
##' the \code{RFate} functions (\code{PRE_FATE}, \code{FATE} and 
##' \code{POST_FATE}), from the building of PFG to the treatment of \code{FATE} 
##' output files.
##'              
##'  
##' @keywords shiny application, interface, GUI
##'  
##' @export
##' 
## @importFrom shiny runApp
##' 
## END OF HEADER ###############################################################


RFATE = function()
{
  if (!isNamespaceLoaded("shiny")) { 
    if (!requireNamespace('shiny', quietly = TRUE)) stop("Package 'shiny' not found")
  }
  
  appDir <- system.file("shinyApp", package = "RFate")
  if (appDir == "") {
    stop("Could not find shinyApp directory. Try re-installing `RFate`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}

