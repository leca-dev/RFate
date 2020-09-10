### HEADER #####################################################################
##' @title Find operating system of your computer
##' 
##' @name .getOS
##'
##' @description This functions finds on which operating sytem the user is 
##' currently using \code{RFate}. It notably allows to set the use of 
##' parallelisation.
##' 
##' @export
##'
## END OF HEADER ###############################################################


.getOS = function()
{
  sysinf = Sys.info()
  if (!is.null(sysinf))
  {
    os = sysinf['sysname']
    if (os == 'Darwin') os = "mac"
  } else
  {
    os = .Platform$OS.type
    if (grepl("^darwin", R.version$os)) os = "mac"
    if (grepl("linux-gnu", R.version$os)) os = "linux"
  }
  return(tolower(os))
}
