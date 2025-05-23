### HEADER #####################################################################
##' @title Load a \code{R} package and install it if necessary
##' 
##' @name .loadPackage
##'
##' @description This function loads a \code{R} package and install it if 
##' necessary.
##' 
##' @param package.name a \code{string} corresponding to the name of the package 
##' that will be loaded or installed
##' 
##' @keywords internal
##'
##' @export
##'
##' @importFrom utils install.packages packageDescription read.delim
##'
## END OF HEADER ###############################################################

.loadPackage = function(package.name)
{
  if (missing(package.name) || is.na(package.name) || length(package.name) == 0){
    stop("No data given!\n (missing `package.name`)")
  } else if (!is.character(package.name)){
    stop("Wrong type of data!\n `package.name` must contain a character value")
  } else {
    load.package = requireNamespace(package.name)
    if (!load.package)
    {
      cat("\n > Installing `",package.name,"` package...\n")
      install.packages(package.name)
      load.package = requireNamespace(package.name)
      if (!load.package)
      {
        stop(paste0("Installation of `",package.name,"` package failed!"))
      } else
      {
        message(paste0("Installation of `",package.name,"` package succeeded!"))
      }
    }
  }
}


.onAttach = function(libname, pkgname)
{
  actual = packageDescription(pkgname)[["Version"]]
  previouswarn = getOption("warn")
  options(warn = 2)
  webpage = "https://raw.githubusercontent.com/leca-dev/RFate/master/DESCRIPTION"
  description = try(read.delim(webpage, header = F, stringsAsFactors = F), silent = TRUE)
  
  m = paste("Welcome in package", pkgname, "!")
  m = paste0(m, "\n Support functions for the software FATE.")
  # m = paste0(m, "\n Your version is ", actual, ".")
  
  if (!inherits(description, "try-error"))
  {
    recent = description[grep("Version:", description[, 1]), 1]
    recent = strsplit(recent, " ")[[1]][2]

    m = paste0(m, "\n Your version is ", actual, ".")
    if (recent != "" && actual != recent)
    {
      m = paste0(m, " Most recent is ", recent, ".")
    } else
    {
      # m = paste0(m, "\n No internet connection is available to check for update.")
    }
  } else
  {
    # m = paste0(m, "\n No internet connection is available to check for update.")
  }
  m = paste0(m, "\n More informations can be find here :")
  m = paste0(m, "\n - https://leca-dev.github.io/RFate/")
  
  packageStartupMessage(m)
  options(warn = previouswarn)
}

.onUnload = function (libpath)
{
  library.dynam.unload("RFate", libpath)
}
