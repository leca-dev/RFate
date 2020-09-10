### HEADER #####################################################################
##' @title Create a parameters file
##' 
##' @name .createParams
##'
##' @author Maya Guéguen
##' 
##' @description Create a text file containing value(s) for one or several 
##' parameters and separated by a same character value.
##' 
##' @param params.file a \code{string} corresponding to the name of the file 
##' that will be created
##' @param params.list a \code{list} containing all the parameters that will be 
##' included into \code{params.file}, and whose names correspond to the 
##' parameter names
##' @param separator a \code{string} to separate each parameter values within 
##' the parameter file
##'
## END OF HEADER ###############################################################

.createParams = function(params.file, params.list, separator = " ")
{
  if (missing(params.file) ||
      is.na(params.file) ||
      is.null(length(params.file)) ||
      !is.character(params.file) ||
      nchar(params.file) == 0)
  {
    stop("Wrong type of data!\n `params.file` must contain a character value of length > 0")
  } else
  {
    if (tail(strsplit(params.file, "[.]")[[1]], 1) != "txt")
    {
      stop("Wrong type of data!\n `params.file` must be a file name with .txt extension")
    }
    if (!dir.exists(dirname(params.file)))
    {
      stop("Wrong name file given!\n `params.file` directory (", dirname(params.file), ") does not exist")
    }
    if (file.exists(params.file))
    {
      warning(paste0("`params.file` (", params.file, ") already exists. It will be replaced."))
    }
  }
  if (missing(params.list) || !is.list(params.list))
  {
    stop("Wrong type of data!\n `params.list` must be a list")
  } else
  {
    if (is.null(names(params.list)))
    {
      stop("Wrong type of data!\n `params.list` must be a list with non-null names")
    }
  }
  

  
  text.to.paste = sapply(1:length(params.list), function(x){
    res = paste0(params.list[[x]], collapse = separator)
    res = paste0(names(params.list)[x], separator, res, "\n")
  })
  text.to.paste = paste0(text.to.paste, collapse = "")
  text.to.paste = paste0("## File automatically generated \n"
                         , "## Date : ", date(), "\n"
                         , text.to.paste)
  
  cat(text.to.paste, sep = "", file = params.file, append = FALSE)
  message(paste0("The parameter file ", params.file, " has been successfully created !"))
}
