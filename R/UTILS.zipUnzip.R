### HEADER #####################################################################
##' @title Compress (\code{.tif}, \code{.img}) or decompress (\code{.gz}) files 
##' contained in results folder
##' 
##' @name .unzip_ALL
##' @aliases .unzip
##' @aliases .zip_ALL
##' @aliases .zip
##' 
##' @usage
##' .unzip_ALL(folder_name, no_cores)
##' .unzip(folder_name, list_files, no_cores)
##' .zip_ALL(folder_name, no_cores)
##' .zip(folder_name, list_files, no_cores)
##'
##' @description These functions compress (\code{.tif}, \code{.img}) or 
##' decompress (\code{.gz}) files contained in a given folder.
##' 
##' @param folder_name a \code{string} corresponding to the directory to be 
##' scanned
##' @param list_files a \code{vector} containing filenames to be compress or 
##' decompress, in order not to scan the whole given directory
##' @param no_cores default \code{1}. \cr an \code{integer} corresponding to the 
##' number of computing resources that can be used to parallelize the 
##' (de)compression
##' 
##' @importFrom parallel mclapply
##' @importFrom R.utils gunzip gzip
##' @importFrom utils txtProgressBar setTxtProgressBar
##'
## END OF HEADER ###############################################################

NULL

##' @export

.unzip_ALL = function(folder_name, no_cores = 1)
{
  list_files = list.files(folder_name, pattern = ".gz$", full.names = FALSE)
  .unzip(folder_name = folder_name, list_files = list_files, no_cores = no_cores)
}



##' @export

.unzip = function(folder_name, list_files, no_cores = 1)
{
  list_files = paste0(folder_name, list_files)
  list_files = grep(pattern = ".gz$", list_files, value = TRUE)
  list_files = list_files[file.exists(list_files)]
  if (length(list_files) > 0)
  {
    cat("\n UNZIP RASTER FILES from repository ", folder_name, "...\n")
    if (no_cores > 1 && .getOS() == "windows")
    {
      no_cores = 1
      warning("Parallelisation is not available for Windows. Sorry.")
    }
    PROGRESS = txtProgressBar(min = 0, max = length(list_files), style = 3)
    mclapply(1:length(list_files), function(x) {
      setTxtProgressBar(pb = PROGRESS, value = x)
      gunzip(list_files[x], skip = TRUE, remove = FALSE)
    }, mc.cores = no_cores)
    close(PROGRESS)
  }
}



##' @export

.zip_ALL = function(folder_name, no_cores = 1)
{
  list_files = list.files(folder_name, pattern = ".tif$|.img$", full.names = FALSE)
  .zip(folder_name = folder_name, list_files = list_files, no_cores = no_cores)
}



##' @export

.zip = function(folder_name, list_files, no_cores = 1)
{
  list_files = paste0(folder_name, list_files)
  list_files = grep(pattern = ".tif$|.img$", list_files, value = TRUE)
  list_files = list_files[file.exists(list_files)]
  if (length(list_files) > 0)
  {
    cat("\n ZIP RASTER FILES from repository ", folder_name, "...\n")
    if (no_cores > 1 && .getOS() == "windows")
    {
      no_cores = 1
      warning("Parallelisation is not available for Windows. Sorry.")
    }
    PROGRESS = txtProgressBar(min = 0, max = length(list_files), style = 3)
    mclapply(1:length(list_files), function(x) {
      setTxtProgressBar(pb = PROGRESS, value = x)
      gzip(list_files[x], skip = TRUE, remove = TRUE)
      if (file.exists(list_files[x])) file.remove(list_files[x])
    }, mc.cores = no_cores)
    close(PROGRESS)
  }
}
