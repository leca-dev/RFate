### HEADER #####################################################################
##' @title Adapt all raster maps of a \code{FATE} simulation folder (change NA 
##' to 0, and save as .tif)
##' 
##' @name .adaptMaps
##' 
##' @author Maya Guéguen
##' 
##' @description This function scan all the raster files within a \code{FATE} 
##' simulation folder, change all NA values to 0 and save them with the 
##' specified extension.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param extension.old a \code{string} corresponding to the extension of 
##' raster files to be found
##' @param extension.new a \code{string} (either \code{tif} or \code{img}) 
##' corresponding to the new extension to save all the maps
##' 
##' 
##' @examples 
##'
##' ## Load example data
##' 
##' @export
##' 
##' @importFrom raster raster writeRaster
##'
## END OF HEADER ###############################################################


.adaptMaps = function(name.simulation
                      , extension.old
                      , extension.new
                      
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  .testParam_notChar.m("extension.old", extension.old)
  .testParam_notInValues.m("extension.new", extension.new, c("tif", "img"))
  
  all.files = list.files(path = paste0(name.simulation, "/DATA")
                         , pattern = paste0(".", extension.old, "$")
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    stop(paste0("Missing data!\n The folder ", name.simulation
                , "/DATA does not contain adequate files (.", extension.old, ")"))
  }
  
  for (fi in all.files)
  {
    new_fi = sub(paste0(".", extension.old, "$"), paste0(".", extension.new), fi)
    ras = raster(fi)
    ras[which(is.na(ras))] = 0
    writeRaster(ras, filename = new_fi, overwrite = TRUE)
    message(paste0("\n The raster file ", fi
                   , " has been successfully changed and saved ("
                   , new_fi, ") !"))
  }
}
