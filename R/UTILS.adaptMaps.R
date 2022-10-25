### HEADER #####################################################################
##' @title Adapt all raster maps of a \code{FATE} simulation folder (change NA 
##' to 0, and save as .tif)
##' 
##' @name .adaptMaps
##' 
##' @author Maya Guéguen
##' 
##' @description This function scan all the raster files within a \code{FATE} 
##' simulation folder, change all NA values to 0, potentially reproject onto 
##' a raster mask and save them with the specified extension.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param opt.name.file (\emph{optional}) \cr a \code{string} corresponding  
##' to the complete or partial name of the file in which to search and change 
##' the pattern
##' @param extension.old a \code{string} corresponding to the extension of 
##' raster files to be found
##' @param extension.new (\emph{optional}) \cr a \code{string} (either 
##' \code{tif} or \code{img}) corresponding to the new extension to save all 
##' the maps
##' @param opt.name.MASK (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster 
##' mask, with either \code{0} or \code{1} within each pixel, \code{1} 
##' corresponding to the cells of the studied area in which the succession 
##' (core) module of the \code{FATE} simulation will take place (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' 
##' 
##' @export
##' 
##' @importFrom raster raster projection projectRaster writeRaster
##'
## END OF HEADER ###############################################################


.adaptMaps = function(name.simulation
                      , opt.name.file = NULL
                      , extension.old
                      , extension.new = NULL
                      , opt.name.MASK = NULL
                      
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  .testParam_notChar.m("extension.old", extension.old)
  if (!is.null(extension.new)){
    .testParam_notInValues.m("extension.new", extension.new, c("tif", "img"))
  }
  
  all.files = list.files(path = paste0(name.simulation, "/DATA")
                         , pattern = paste0(".", extension.old, "$")
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    stop(paste0("Missing data!\n The folder ", name.simulation
                , "/DATA does not contain adequate files (.", extension.old, ")"))
  }
  
  if (is.null(opt.name.file) ||
      (!is.null(opt.name.file) && !is.character(opt.name.file)) ||
      (!is.null(opt.name.file) && nchar(opt.name.file) == 0)){
    warning("As `opt.name.file` does not contain character value, it will be ignored")
  } else {
    all.files = all.files[grep(opt.name.file, all.files)]
    if (length(all.files) == 0){
      stop(paste0("Missing data!\n The folder ", name.simulation
                  , "/DATA does not contain adequate files (", opt.name.file, ")"))
    }
  }
  
  ## CHECK parameter opt.name.MASK
  if (!.testParam_notDef(opt.name.MASK))
  {
    .testParam_notChar.m("opt.name.MASK", opt.name.MASK)
    .testParam_existFile(opt.name.MASK)
    ras.mask = raster(opt.name.MASK)
  }
  
  for (fi in all.files)
  {
    ras = raster(fi)
    ## Reproject to study area
    if (exists("ras.mask")){
      proj.method = "bilinear"
      if (sum(unique(ras[]) %in% c(0,1)) == 2 || length(unique(ras[])) < 10){
        proj.method = "ngb"
      }
      if (is.na(projection(ras))){
        projection(ras) = projection(ras.mask)
      }
      ras = projectRaster(from = ras, to = ras.mask, method = proj.method)
    }
    
    ## Change potential NA values to 0
    ind_na = which(is.na(ras[]))
    if (length(ind_na) > 0) ras[ind_na] = 0
    
    ## Rewrite raster, changing potentially extension
    new_fi = fi
    if (!is.null(extension.new)){
      new_fi = sub(paste0(".", extension.old, "$"), paste0(".", extension.new), fi)
    }
    writeRaster(ras, filename = new_fi, overwrite = TRUE)
    message(paste0("The raster file ", fi
                   , " has been successfully changed and saved ("
                   , new_fi, ") !"))
  }
}
