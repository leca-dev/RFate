### HEADER #####################################################################
##' @title Upscale / crop all raster maps of a \code{FATE} simulation folder
##' 
##' @name .upscaleMaps
##' @aliases .cropMaps
##' 
##' @usage
##' .upscaleMaps(name.simulation, resolution)
##' .cropMaps(name.simulation, extent)
##'
##' @author Maya Guéguen
##' 
##' @description These functions scan all the raster files within a 
##' \code{FATE} simulation folder and upscale / crop them to the specified 
##' resolution / extent.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param resolution an \code{integer} corresponding to the new resolution to 
##' upscale all the maps
##' @param extent a \code{vector} of 4 \code{numeric} values corresponding to 
##' the new extent to crop all the maps
##' 
##' 
##' @examples 
##'
##' ## Load example data
##' 
##' 
##' @importFrom raster raster projectRaster writeRaster res projection extent 
##' crop nlayers
##'
## END OF HEADER ###############################################################

NULL

##' @export

.upscaleMaps = function(name.simulation
                        , resolution
                        
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notNum(resolution) ||
      sum(resolution <= 0) > 0){
    .stopMessage_beInteger("resolution")
  }
  
  all.files = list.files(path = paste0(name.simulation, "/DATA")
                         , pattern = ".tif$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    all.files = list.files(path = paste0(name.simulation, "/DATA")
                           , pattern = ".img$"
                           , full.names = TRUE
                           , recursive = TRUE
                           , include.dirs = FALSE)
    if (length(all.files) == 0){
      all.files = list.files(path = paste0(name.simulation, "/DATA")
                             , pattern = ".asc$"
                             , full.names = TRUE
                             , recursive = TRUE
                             , include.dirs = FALSE)
      if (length(all.files) == 0){
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/DATA does not contain adequate files (.tif, .img or .asc)"))
      }
    }
  }
  
  for (fi in all.files)
  {
    ras = raster(fi)
    old.res = unique(res(ras))
    old.proj = projection(ras)
    if (old.res <= resolution)
    {
      if (!is.na(old.proj))
      {
        proj.method = "bilinear"
        if (sum(unique(ras[]) %in% c(0,1)) == 2 || length(unique(ras[])) < 10){
          proj.method = "ngb"
        }
        ras.new = projectRaster(from = ras
                                , res = resolution
                                , crs = old.proj
                                , method = proj.method
                                , filename = fi
                                , overwrite = TRUE)
        message(paste0("\n The raster file ", fi, " has been successfully upscaled !"))
      } else
      {
        warning(paste0("\n The raster file ", fi
                       , " does not contain projection information. Please check."))
      }
    } else
    {
      warning(paste0("\n The raster file ", fi, " has a coarser resolution (", old.res
                     , ") than the one resquested (", resolution, "). Please check."))
    }
  }
}


##' @export

.cropMaps = function(name.simulation
                    , extent
                    
){
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  if (.testParam_notNum(extent) ||
      length(extent) != 4){
    stop(paste0("Wrong type of data!\n `extent` must contain 4 numeric values"))
  }
  
  all.files = list.files(path = paste0(name.simulation, "/DATA")
                         , pattern = ".tif$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    all.files = list.files(path = paste0(name.simulation, "/DATA")
                           , pattern = ".img$"
                           , full.names = TRUE
                           , recursive = TRUE
                           , include.dirs = FALSE)
    if (length(all.files) == 0){
      all.files = list.files(path = paste0(name.simulation, "/DATA")
                             , pattern = ".asc$"
                             , full.names = TRUE
                             , recursive = TRUE
                             , include.dirs = FALSE)
      if (length(all.files) == 0){
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/DATA does not contain adequate files (.tif, .img or .asc)"))
      }
    }
  }
  
  for (fi in all.files)
  {
    ras = raster(fi)
    old.proj = projection(ras)
    if (!is.na(old.proj))
    {
      ras.new = crop(x = ras, y = extent, filename = fi, overwrite = TRUE)
      message(paste0("\n The raster file ", fi
                     , " has been successfully cropped !"))
    } else
    {
      warning(paste0("\n The raster file ", fi
                     , " does not contain projection information. Please check."))
    }
  }
}

