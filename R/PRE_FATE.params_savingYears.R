### HEADER #####################################################################
##' @title Create \emph{SAVE} parameter files for a \code{FATE} simulation
##' 
##' @name PRE_FATE.params_savingYears
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a parameter file containing 
##' simulation years at which the \code{FATE} software must save rasters of 
##' PFG abundances (as well as light and soil resources if these modules are 
##' activated) and/or simulation objects.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory  
##' or simulation name of the \code{FATE} simulation
##' @param years.maps (\emph{optional}) \cr a \code{vector} of simulation years 
##' at which PFG abundance maps will be saved \cr (\emph{as well as maps of 
##' light and soil resources if these modules are activated})
##' @param years.objects (\emph{optional}) \cr a \code{vector} of simulation 
##' years at which \code{FATE} simulation state will be saved
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/SAVE/} directory to store the results
##' 
##' 
##' @details 
##' 
##' \code{FATE} software allows the user to save two different types of 
##' outputs :
##' 
##' \describe{
##'   \item{Raster maps}{PFG abundance maps can be saved for all specified 
##'   simulation years. \cr It includes maps per PFG per strata 
##'   (\code{ABUND_perPFG_perStrata} folder) and summary maps 
##'   per PFG for all height strata combined (\code{ABUND_perPFG_allStrata} 
##'   folder). \cr If the light and / or soil modules are activated (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), maps for light and / or 
##'   soil resources are also saved. \cr Raster format used is depending on 
##'   input data format. It can be either \code{.img} or \code{.tif}.}
##'   \item{Model objects}{using \code{BOOST} library and its serialization 
##'   functions, \code{FATE} is able to save a simulation at a specific 
##'   time. This object allows the user to restart a simulation from this 
##'   precise state by specifying its name within the \emph{Simul_parameters} 
##'   file with the \code{SAVED_STATE} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}).}
##' }
##' 
##' 
##' @return Two \code{.txt} files into the \code{name.simulation/DATA/SAVE/}
##' directory :
##' 
##'  \itemize{
##'    \item \file{SAVE_YEARS_maps.txt} : one line for each simulation year for 
##'    which the raster maps are to be saved
##'    \item \file{SAVE_YEARS_objects.txt} : one line for each simulation year 
##'    for which the \code{FATE} objects are to be saved \cr \cr
##'  }
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \code{name.simulation/DATA/SAVE/opt.folder.name/}.
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a SAVE_year_maps or/and SAVE_year_objects parameter file
##' PRE_FATE.params_savingYears(name.simulation = "FATE_simulation"
##'                           , years.maps = c(100, 150, 200)
##'                           , years.objects = 200)
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_savingYears = function(
  name.simulation
  , years.maps = NULL
  , years.objects = NULL
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/SAVE/")
  
  if ( (!is.null(years.maps) && !is.numeric(years.maps)) ||
       (!is.null(years.objects) && !is.numeric(years.objects))){
    stop(paste0("Wrong type of data!\n `years.maps` and/or `years.objects` "
                , "must contain numeric values"))
  }
  if (is.null(years.maps) && is.null(years.objects)){
    warning(paste0("Both `years.maps` and `years.objects` parameters are NULL. "
                   , "No parameter file will be created"))
  } else
  {
    ## CHECK parameter opt.folder.name
    opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                                , paste0(name.simulation, "/DATA/SAVE/"))
    
    #############################################################################
    
    if (!is.null(years.maps))
    {
      params = lapply(years.maps, function(x) x)
      names(params) = rep("", length(params))
      
      file.name = paste0(name.simulation, "/DATA/SAVE/"
                         , opt.folder.name, "SAVE_YEARS_maps.txt")
      .createParams(params.file = file.name
                    , params.list = params)
      file.lines = readLines(file.name)
      file.lines = file.lines[-c(1,2)]
      file.lines = gsub(" ", "", file.lines)
      cat(file.lines, sep = "\n", file = file.name, append = FALSE)
    }
    
    #############################################################################
    
    if (!is.null(years.objects))
    {
      params = lapply(years.objects, function(x) x)
      names(params) = rep("", length(params))
      
      file.name = paste0(name.simulation, "/DATA/SAVE/"
                         , opt.folder.name, "SAVE_YEARS_objects.txt")
      .createParams(params.file = file.name
                    , params.list = params)
      file.lines = readLines(file.name)
      file.lines = file.lines[-c(1,2)]
      file.lines = gsub(" ", "", file.lines)
      cat(file.lines, sep = "\n", file = file.name, append = FALSE)
    }
  }
}

