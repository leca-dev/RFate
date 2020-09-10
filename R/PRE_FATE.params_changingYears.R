### HEADER #####################################################################
##' @title Create \emph{SCENARIO} parameter files for a \code{FATE}
##' simulation
##' 
##' @name PRE_FATE.params_changingYears
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create several parameter files to 
##' manage the update of simulation maps : 1) simulation years at which the 
##' maps should be changed ; 2) filenames corresponding to the new simulation 
##' maps to be used.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param type.changing a \code{string} to choose the concerned module :
##' \itemize{
##'   \item \code{MASK} (succession),
##'   \item \code{HABSUIT} (habitat suitability),
##'   \item \code{DIST} (disturbances),
##'   \item \code{DROUGHT} (drought disturbance),
##'   \item \code{ALIENS} or \code{ALIENS_F} (aliens introduction, masks or 
##'   frequencies)
##'   \item \code{FIRE} or \code{FIRE_F} (fire disturbance, masks or 
##'   frequencies)
##' }
##' @param mat.changing a \code{data.frame} with 3 columns : \cr \code{year}, 
##' \code{order}, \code{new.value}
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/SCENARIO/} directory to store the results
##' 
##' 
##' @details 
##' 
##' Several modules of the \code{FATE} software allow the user to simulate 
##' changes over time :
##' 
##' \describe{
##'   \item{succession}{the \strong{core module} is based on a raster mask given 
##'   within the \emph{Simul_parameters} file with the \code{MASK} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}), \strong{with either 
##'   \code{0} or \code{1}} within each pixel, \code{1} corresponding to the 
##'   cells in which the PFG can try to colonize. The available pixels can 
##'   change through time, to simulate habitat loss (e.g. urbanization) or gain 
##'   (e.g. glacial retreat).}
##'   \item{habitat suitability}{\strong{if this module is activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), PFG colonization depends 
##'   on maps given for each PFG within the \emph{Simul_parameters} file with 
##'   the \code{PFG_HAB_MASK} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}). \cr These maps must 
##'   contain \strong{values between \code{0} and \code{1}} corresponding to the 
##'   probability of presence of the PFG in each pixel. These probabilities 
##'   can change through time, as they often come from Species Distribution 
##'   Models (SDM) that can be based for example on climatic variables (e.g. 
##'   simulating regional warming).}
##'   \item{disturbances}{\strong{if this module is activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), each disturbance relies 
##'   on a raster given within the \emph{Simul_parameters} file with the 
##'   \code{DIST_MASK} flag (see \code{\link{PRE_FATE.params_simulParameters}}).
##'   \cr As for succession, this mask is filled \strong{with either \code{0} 
##'   or \code{1}} to define where the perturbation occurs. The impacted pixels 
##'   can also change through time (e.g. change in forestry practices, expansion 
##'   of grazing areas, etc).}
##'   \item{drought disturbance}{\strong{if this module is activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), drought disturbance 
##'   relies on a raster given within the \emph{Simul_parameters} file with the 
##'   \code{DROUGHT_MASK} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}).
##'   \cr This map contains values defining the drought intensity experienced by 
##'   the area. This intensity can change through time and space (e.g. regional 
##'   warming, extreme years, change in agriculture practices that can leave a 
##'   place more exposed, etc).}
##'   \item{aliens introduction}{\strong{if this module is activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), aliens introduction 
##'   depends on maps given for each alien within the \emph{Simul_parameters} 
##'   file with the \code{PFG_ALIENS_MASK} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}).
##'   \cr As for succession, these masks are filled \strong{with either \code{0} 
##'   or \code{1}} to define where the introductions occur. The impacted pixels 
##'   can also change through time (e.g. colonization, eradication campaign, 
##'   etc), as well as the frequencies of introduction (see \code{ALIENS_FREQ} 
##'   flag in \code{\link{PRE_FATE.params_globalParameters}}).}
##'   
##'   \item{fire disturbance}{\strong{if this module is activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), fire disturbance 
##'   can rely on a raster given within the \emph{Simul_parameters} file with 
##'   the \code{FIRE_MASK} flag (see 
##'   \code{\link{PRE_FATE.params_simulParameters}}). \cr
##'   As for succession, this mask is filled \strong{with either \code{0} 
##'   or \code{1}} to define where the perturbation occurs. The impacted pixels 
##'   can also change through time (e.g. change in forestry practices, expansion 
##'   of drought events, etc), as well as the frequencies of perturbations (see 
##'   \code{FIRE_FREQ} flag in \code{\link{PRE_FATE.params_globalParameters}}). 
##'   \cr \cr}
##' }
##' 
##' Several parameters, given within \code{mat.changing}, are required to set up 
##' these temporal changes :
##' 
##' \describe{
##'   \item{year}{all simulation years at which the raster files of a specific 
##'   module \emph{(succession \code{MASK}, habitat suitability \code{HABSUIT}, 
##'   disturbance \code{DIST}, drought \code{DROUGHT}, aliens introduction 
##'   \code{ALIENS} or \code{ALIENS_F}, fire \code{FIRE} or \code{FIRE_F})} will 
##'   be changed}
##'   \item{new.value}{the names of the new raster files for each year of 
##'   change. It can be either \code{.img} or \code{.tif}.\cr
##'   There is an exception if \code{ALIENS_F} or \code{FIRE_F} is selected : 
##'   the values should be \code{integer} representing the frequencies of aliens 
##'   introduction or fire perturbations.}
##'   \item{order}{an \code{integer} associated to each new map in order to 
##'   always give the raster maps in the same order throughout the years \cr \cr}
##' }
##' 
##' 
##' 
##' @return Several \file{.txt} files into the 
##' \code{name.simulation/DATA/SCENARIO/} :
##' \itemize{
##'   \item ..\emph{type.changing}..\file{.changingmask_years.txt} : one line for each 
##'   simulation year 
##'   \item ..\emph{type.changing}..\file{.changingmask_files_t}..\emph{year}..\file{.txt} : 
##'   one line for each new raster file \cr \cr
##' }
##' 
##' OR
##' \itemize{
##'   \item ..\emph{type.changing}..\file{.changingfreq_years.txt} : one line for each 
##'   simulation year 
##'   \item ..\emph{type.changing}..\file{.changingfreq_files_t}..\emph{year}..\file{.txt} : 
##'   one line for each new frequency \cr \cr
##' }
##' 
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the folder
##' \code{name.simulation/DATA/SCENARIO/opt.folder.name/}.
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Changing_times parameter file
##' PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
##'                               , type.changing = "DIST"
##'                               , mat.changing = data.frame(year = c(50,50,80,80)
##'                                                           , order = c(1,2,1,2)
##'                                                           , new.value = c("MASK_DIST1_50.tif"
##'                                                                           , "MASK_DIST2_50.tif"
##'                                                                           , "MASK_DIST1_80.tif"
##'                                                                           , "MASK_DIST2_80.tif")))
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.params_changingYears = function(
  name.simulation
  , type.changing
  , mat.changing
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/SCENARIO/")
  
  ## CHECK parameter type.changing
  .testParam_notInValues.m("type.changing", type.changing
                           , c("MASK", "HABSUIT", "DIST", "DROUGHT"
                               , "ALIENS", "ALIENS_F"
                               , "FIRE", "FIRE_F"))
  ## CHECK parameter mat.changing
  if (.testParam_notDf(mat.changing))
  {
    .stopMessage_beDataframe("mat.changing")
  } else
  {
    if (nrow(mat.changing) == 0 || ncol(mat.changing) != 3)
    {
      .stopMessage_numRowCol("mat.changing", c("year", "order", "new.value"))
    } else if (.testParam_notColnames(mat.changing, c("year", "order", "new.value")))
    {
      .stopMessage_columnNames("mat.changing", c("year", "order", "new.value"))
    }
    .testParam_notNum.m("mat.changing$year", mat.changing$year)
    .testParam_NAvalues.m("mat.changing$year", mat.changing$year)
    mat.changing$new.value = as.character(mat.changing$new.value)
    .testParam_notChar.m("mat.changing$new.value", mat.changing$new.value)
    .testParam_NAvalues.m("mat.changing$new.value", mat.changing$new.value)
    .testParam_notNum.m("mat.changing$order", mat.changing$order)
    .testParam_NAvalues.m("mat.changing$order", mat.changing$order)
    if (length(unique(table(mat.changing$year, mat.changing$order))) > 1) {
      stop(paste0("Wrong type of data!\n Columns `year` and `order` are not balanced\n"
                  , " All combinations must be represented"))
    }
  }
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/SCENARIO/"))
  
  #############################################################################
  
  TYPE.CHANGING = ifelse(type.changing %in% c("ALIENS_F", "FIRE_F")
                         , "changingfreq", "changingmask")
  type.changing = sub("_F$", "", type.changing)
  
  #############################################################################
  
  ### CREATE changing_times.txt file
  params = lapply(sort(unique(mat.changing$year)), function(x) x)
  names(params) = rep("", length(params))
  
  new.value = paste0(name.simulation, "/DATA/SCENARIO/"
                     , opt.folder.name, type.changing
                     , "_", TYPE.CHANGING, "_years.txt")
  .createParams(params.file = new.value
                , params.list = params)
  file.lines = readLines(new.value)
  file.lines = file.lines[-c(1,2)]
  file.lines = gsub(" ", "", file.lines)
  cat(file.lines, sep = "\n", file = new.value, append = FALSE)
  
  #############################################################################
  
  ### CREATE changing_masks.txt files
  for(y in sort(unique(mat.changing$year)))
  {
    changing.names = mat.changing[which(mat.changing$year == y),]
    changing.names = changing.names$new.value[order(changing.names$order)]
    
    params = lapply(changing.names, function(x) x)
    names(params) = rep("", length(params))
    
    new.value = paste0(name.simulation, "/DATA/SCENARIO/"
                       , opt.folder.name, type.changing
                       , "_", TYPE.CHANGING, "_files_t", y, ".txt")
    .createParams(params.file = new.value
                  , params.list = params)
    file.lines = readLines(new.value)
    file.lines = file.lines[-c(1,2)]
    file.lines = gsub(" ", "", file.lines)
    cat(file.lines, sep = "\n", file = new.value, append = FALSE)
  }
}
