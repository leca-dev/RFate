### HEADER #####################################################################
##' @title Create the skeleton folder for a \code{FATE} simulation
##' 
##' @name PRE_FATE.skeletonDirectory
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a user-friendly directory 
##' tree to run a \code{FATE} simulation.
##'              
##' @param name.simulation a \code{string} that will be used as the main 
##' directory and simulation name
##' 
##' @details 
##' 
##' \code{FATE} requires only one input parameter (see 
##' \code{\link{PRE_FATE.params_simulParameters}}), 
##' 
##' \itemize{
##'   \item which is a file containing the names of parameter files,
##'   \item which may themselves contain 
##'   \itemize{
##'     \item parameters (e.g. succession, dispersal files...)
##'     \item or other file names (e.g. disturbance or environmental change 
##'     masks).
##'   }
##' }
##' 
##' The user could give names of files stored everywhere on a machine, and does 
##' not have to put them all in one same place. \cr
##' But as this is more practical, this function proposes a way to 
##' \strong{organize all those files or parameter files} that will or could be 
##' used by a \code{FATE} simulation. \cr \cr
##' 
##' The tree structure is detailed below :
##' 
##' \describe{
##'   \item{\code{DATA}}{this folder will contain all the data or parameters 
##'   that are needed by the model
##'   \describe{
##'     \item{\code{GLOBAL_PARAMETERS}}{files containing global parameters for 
##'     the simulation \cr (see \code{\link{PRE_FATE.params_globalParameters}})}
##'     \item{\code{MASK}}{raster maps used in the model}
##'     \item{\code{SCENARIO}}{files containing information about changes in
##'     raster maps input \cr (see \code{\link{PRE_FATE.params_changingYears}})}
##'     \item{\code{SAVE}}{files containing information about simulation times 
##'     to save outputs \cr (see \code{\link{PRE_FATE.params_savingYears}})}
##'     \item{\code{PFGS}}{ \cr
##'     \describe{
##'       \item{\code{SUCC}}{life history parameter files \cr 
##'       (see \code{\link{PRE_FATE.params_PFGsuccession}})}
##'       \item{\code{LIGHT}}{response to light competition parameter files \cr 
##'       (see \code{\link{PRE_FATE.params_PFGlight}})}
##'       \item{\code{SOIL}}{response to soil competition parameter files \cr 
##'       (see \code{\link{PRE_FATE.params_PFGsoil}})}
##'       \item{\code{DISP}}{dispersal parameter files \cr 
##'       (see \code{\link{PRE_FATE.params_PFGdispersal}})}
##'       \item{\code{HABSUIT}}{habitat suitability maps}
##'       \item{\code{DIST}}{response to disturbances parameter files \cr 
##'       (see \code{\link{PRE_FATE.params_PFGdisturbance}})}
##'       \item{\code{DROUGHT}}{response to drought disturbance parameter files 
##'       \cr (see \code{\link{PRE_FATE.params_PFGdrought}})}
##'       \item{\code{ALIENS}}{aliens introduction maps}
##'     }
##'     }
##'   }
##'   }
##'   \item{\code{PARAM_SIMUL}}{this folder will contain simulation files that can be
##'   given as input to the software \cr (see 
##'   \code{\link{PRE_FATE.params_simulParameters}})}
##'   \item{\code{RESULTS}}{this folder will collect all the results produced by the
##'   software with a folder for each simulation}
##' }
##' 
##' \strong{NB :} \cr
##' All the functions of the \code{RFate} package are based on this folder 
##' structure.
##' 
##' 
##' @return A directory tree with folders to contain the parameter files, the
##' simulation files and the results.
##' 
##' @keywords folder structure
##' 
##' @seealso \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGsoil}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGdrought}},
##' \code{\link{PRE_FATE.params_changingYears}},
##' \code{\link{PRE_FATE.params_savingYears}},
##' \code{\link{PRE_FATE.params_simulParameters}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a skeleton folder with a specific name
##' PRE_FATE.skeletonDirectory(name.simulation = "FATE_AlpineForest")
##' 
##' @export
##'
## END OF HEADER ###############################################################


PRE_FATE.skeletonDirectory = function(name.simulation = "FATE_simulation")
{
  .testParam_notChar.m("name.simulation", name.simulation)
  
  if (file.exists(name.simulation)) {
    ## do nothing if directory already exists
    warning("Directory already exists! (`", name.simulation, "`)")
  } else {
    ## the main simulation dir
    dir.create(name.simulation, showWarnings = FALSE)
    ## the DATA dir
    dir.create(file.path(name.simulation, "DATA"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "GLOBAL_PARAMETERS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "MASK"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "SCENARIO"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "SAVE"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "SUCC"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "LIGHT"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "SOIL"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "DISP"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "HABSUIT"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "DIST"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "DROUGHT"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "ALIENS"), showWarnings = FALSE)
    dir.create(file.path(name.simulation, "DATA", "PFGS", "FIRE"), showWarnings = FALSE)
    ## the simulation parameters dir
    dir.create(file.path(name.simulation, "PARAM_SIMUL"), showWarnings = FALSE)
    ## the RESULTS dir
    dir.create(file.path(name.simulation, "RESULTS"), showWarnings = FALSE)
    
    message("\n Your directory tree for your FATE simulation ("
            , name.simulation, ") is ready!\n")
  }
}


