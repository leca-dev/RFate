### HEADER #####################################################################
##' @title Replace a pattern with a new within all parameter files of a 
##' \code{FATE} simulation folder
##' 
##' @name .setPattern
##'
##' @author Maya Guéguen
##' 
##' @description This function scans all the files within a \code{FATE} 
##' simulation folder to find a specific pattern and replace it with a new one
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param opt.name.file (\emph{optional}) \cr a \code{string} corresponding  
##' to the complete or partial name of the file in which to search and change 
##' the pattern
##' @param pattern.tofind a \code{string} corresponding to the pattern to find
##' @param pattern.toreplace a \code{string} corresponding to the pattern to 
##' replace
##' 
##' 
##' @examples 
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create a Global_parameters file
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
##'                                  , required.no_PFG = 6
##'                                  , required.no_strata = 5
##'                                  , required.simul_duration = 100
##'                                  , required.seeding_duration = c(10,50)
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.max_abund_low = 30000
##'                                  , required.max_abund_medium = 50000
##'                                  , required.max_abund_high = 90000)
##'                                  
##'                                  
##' ## Change number of PFG
##' readLines("FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt")
##' 
##' .setPattern(name.simul = "FATE_simulation"
##'             , opt.name.file = "Global_parameters_V1.txt"
##'             , pattern.tofind = "NO_PFG 6"
##'             , pattern.toreplace = "NO_PFG 14")
##'           
##' readLines("FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt")
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'                                 
##' ## Load example data
##' 
##' 
##' @export
##'
## END OF HEADER ###############################################################


.setPattern = function(name.simulation
                       , opt.name.file = NULL
                       , pattern.tofind
                       , pattern.toreplace
                       
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "")
  name.simulation = sub("/$", "", name.simulation)
  
  .testParam_notChar.m("pattern.tofind", pattern.tofind)
  .testParam_notChar.m("pattern.toreplace", pattern.toreplace)
  
  #############################################################################
  
  all.files = list.files(path = name.simulation
                         , pattern = ".txt$"
                         , full.names = TRUE
                         , recursive = TRUE
                         , include.dirs = FALSE)
  if (length(all.files) == 0){
    stop(paste0("Missing data!\n The folder ", name.simulation
                , " does not contain adequate files (.txt)"))
  }
  
  if (is.null(opt.name.file) ||
      (!is.null(opt.name.file) && !is.character(opt.name.file)) ||
      (!is.null(opt.name.file) && nchar(opt.name.file) == 0)){
    warning("As `opt.name.file` does not contain character value, it will be ignored")
  } else {
    all.files = all.files[grep(opt.name.file, all.files)]
    if (length(all.files) == 0){
      stop(paste0("Missing data!\n The folder ", name.simulation
                  , " does not contain adequate files (", opt.name.file, ")"))
    }
  }
  
  for (fi in all.files)
  {
    params.lines = readLines(con = fi, warn = FALSE)
    if (length(grep(pattern.tofind, params.lines)) > 0){
      params.lines = sub(pattern.tofind, pattern.toreplace, params.lines)
      cat(params.lines, sep = "\n", file = fi, append = FALSE)
      message(paste0("\n The parameter file ", fi
                     , " has been successfully corrected !\n"))
    }
  }
}

