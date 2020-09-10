### HEADER #####################################################################
##' @title Create \emph{Simul_parameters} parameter file for a \code{FATE} 
##' simulation
##' 
##' @name PRE_FATE.params_simulParameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create one (or several) parameter 
##' file containing \code{PARAMETER FILENAMES} used in \code{FATE} model.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param name.MASK a \code{string} corresponding to the file name of a raster 
##' mask, with either \code{0} or \code{1} within each pixel, \code{1} 
##' corresponding to the cells of the studied area in which the succession 
##' (core) module of the \code{FATE} simulation will take place (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param name.SAVED_STATE (\emph{optional}) \cr a \code{string} corresponding 
##' to the file name of a \code{FATE} object, obtained from a previous 
##' simulation and from which to restart this new simulation
##' @param name.DIST (\emph{optional}) \cr a \code{string} corresponding to the 
##' file name of a raster mask, with either \code{0} or \code{1} within each 
##' pixel, \code{1} corresponding to the cells of the studied area in which the 
##' disturbance module of the \code{FATE} simulation will take place (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param name.DROUGHT (\emph{optional}) \cr a \code{string} corresponding to 
##' the name of a raster file, with a \code{numeric} value within each pixel 
##' corresponding to the drought intensity experienced by this pixel through 
##' the drought (or fire) disturbance module of the \code{FATE} simulation (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param name.FIRE (\emph{optional}) \cr a \code{string} corresponding to the 
##' file name of a raster mask, with either \code{0} or \code{1} within each 
##' pixel, \code{1} corresponding to the cells of the studied area in which the 
##' fire disturbance module of the \code{FATE} simulation will take place (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param name.ELEVATION (\emph{optional}) \cr a \code{string} corresponding to 
##' the name of a raster file, with a \code{numeric} value within each pixel 
##' corresponding to the elevation of this pixel and used by the fire 
##' disturbance module of the \code{FATE} simulation (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param name.SLOPE (\emph{optional}) \cr a \code{string} corresponding to 
##' the name of a raster file, with a \code{numeric} value within each pixel 
##' corresponding to the slope of this pixel and used by the fire 
##' disturbance module of the \code{FATE} simulation (see 
##' \href{PRE_FATE.params_globalParameters.html#details}{\code{PRE_FATE.params_globalParameters}})
##' @param opt.global.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the global parameter file in the folder 
##' \code{name.simulation/DATA/GLOBAL_PARAMETERS/} that will be used to build 
##' the simulation parameter file
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder in each \code{name.simulation/DATA/PFGS/module/} 
##' from which PFG file names will be extracted to build the simulation 
##' parameter file
##' 
##' 
##' 
##' @details 
##' 
##' The \code{FATE} software takes only one input parameter : a file 
##' containing links to other files containing all the parameters and data 
##' needed by the program to run.
##' 
##' 
##' \describe{
##'   \item{GLOBAL_PARAMS}{file where parameters related to the simulation 
##'   definition are referred (e.g. number of PFG involved, number of height 
##'   strata, simulation duration, computer resources, modules loaded, etc) \cr
##'   (see \code{\link{PRE_FATE.params_globalParameters}}) \cr \cr
##'   }
##'   \item{SAVING_DIR}{directory where simulation outputs will be stored}
##'   \item{SAVED_STATE (\emph{optional})}{file containing the results of a 
##'   previous \code{FATE} simulation from which to restart this new simulation 
##'   \cr }
##'   \item{SAVING_YEARS_ \cr ARRAYS (\emph{optional})}{file containing the 
##'   years for which simulation maps will be saved \cr
##'   (see \code{\link{PRE_FATE.params_savingYears}})
##'   }
##'   \item{SAVING_YEARS_ \cr OBJECTS (\emph{optional})}{file containing the 
##'   years for which simulation outputs will be saved \cr
##'   (see \code{\link{PRE_FATE.params_savingYears}}) \cr \cr
##'   }
##'   \item{MASK}{raster mask that will define the study area}
##'   \item{MASK_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{MASK_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the succession module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr LIFE_HISTORY}{PFG life history related parameters 
##'   (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsuccession}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr LIGHT (\emph{optional})}{PFG light preferences and 
##'   tolerance related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGlight}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr SOIL (\emph{optional})}{PFG soil contribution and 
##'   tolerance related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGsoil}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr DISPERSAL (\emph{optional})}{PFG dispersal 
##'   capacity related parameters (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdispersal}}) \cr \cr
##'   }
##'   \item{PFG_MASK_HABSUIT \cr (\emph{optional})}{raster masks (one by PFG) 
##'   containing PFG habitat suitability for the study area}
##'   \item{HABSUIT_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{HABSUIT_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the habitat suitability module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_ \cr DISTURBANCES (\emph{optional})}{PFG disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdisturbance}})
##'   }
##'   \item{DIST_MASK \cr (\emph{optional})}{raster masks that will define the 
##'   disturbance areas}
##'   \item{DIST_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the years 
##'   to change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})
##'   }
##'   \item{DIST_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the files 
##'   to change rasters for the disturbance module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr
##'   }
##'   \item{PFG_PARAMS_DROUGHT \cr (\emph{optional})}{PFG drought disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdrought}})}
##'   \item{DROUGHT_MASK \cr (\emph{optional})}{raster mask that will define the 
##'   drought intensity of the area}
##'   \item{DROUGHT_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change rasters for the drought disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{DROUGHT_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the 
##'   files to change rasters for the drought disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr}
##'   \item{PFG_MASK_ALIENS \cr (\emph{optional})}{raster masks (one by alien) 
##'   containing alien introduction zones for the study area}
##'   \item{ALIENS_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change rasters for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the 
##'   files to change rasters for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEFREQ_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change frequencies for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ALIENS_CHANGEFREQ_FILES \cr (\emph{optional})}{file containing the 
##'   files to change frequencies for the aliens introduction module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}}) \cr \cr}
##'   \item{PFG_PARAMS_FIRE \cr (\emph{optional})}{PFG fire disturbance 
##'   related parameters in terms of resprouting and mortality (one by PFG) \cr
##'   (see \code{\link{PRE_FATE.params_PFGdisturbance}})}
##'   \item{FIRE_MASK \cr (\emph{optional})}{raster mask that will define the 
##'   fire disturbance areas}
##'   \item{FIRE_CHANGEMASK_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change rasters for the fire disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{FIRE_CHANGEMASK_FILES \cr (\emph{optional})}{file containing the 
##'   files to change rasters for the fire disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{FIRE_CHANGEFREQ_YEARS \cr (\emph{optional})}{file containing the 
##'   years to change frequencies for the fire disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{FIRE_CHANGEFREQ_FILES \cr (\emph{optional})}{file containing the 
##'   files to change frequencies for the fire disturbances module \cr
##'   (see \code{\link{PRE_FATE.params_changingYears}})}
##'   \item{ELEVATION_MASK \cr (\emph{optional})}{raster mask that will define 
##'   the elevation of the area}
##'   \item{SLOPE_MASK \cr (\emph{optional})}{raster mask that will define the 
##'   slope of the area}
##' }
##' 
##' 
##' 
##' @note 
##' 
##' \itemize{
##'   \item The function produces links to files that are \strong{NOT absolute 
##'   paths BUT relative ones}. \cr
##'   \emph{When relative paths are used}, the user should be careful of the 
##'   folder from which the simulation is launched. \cr
##'   \emph{A function allows to transform these relative paths into absolute 
##'   ones. (see examples of \code{\link{.setPattern}})}.
##'   \item \strong{The order of files matters!} \cr
##'   For instance the first link below \code{--PFG_PARAMS_LIFE_HISTORY--} flag 
##'   (e.g. \emph{PFG Albert}) has to match with the first item below the 
##'   \code{--PFG_PARAMS_DISPERSAL--} flag (must be \emph{PFG Albert} too).
##' }
##' 
##' 
##' 
##' 
##' @return A \code{.txt} file into the \code{name.simulation/PARAM_SIMUL/}
##' directory with the following parameters :
##' 
##' \itemize{
##'   \item \strong{--GLOBAL_PARAMS--}
##'   \item \strong{--SAVING_DIR--}
##'   \itemize{
##'   \item --SAVED_STATE-- (\emph{optional})
##'   \item --SAVING_YEARS_MAPS-- (\emph{optional})
##'   \item --SAVING_YEARS_OBJECTS-- (\emph{optional})
##'   }
##'   \item \strong{--MASK--}
##'   \itemize{
##'   \item --MASK_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --MASK_CHANGEMASK_FILES-- (\emph{optional})
##'   }
##'   \item \strong{--PFG_PARAMS_LIFE_HISTORY--}
##'   \itemize{
##'   \item --PFG_PARAMS_LIGHT-- (\emph{optional})
##'   \item --PFG_PARAMS_SOIL-- (\emph{optional})
##'   \item --PFG_PARAMS_DISPERSAL-- (\emph{optional})
##'   \item --PFG_MASK_HABSUIT-- (\emph{optional})
##'   \item --HABSUIT_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --HABSUIT_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_PARAMS_DISTURBANCES-- (\emph{optional})
##'   \item --DIST_MASK-- (\emph{optional})
##'   \item --DIST_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --DIST_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_PARAMS_DROUGHT-- (\emph{optional})
##'   \item --DROUGHT_MASK-- (\emph{optional})
##'   \item --DROUGHT_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --DROUGHT_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --PFG_MASK_ALIENS-- (\emph{optional})
##'   \item --ALIENS_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --ALIENS_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --ALIENS_CHANGEFREQ_YEARS-- (\emph{optional})
##'   \item --ALIENS_CHANGEFREQ_FILES-- (\emph{optional})
##'   \item --PFG_PARAMS_FIRE-- (\emph{optional})
##'   \item --FIRE_MASK-- (\emph{optional})
##'   \item --FIRE_CHANGEMASK_YEARS-- (\emph{optional})
##'   \item --FIRE_CHANGEMASK_FILES-- (\emph{optional})
##'   \item --FIRE_CHANGEFREQ_YEARS-- (\emph{optional})
##'   \item --FIRE_CHANGEFREQ_FILES-- (\emph{optional})
##'   \item --ELEVATION_MASK-- (\emph{optional})
##'   \item --SLOPE_MASK-- (\emph{optional})
##' }
##'   \item \strong{--END_OF_FILE--}
##' }
##' 
##' 
##' @keywords FATE, simulation
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' \code{\link{PRE_FATE.params_PFGsoil}},
##' \code{\link{PRE_FATE.params_PFGdispersal}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}},
##' \code{\link{PRE_FATE.params_PFGdrought}},
##' \code{\link{PRE_FATE.params_savingYears}},
##' \code{\link{PRE_FATE.params_changingYears}},
##' \code{\link{.setPattern}}
##' 
##' @examples
##' 
##'            
##' ## ----------------------------------------------------------------------------------------- ##
##'                                 
##' ## Load example data
##' 
##' @export
##' 
##' @importFrom foreach foreach %do%
##'
## END OF HEADER ###############################################################


PRE_FATE.params_simulParameters = function(
  name.simulation
  , name.MASK
  , name.SAVED_STATE = NULL
  , name.DIST = NULL
  , name.DROUGHT = NULL
  , name.FIRE = NULL
  , name.ELEVATION = NULL
  , name.SLOPE = NULL
  , opt.global.name = NULL
  , opt.folder.name = NULL
){
  
  ############################################################################# 
  
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "DATA/GLOBAL_PARAMETERS/")
  .testParam_existFolder(name.simulation, "DATA/SAVE/")
  .testParam_existFolder(name.simulation, "DATA/SCENARIO/")
  .testParam_existFolder(name.simulation, "DATA/MASK/")
  .testParam_existFolder(name.simulation, "DATA/PFGS/SUCC/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_notChar.m("name.MASK", name.MASK)
  .testParam_existFile(paste0(name.simulation, "/DATA/MASK/", name.MASK))
  ## CHECK parameter opt.global.name
  opt.global.name = .getParam_opt.folder.name(opt.global.name
                                              , basename(opt.global.name)
                                              , create.dir = FALSE)
  opt.global.name = sub("/$", "", opt.global.name)
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(opt.folder.name, "/")
                                              , create.dir = FALSE)
  
  type.changing1 = c("MASK", "HABSUIT", "DIST", "DROUGHT", "ALIENS", "FIRE") #, "ALIENS_F")
  type.changing2 = c("HABSUIT", "ALIENS")
  
  
  #############################################################################
  ## Get the name(s) of global parameter file(s) that will
  ## be used to create the simulation parameter file(s)
  
  if (opt.global.name == "")
  {
    files.GLOBAL = list.files(path = paste0(name.simulation
                                            , "/DATA/GLOBAL_PARAMETERS")
                              , pattern = "^Global_parameters.*.txt"
                              , full.names = TRUE)
  } else
  {
    files.GLOBAL = paste0(name.simulation
                          , "/DATA/GLOBAL_PARAMETERS/"
                          , opt.global.name)
  }
  if (length(files.GLOBAL) == 0)
  {
    stop(paste0("Wrong number of files!\n There is no adequate file "
                , "(`.txt` file starting with `Global_parameters`) "
                , "into the DATA/GLOBAL_PARAMETERS/ folder"))
  }
  
  #############################################################################
  ## Get the name(s) of SAVE directory
  
  dirs.SAVE = list.dirs(path = paste0(name.simulation, "/DATA/SAVE")
                        , full.names = FALSE
                        , recursive = FALSE)
  if (length(dirs.SAVE) > 0)
  {
    dirs.SAVE = paste0(name.simulation, "/DATA/SAVE/", dirs.SAVE)
  } else
  {
    dirs.SAVE = paste0(name.simulation, "/DATA/SAVE")
  }
  
  #############################################################################
  ## Get the name(s) of SCENARIO directory
  
  dirs.SCENARIO = list.dirs(path = paste0(name.simulation, "/DATA/SCENARIO")
                            , full.names = FALSE
                            , recursive = FALSE)
  
  if (length(dirs.SCENARIO) > 0)
  {
    dirs.SCENARIO = paste0(name.simulation, "/DATA/SCENARIO/", dirs.SCENARIO)
  } else
  {
    dirs.SCENARIO = paste0(name.simulation, "/DATA/SCENARIO")
  }
  
  warn.messages = list()
  for (ty in type.changing1)
  {
    assign(x = paste0("dirs.SCENARIO.", ty), value = vector())
    for (di in dirs.SCENARIO)
    {
      files.ty = list.files(path = di
                            , pattern = paste0("^", ty, "_changingmask_years")
                            , full.names = TRUE)
      if (length(files.ty) > 0)
      {
        eval(parse(text = paste0("dirs.SCENARIO.", ty
                                 , " = c(dirs.SCENARIO.", ty
                                 , ", di)")))
      } else
      {
        warn.messages[[length(warn.messages) +1]] = c(ty, di)
        # warning(paste0("There is no adequate file (`.txt` file starting with `"
        #                , ty, "_changingmask_years`) "
        #                , "into the folder ", di))
      }
    }
  }
  if (length(warn.messages) > 0)
  {
    warning(paste0("There is no adequate file(s) into some folder(s) : \n"
                   , paste0(" > ", sapply(warn.messages, function(x) x[1])
                            , "_changingmask_years[...].txt (folder "
                            , sapply(warn.messages, function(x) x[2]), ") \n"
                            , collapse = "")))
  }

  
  #############################################################################
  ## Get the name(s) of HABSUIT / ALIENS directory
  
  for (ty in type.changing2)
  {
    name.dir = paste0(name.simulation, "/DATA/PFGS/", ty)
    dirs.SCE = list.dirs(path = name.dir
                         , full.names = FALSE
                         , recursive = FALSE)
    if (length(dirs.SCE) > 0)
    {
      dirs.SCE = paste0(name.dir, "/", dirs.SCE)
    } else
    {
      dirs.SCE = name.dir
    }
    
    assign(x = paste0("dirs.", ty), value = vector())
    for (di in dirs.SCE)
    {
      files.di = list.files(path = di, full.names = TRUE)
      if (length(files.di) > 0)
      {
        eval(parse(text = paste0("dirs.", ty, " = c(dirs.", ty, ", di)")))
      }
    }
  }
  
  #############################################################################
  #############################################################################
  ## Combine the names found (global parameters, SAVE directory, 
  ## SCENARIO directory, HABSUIT directory)
  
  sce.mask = sce.habsuit = sce.dist = sce.drought = sce.aliens = sce.fire = 0
  ras.habsuit = ras.aliens = 0
  if (length(dirs.SCENARIO.MASK) > 0) sce.mask = 1:length(dirs.SCENARIO.MASK)
  if (length(dirs.SCENARIO.HABSUIT) > 0) sce.habsuit = 1:length(dirs.SCENARIO.HABSUIT)
  if (length(dirs.SCENARIO.DIST) > 0) sce.dist = 1:length(dirs.SCENARIO.DIST)
  if (length(dirs.SCENARIO.DROUGHT) > 0) sce.drought = 1:length(dirs.SCENARIO.DROUGHT)
  if (length(dirs.SCENARIO.ALIENS) > 0) sce.aliens = 1:length(dirs.SCENARIO.ALIENS)
  if (length(dirs.SCENARIO.FIRE) > 0) sce.fire = 1:length(dirs.SCENARIO.FIRE)
  if (length(dirs.HABSUIT) > 0) ras.habsuit = 1:length(dirs.HABSUIT)
  if (length(dirs.ALIENS) > 0) ras.aliens = 1:length(dirs.ALIENS)
  
  PARAMS.combi = expand.grid(GLOBAL = 1:length(files.GLOBAL)
                             , SAVE = 1:length(dirs.SAVE)
                             , SCENARIO.MASK = sce.mask
                             , SCENARIO.HABSUIT = sce.habsuit
                             , SCENARIO.DIST = sce.dist
                             , SCENARIO.DROUGHT = sce.drought
                             , SCENARIO.ALIENS = sce.aliens
                             , SCENARIO.FIRE = sce.fire
                             , PFG.HABSUIT = ras.habsuit
                             , PFG.ALIENS = ras.aliens)
  
  #############################################################################
  #############################################################################
  ## Build a simulation parameter file for each combination found
  
  for (i in 1:nrow(PARAMS.combi))
  {
    
    params.combi = data.frame(GLOBAL = files.GLOBAL[PARAMS.combi$GLOBAL[i]]
                              , MASK = paste0(name.simulation
                                              , "/DATA/MASK/"
                                              , name.MASK)
                              , stringsAsFactors = FALSE)
    names.params.combi = c("--GLOBAL_PARAMS--", "--MASK--")
    if (!is.null(name.SAVED_STATE) && nchar(name.SAVED_STATE) > 0)
    {
      .testParam_existFile(name.SAVED_STATE)
      params.combi[["SAVED_STATE"]] = name.SAVED_STATE
      names.params.combi = c(names.params.combi, "--SAVED_STATE--")
    }
    
    #############################################################################
    
    di = dirs.SAVE[PARAMS.combi$SAVE[i]]
    di.opt = data.frame(pat = c("SAVE_YEARS_maps", "SAVE_YEARS_objects")
                        , nam = c("SAVE.maps", "SAVE.obj")
                        , param = c("SAVING_YEARS_MAPS", "SAVING_YEARS_OBJECTS")
                        , stringsAsFactors = FALSE)
    
    warn.messages = list()
    for (ii in 1:nrow(di.opt))
    {
      files.found = list.files(path = di
                               , pattern = paste0("^", di.opt$pat[ii])
                               , full.names = TRUE)
      if (length(files.found) == 0)
      {
        warn.messages[[length(warn.messages) +1]] = c(di.opt$pat[ii], di)
        # warning(paste0("There is no adequate file (`.txt` file starting with `"
        #                , di.opt$pat[ii], "`) "
        #                , "into the folder ", di))
      } else if (length(files.found) == 1)
      {
        params.combi[[di.opt$nam[ii]]] = files.found
        names.params.combi = c(names.params.combi
                               , paste0("--", di.opt$param[ii], "--"))
      } else
      {
        stop(paste0("There is too many adequate files (`.txt` file starting with `"
                    , di.opt$pat[ii], "`) "
                    , "into the folder ", di))
      }
    }
    if (length(warn.messages) > 0)
    {
      warning(paste0("There is no adequate file(s) into some folder(s) : \n"
                     , paste0(" > ", sapply(warn.messages, function(x) x[1])
                              , "[...].txt (folder "
                              , sapply(warn.messages, function(x) x[2]), ") \n"
                              , collapse = "")))
    }
    
    #############################################################################
    
    warn.messages = list()
    for (ty in type.changing1)
    {
      if (PARAMS.combi[, paste0("SCENARIO.", ty)][i] > 0)
      {
        eval(parse(text = paste0("di = dirs.SCENARIO.", ty
                                 , "[PARAMS.combi$SCENARIO.", ty, "[i]]")))
        
        ## Changing years
        files.SCE.years = list.files(path = di
                                     , pattern = paste0("^", ty, "_changingmask_years")
                                     , full.names = TRUE)
        if (length(files.SCE.years) == 1)
        {
          params.combi[[paste0("SCENARIO.", ty)]] = files.SCE.years
          names.params.combi = c(names.params.combi
                                 , paste0("--", ty, "_CHANGEMASK_YEARS--"))
        } else
        {
          stop(paste0("There is too many adequate files (`.txt` file starting with `"
                      , ty, "_changingmask_years`) "
                      , "into the folder ", di))
        }
        
        ## Changing files
        files.SCE.masks = list.files(path = di
                                     , pattern = paste0("^", ty, "_changingmask_files")
                                     , full.names = TRUE)
        if (length(files.SCE.masks) == 0)
        {
          warn.messages[[length(warn.messages) +1]] = c(ty, di)
          # warning(paste0("There is no adequate file (`.txt` file starting with `"
          #                , ty, "_changingmask_files`) "
          #                , "into the folder ", di))
        } else if (length(files.SCE.masks) > 0)
        {
          assign(x = paste0("SCENARIO.", ty), value = files.SCE.masks)
        }
      }
    }
    if (length(warn.messages) > 0)
    {
      warning(paste0("There is no adequate file(s) into some folder(s) : \n"
                     , paste0(" > ", sapply(warn.messages, function(x) x[1])
                              , "_changingmask_files[...].txt (folder "
                              , sapply(warn.messages, function(x) x[2]), ") \n"
                              , collapse = "")))
    }
    
    #############################################################################
    
    globi = as.character(files.GLOBAL[PARAMS.combi$GLOBAL[i]])
    
    ### -------------------------------------------------------------------- ###
    
    no_PFG = .getParam(params.lines = globi
                       , flag = "NO_PFG"
                       , flag.split = " "
                       , is.num = TRUE)
    no_PFG = as.numeric(no_PFG)
    
    do.SUCC = 1
    do.LIGHT = .getParam(params.lines = globi
                         , flag = "DO_LIGHT_COMPETITION"
                         , flag.split = " "
                         , is.num = TRUE)
    do.SOIL = .getParam(params.lines = globi
                        , flag = "DO_SOIL_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
    do.DISP = .getParam(params.lines = globi
                        , flag = "DO_DISPERSAL"
                        , flag.split = " "
                        , is.num = TRUE)
    do.HABSUIT = .getParam(params.lines = globi
                           , flag = "DO_HAB_SUITABILITY"
                           , flag.split = " "
                           , is.num = TRUE)
    do.DIST = .getParam(params.lines = globi
                        , flag = "DO_DISTURBANCES"
                        , flag.split = " "
                        , is.num = TRUE)
    do.DROUGHT = .getParam(params.lines = globi
                           , flag = "DO_DROUGHT_DISTURBANCE"
                           , flag.split = " "
                           , is.num = TRUE)
    do.ALIENS = .getParam(params.lines = globi
                          , flag = "DO_ALIENS_INTRODUCTION"
                          , flag.split = " "
                          , is.num = TRUE)
    do.FIRE = .getParam(params.lines = globi
                        , flag = "DO_FIRE_DISTURBANCE"
                        , flag.split = " "
                        , is.num = TRUE)
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("SUCC", "LIGHT", "SOIL", "DISP", "DIST", "DROUGHT", "FIRE")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_existFolder(name.simulation, paste0("DATA/PFGS/", mod, "/"))
        name.dir = paste0(name.simulation, "/DATA/PFGS/", mod)
        
        ## Get folders
        if (opt.folder.name != "")
        {
          dirs.mod = paste0(name.dir, "/", opt.folder.name)
        }
        if (opt.folder.name == "" || !dir.exists(dirs.mod))
        {
          dirs.mod = list.dirs(path = name.dir
                               , full.names = FALSE
                               , recursive = FALSE)
          if (length(dirs.mod) > 0)
          {
            dirs.mod = paste0(name.dir, "/", dirs.mod)
          } else
          {
            dirs.mod = name.dir
          }
        }
        
        ## Get files
        files.PFG = foreach (di.mod = dirs.mod, .combine = "cbind") %do%
        {
          files.PFG = list.files(path = di.mod
                                 , pattern = paste0("^", mod)
                                 , full.names = TRUE)
          if (length(files.PFG) != no_PFG)
          {
            stop(paste0("There is not the same number of files "
                        , "(`.txt` file starting with `", mod, "`) "
                        , "into the ", di.mod, "/ folder as the number of PFG "
                        , "indicated into the file "
                        , globi))
          }
          return(data.frame(files.PFG, stringsAsFactors = FALSE))
        }
        assign(x = paste0("files.PFG.", mod), value = files.PFG)
        assign(x = paste0("no_files.PFG.", mod)
               , value = ncol(get(paste0("files.PFG.", mod))))
      } else
      {
        assign(x = paste0("files.PFG.", mod), value = "")
        assign(x = paste0("no_files.PFG.", mod), value = 0)
      }
    }
    
    no_files.PFG = sapply(MODULES, function(x) get(paste0("no_files.PFG.", x)))
    no_files.PFG = no_files.PFG[which(no_files.PFG > 0)]
    if (length(unique(no_files.PFG)) > 1)
    {
      PFG.combi = expand.grid(sapply(no_files.PFG, function(x) 1:x))
    } else
    {
      PFG.combi = matrix(rep(0, length(no_files.PFG) * unique(no_files.PFG))
                         , ncol = length(no_files.PFG))
      PFG.combi = as.data.frame(PFG.combi)
      colnames(PFG.combi) = names(no_files.PFG)
      for (mod in 1:length(no_files.PFG))
      {
        PFG.combi[[names(no_files.PFG)[mod]]] = 1:no_files.PFG[mod]
      }
    }
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("DIST", "DROUGHT", "FIRE")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_notChar.m(paste0("name.", mod), get(paste0("name.", mod)))
        .testParam_existFile(paste0(name.simulation
                                    , "/DATA/MASK/"
                                    , get(paste0("name.", mod))))
        assign(x = paste0("no.", mod)
               , value = .getParam(params.lines = globi
                                   , flag = paste0(mod, "_NO")
                                   , flag.split = " "
                                   , is.num = TRUE))
      } else
      {
        assign(x = paste0("no.", mod), value = 0)
      }
    }
    
    ### -------------------------------------------------------------------- ###
    
    MODULES = c("HABSUIT", "ALIENS")
    for (mod in MODULES)
    {
      if (get(paste0("do.", mod)))
      {
        .testParam_existFolder(name.simulation, paste0("DATA/PFGS/", mod, "/"))
        
        if (PARAMS.combi[[paste0("PFG.", mod)]][i] > 0)
        {
          eval(parse(text = paste0("di = dirs.", mod
                                   , "[PARAMS.combi$PFG.", mod, "[i]]")))
          assign(x = paste0("files.PFG.", mod)
                 , value = list.files(path = di, full.names = TRUE))
          if (length(get(paste0("files.PFG.", mod))) != no_PFG)
          {
            warning(paste0("There is not the same number of files into the "
                           , "DATA/PFGS/", mod, "/ folder as the number of PFG "
                           , "indicated into the file "
                           , globi))
          }
        } else
        {
          assign(x = paste0("files.PFG.", mod), value = "")
        }
      } else
      {
        assign(x = paste0("files.PFG.", mod), value = "")
      }
    }
    
    #############################################################################
    
    FILE_NUMBER = 0
    if (nrow(PARAMS.combi) == 1 && nrow(PFG.combi) == 1)
    {
      FILE_NUMBER = length(list.files(path = paste0(name.simulation, "/PARAM_SIMUL/")
                                      , pattern = "^Simul_parameters_V"
                                      , recursive = FALSE
                                      , include.dirs = FALSE)) + 1
    }
    
    #############################################################################
    #############################################################################
    
    for (ii in 1:nrow(PFG.combi))
    {
      params.list = lapply(1:ncol(params.combi)
                           , function(x) { as.character(params.combi[, x]) })
      names.params.list = names.params.combi
      
      if (FILE_NUMBER == 0)
      {
        params.list = c(params.list, list(paste0(name.simulation
                                                 , "/RESULTS/SIMUL_V"
                                                 , i, ".", ii)))
      } else
      {
        params.list = c(params.list, list(paste0(name.simulation
                                                 , "/RESULTS/SIMUL_V"
                                                 , FILE_NUMBER)))
      }
      names.params.list = c(names.params.list, "--SAVING_DIR--")
      
      params.list = c(params.list, list(files.PFG.SUCC[, PFG.combi$SUCC[ii]]))
      names.params.list = c(names.params.list, "--PFG_PARAMS_LIFE_HISTORY--")
      
      if (exists("SCENARIO.MASK"))
      {
        params.list = c(params.list, list(SCENARIO.MASK))
        names.params.list = c(names.params.list, "--MASK_CHANGEMASK_FILES--")
      }
      
      if (do.LIGHT)
      {
        params.list = c(params.list, list(files.PFG.LIGHT[, PFG.combi$LIGHT[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_LIGHT--")
      }
      if (do.SOIL)
      {
        params.list = c(params.list, list(files.PFG.SOIL[, PFG.combi$SOIL[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_SOIL--")
      }
      if (do.DISP)
      {
        params.list = c(params.list, list(files.PFG.DISP[, PFG.combi$DISP[ii]]))
        names.params.list = c(names.params.list, "--PFG_PARAMS_DISPERSAL--")
      }
      if (do.HABSUIT)
      {
        params.list = c(params.list, list(files.PFG.HABSUIT))
        names.params.list = c(names.params.list, "--PFG_MASK_HABSUIT--")
        if (exists("SCENARIO.HABSUIT"))
        {
          params.list = c(params.list, list(SCENARIO.HABSUIT))
          names.params.list = c(names.params.list, "--HABSUIT_CHANGEMASK_FILES--")
        }
      }
      if (do.DIST)
      {
        params.list = c(params.list
                        , list(files.PFG.DIST[, PFG.combi$DIST[ii]])
                        , list(rep(paste0(name.simulation
                                          , "/DATA/MASK/"
                                          , name.DIST)
                                   , no.DIST)))
        names.params.list = c(names.params.list
                              , "--PFG_PARAMS_DISTURBANCES--"
                              , "--DIST_MASK--")
        if (exists("SCENARIO.DIST"))
        {
          params.list = c(params.list, list(SCENARIO.DIST))
          names.params.list = c(names.params.list, "--DIST_CHANGEMASK_FILES--")
        }
      }
      if (do.DROUGHT)
      {
        params.list = c(params.list
                        , list(files.PFG.DROUGHT[, PFG.combi$DROUGHT[ii]])
                        , list(paste0(name.simulation
                                      , "/DATA/MASK/"
                                      , name.DROUGHT)))
        names.params.list = c(names.params.list
                              , "--PFG_PARAMS_DROUGHT--"
                              , "--DROUGHT_MASK--")
        if (exists("SCENARIO.DROUGHT"))
        {
          params.list = c(params.list, list(SCENARIO.DROUGHT))
          names.params.list = c(names.params.list, "--DROUGHT_CHANGEMASK_FILES--")
        }
      }
      if (do.ALIENS)
      {
        params.list = c(params.list, list(files.PFG.ALIENS))
        names.params.list = c(names.params.list, "--PFG_MASK_ALIENS--")
        if (exists("SCENARIO.ALIENS"))
        {
          params.list = c(params.list, list(SCENARIO.ALIENS))
          names.params.list = c(names.params.list, "--ALIENS_CHANGEMASK_FILES--")
        }
      }
      if (do.FIRE)
      {
        params.list = c(params.list
                        , list(files.PFG.FIRE[, PFG.combi$FIRE[ii]])
                        , list(rep(paste0(name.simulation
                                          , "/DATA/MASK/"
                                          , name.FIRE)
                                   , no.FIRE)))
        names.params.list = c(names.params.list
                              , "--PFG_PARAMS_FIRE--"
                              , "--FIRE_MASK--")
        if (exists("SCENARIO.FIRE"))
        {
          params.list = c(params.list, list(SCENARIO.FIRE))
          names.params.list = c(names.params.list, "--FIRE_CHANGEMASK_FILES--")
        }
      }
      
      ### -------------------------------------------------------------------- ###
      
      params = c(params.list, list(""))
      names(params) = c(names.params.list, "--END_OF_FILE--")
      
      .createParams(params.file = paste0(name.simulation
                                         , "/PARAM_SIMUL/Simul_parameters_V"
                                         , ifelse(FILE_NUMBER == 0
                                                  , paste0(i, ".", ii)
                                                  , FILE_NUMBER)
                                         , ".txt")
                    , params.list = params
                    , separator = "\n")
      
    }
  }
}
