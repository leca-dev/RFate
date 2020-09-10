### HEADER #####################################################################
##' @title Create multiple set(s) of parameter files for a \code{FATE} 
##' simulation
##' 
##' @name PRE_FATE.params_multipleSet
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create multiple sets of parameters 
##' using Latin Hypercube Sampling to help find best combination of parameters 
##' (see \code{Details})
##'              
##' @param name.simulation.1 a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation from which to 
##' retrieve the first parameter simulation file (\code{file.simulParam.1}), 
##' and the second if given (\code{file.simulParam.2}) and no other directory 
##' provided (\code{name.simulation.2 = NULL})
##' @param name.simulation.2 (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the main directory or simulation name of 
##' the \code{FATE} simulation from which to retrieve the second parameter 
##' simulation file (\code{file.simulParam.2})
##' @param file.simulParam.1 a \code{string} corresponding to the name of the 
##' simulation parameter file from which to retrieve parameter values that will 
##' be used to build the multiple set of new parameters
##' @param file.simulParam.2 (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the name of the second simulation parameter 
##' file from which to retrieve parameter values that will be used to build 
##' parameter ranges in comparison with values from \code{file.simulParam.1}
##' @param no_simulations an \code{integer} corresponding to the number of set 
##' of parameters that will be produced according to Latin Hypercube Sampling 
##' (LHS)
##' @param opt.percent_maxAbund default \code{0.5}. Amount of variation 
##' (between \code{0} and \code{1}) around the original value of 
##' \code{MAX_ABUND_LOW}, \code{MAX_ABUND_MEDIUM}, \code{MAX_ABUND_HIGH} if 
##' selected
##' @param opt.percent_seeding default \code{0.5}. Amount of variation (between 
##' \code{0} and \code{1}) around the original value of \code{SEEDING_DURATION}, 
##' \code{SEEDING_TIMESTEP}, \code{SEEDING_INPUT} if selected
##' @param opt.percent_light default \code{0.5}. Amount of variation (between 
##' \code{0} and \code{1}) around the original value of 
##' \code{LIGHT_THRESH_MEDIUM}, \code{LIGHT_THRESH_LOW} if selected
##' @param opt.percent_soil default \code{0.5}. Amount of variation (between 
##' \code{0} and \code{1}) around the original value of \code{SOIL_INIT}, 
##' \code{SOIL_RETENTION} if selected
##' @param do.max_abund_low default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_LOW} parameter within \emph{Global_parameters} file will be 
##' declined into a range of values
##' @param do.max_abund_medium default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_MEDIUM} parameter within \emph{Global_parameters} file will 
##' be declined into a range of values
##' @param do.max_abund_high default \code{TRUE}. If \code{TRUE}, 
##' \code{MAX_ABUND_HIGH} parameter within \emph{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_duration default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_DURATION} parameter within \emph{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_timestep default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_TIMESTEP} parameter within \emph{Global_parameters} file will 
##' be declined into a range of values
##' @param do.seeding_input default \code{TRUE}. If \code{TRUE}, 
##' \code{SEEDING_INPUT} parameter within \emph{Global_parameters} file will be 
##' declined into a range of values
##' @param do.no_strata default \code{TRUE}. If \code{TRUE}, \code{NO_STRATA} 
##' parameter within \emph{Global_parameters} file will be declined into a range 
##' of values, with potential impact on some parameters within PFG succession 
##' (and light) files (parameters \code{STRATA}, \code{MAX_ABUNDANCE}, 
##' \code{IMM_SIZE}, \code{CHANG_STR_AGES} (and \code{LIGHT_TOL}), see 
##' \code{\link{PRE_FATE.params_PFGsuccession}} (and 
##' \code{\link{PRE_FATE.params_PFGlight}}))
##' @param do.LIGHT.thresh_medium default \code{TRUE}. If \code{TRUE}, 
##' \code{LIGHT_THRESH_MEDIUM} parameter within \emph{Global_parameters} file 
##' will be declined into a range of values 
##' @param do.LIGHT.thresh_low default \code{TRUE}. If \code{TRUE}, 
##' \code{LIGHT_THRESH_LOW} parameter within \emph{Global_parameters} file 
##' will be declined into a range of values 
##' @param do.SOIL.init default \code{TRUE}. If \code{TRUE}, \code{SOIL_INIT} 
##' parameter within \emph{Global_parameters} file will be declined into a range 
##' of values 
##' @param do.SOIL.retention default \code{TRUE}. If \code{TRUE}, 
##' \code{SOIL_RETENTION} parameter within \emph{Global_parameters} file will be 
##' declined into a range of values 
##' @param do.DISPERSAL.mode default \code{TRUE}. If \code{TRUE}, 
##' \code{DISPERSAL_MODE} parameter within \emph{Global_parameters} file will be 
##' declined into its three possible values (either either packets kernel 
##' (\code{1}), exponential kernel (\code{2}) or exponential kernel with 
##' probability (\code{3}), see \code{\link{PRE_FATE.params_globalParameters}})
##' @param do.HABSUIT.mode default \code{TRUE}. If \code{TRUE}, 
##' \code{HABSUIT_MODE} parameter within \emph{Global_parameters} file will be 
##' declined into its two possible values (either random (\code{1}) or PFG 
##' specific (\code{2}), see \code{\link{PRE_FATE.params_globalParameters}})
##' 
##' 
##' 
##' @details 
##' 
##' A \code{FATE} simulation requires several parameters to define general 
##' characteristics of the simulation : they are saved within a 
##' \emph{Global_parameters} file (see 
##' \code{\link{PRE_FATE.params_globalParameters}}). To fit the model to a 
##' particular area and set of Plant Functional Groups (PFG), these are the 
##' parameters that should be optimized, since they are not data-dependant, 
##' unlike, for example, parameters related to PFG (height, maturity, dispersal 
##' distances, soil tolerance, etc).
##' 
##' (\emph{Note : this is true, except when varying the number of strata, which 
##' will have an impact on some parameters within SUCC and LIGHT PFG parameter 
##' files.})
##' 
##' \strong{The main idea is to start from a complete simulation folder, to 
##' select the parameters that should vary, and to create new parameter files 
##' with new parameter values based on pre-existing values. \cr \cr}
##' 
##' Three possible scenarios are available :
##' \describe{
##'   \item{1 folder - \cr 1 simulation file}{
##'   \itemize{
##'     \item requested parameter values are extracted from the given 
##'     simulation file
##'     \item ranges are assigned to each parameter according to the specified 
##'     value \cr e.g. : if \code{opt.percent_seeding = 0.5}, and 
##'     \code{do.seeding_duration} is asked, values will be generated for this 
##'     parameter between : 
##'     \deqn{\text{SEEDING_DURATION} \pm \text{SEEDING_DURATION} * 
##'     \frac{50}{100}}
##'     \item according to the required number of parameter sets to be produced 
##'     (\code{no_simulations}), Latin Hypercube Sampling is applied to select 
##'     each new parameter values
##'     \item parameter files are created for these new parameter values \cr \cr
##'   }
##'   }
##'   \item{1 folder - \cr 2 simulation files}{
##'   \itemize{
##'     \item same as 1st scenario
##'     \item ranges assigned to each parameter correspond to the extracted 
##'     values (e.g. : if do.seeding_duration is asked, values will be 
##'     generated for this parameter between : \code{SEEDING_DURATION 
##'     (file_simulation.1)} and \code{SEEDING_DURATION (file_simulation.2)} 
##'     \cr \cr
##'   }
##'   }
##'   \item{2 folders - \cr 2 simulation files}{
##'   \itemize{
##'     \item same as 2nd scenario, except that the two given simulation files 
##'     come from two different simulation folders \cr \cr
##'   }
##'   }
##' }
##' 
##' \strong{Latin Hypercube Sampling} is a statistical method to generate a 
##' sampling of parameter values from a multidimensional space, while ensuring 
##' a good representation of the real variability. The range of each parameter 
##' is known, and depending on the number of set of parameters asked to be 
##' obtained at the end, each range is more or less finely cut and values are 
##' drawn in order to explore the whole space of combinations.
##' 
##' 
##' 
##' 
##' @return A new folder containing the different sets of parameters asked. 
##' 
##' Depending on what elements have been asked to be varied, three types of 
##' files can have been modified :
##' \enumerate{
##'   \item the global parameter file
##'   \item the PFG succession files
##'   \item the PFG light succession files \cr \cr
##' }
##' 
##' Below are listed the parameters that can change (if selected) within each 
##' file :
##' 
##' \itemize{
##'   \item Into the \code{name.simulation/DATA/GLOBAL_PARAMETERS} folder :
##'   \itemize{
##'     \item NO_STRATA
##'     \item SEEDING_DURATION
##'     \item SEEDING_TIMESTEP
##'     \item SEEDING_INPUT
##'     \item MAX_ABUND_LOW
##'     \item MAX_ABUND_MEDIUM 
##'     \item MAX_ABUND_HIGH \cr \cr
##'   }
##'   If the simulation includes \emph{light competition} :
##'   \itemize{
##'     \item LIGHT_THRESH_MEDIUM
##'     \item LIGHT_THRESH_LOW
##'   }
##'   If the simulation includes \emph{soil competition} :
##'   \itemize{
##'     \item SOIL_INIT
##'     \item SOIL_RETENTION
##'   }
##'   If the simulation includes \emph{dispersal} :
##'   \itemize{
##'     \item DISPERSAL_MODE
##'   }
##'   If the simulation includes \emph{habitat suitability} :
##'   \itemize{
##'     \item HABSUIT_MODE \cr \cr
##'   }
##'   \item Into the \code{name.simulation/DATA/PFGS/SUCC} folder :
##'   \itemize{
##'     \item STRATA
##'     \item MAX_ABUNDANCE
##'     \item IMM_SIZE
##'     \item CHANG_STR_AGES \cr \cr
##'   }
##'   \item Into the \code{name.simulation/DATA/PFGS/LIGHT} folder :
##'   \itemize{
##'     \item LIGHT_TOL
##'   }
##' }
##' 
##' 
##' @keywords FATE, simulation, Latin Hypercube Sampling
##' 
##' @seealso \code{\link[SPOT]{designLHD}},
##' \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGsuccession}},
##' \code{\link{PRE_FATE.params_PFGlight}},
##' 
##' 
##' @examples
##' 
##' 
##' ## Load example data
##' 
##' 
##' @export
##' 
##' @importFrom SPOT designLHD
##'
## END OF HEADER ###############################################################


PRE_FATE.params_multipleSet = function(
  name.simulation.1
  , name.simulation.2 = NULL
  , file.simulParam.1
  , file.simulParam.2 = NULL
  , no_simulations
  , opt.percent_maxAbund = 0.5
  , opt.percent_seeding = 0.5
  , opt.percent_light = 0.5
  , opt.percent_soil = 0.5
  , do.max_abund_low = TRUE
  , do.max_abund_medium = TRUE
  , do.max_abund_high = TRUE
  , do.seeding_duration = TRUE
  , do.seeding_timestep = TRUE
  , do.seeding_input = TRUE
  , do.no_strata = TRUE
  , do.LIGHT.thresh_medium = TRUE
  , do.LIGHT.thresh_low = TRUE
  , do.SOIL.init = TRUE
  , do.SOIL.retention = TRUE
  , do.DISPERSAL.mode = TRUE
  , do.HABSUIT.mode = TRUE
){
  
  #############################################################################
  
  scenario1 = scenario2 = scenario3 = FALSE
  
  ## CHECK parameter name.simulation.1
  .testParam_existFolder(name.simulation.1, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation.1, "DATA/GLOBAL_PARAMETERS/")
  name.simulation.1 = sub("/$", "", name.simulation.1)
  
  ## CHECK parameter file.simulParam.1
  if (.testParam_notChar(file.simulParam.1))
  {
    abs.simulParams = list.files(paste0(name.simulation.1, "/PARAM_SIMUL/"))
    if (length(abs.simulParams) == 0)
    {
      stop(paste0("Missing data!\n The folder ", name.simulation.1
                  , "/PARAM_SIMUL/ does not contain adequate files"))
    } else
    {
      stop(paste0("Missing data!\n The folder "
                  , name.simulation.1
                  , "/PARAM_SIMUL/ contain one or more files.\n"
                  , "You must select one with the `file.simulParam.1` parameter "))
    }
  } else
  {
    file.simulParam.1 = basename(file.simulParam.1)
    file.simulParam.1 = paste0(name.simulation.1, "/PARAM_SIMUL/", file.simulParam.1)
    .testParam_existFile(file.simulParam.1)
    scenario1 = TRUE
  }
  ## CHECK parameter file.simulParam.2
  if (!.testParam_notChar(file.simulParam.2))
  {
    if (!.testParam_notChar(name.simulation.2))
    {
      .testParam_existFolder(name.simulation.2, "PARAM_SIMUL/")
      .testParam_existFolder(name.simulation.2, "DATA/GLOBAL_PARAMETERS/")
      name.simulation.2 = sub("/$", "", name.simulation.2)
      
      file.simulParam.2 = basename(file.simulParam.2)
      file.simulParam.2 = paste0(name.simulation.2, "/PARAM_SIMUL/", file.simulParam.2)
      .testParam_existFile(file.simulParam.2)
      
      if (name.simulation.1 == name.simulation.2 &&
          file.simulParam.1 == file.simulParam.2)
      {
        stop(paste0("You must select different simulation parameter files !"))
      }
      scenario1 = FALSE
      scenario3 = TRUE
    } else
    {
      file.simulParam.2 = basename(file.simulParam.2)
      file.simulParam.2 = paste0(name.simulation.1, "/PARAM_SIMUL/", file.simulParam.2)
      .testParam_existFile(file.simulParam.2)
      if (file.simulParam.1 == file.simulParam.2)
      {
        stop(paste0("You must select different simulation parameter files !"))
      }
      scenario1 = FALSE
      scenario2 = TRUE
    }
  }
  ## CHECK parameter no_simulations
  .testParam_notInteger.m("no_simulations", no_simulations)
  .testParam_notRound.m("no_simulations", no_simulations)
  ## CHECK parameters scenario1
  if (scenario1)
  {
    .testParam_notNum.m("opt.percent_maxAbund", opt.percent_maxAbund)
    .testParam_notBetween.m("opt.percent_maxAbund", opt.percent_maxAbund, 0, 1)
    .testParam_notNum.m("opt.percent_seeding", opt.percent_seeding)
    .testParam_notBetween.m("opt.percent_seeding", opt.percent_seeding, 0, 1)
    .testParam_notNum.m("opt.percent_light", opt.percent_light)
    .testParam_notBetween.m("opt.percent_light", opt.percent_light, 0, 1)
    .testParam_notNum.m("opt.percent_soil", opt.percent_soil)
    .testParam_notBetween.m("opt.percent_soil", opt.percent_soil, 0, 1)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.params_multipleSet")
  cat("\n #------------------------------------------------------------# \n")
  
  
  #############################################################################
  
  if (sum(c(do.max_abund_low
            , do.max_abund_medium
            , do.max_abund_high
            , do.seeding_duration
            , do.seeding_timestep
            , do.seeding_input
            , do.LIGHT.thresh_medium
            , do.LIGHT.thresh_low
            , do.SOIL.init
            , do.SOIL.retention
            , do.HABSUIT.mode
            , do.DISPERSAL.mode
            , do.no_strata)) == 0)
  {
    stop("You must select some parameters to vary !")
  }
  
  get_checked = vector("list", 7)
  
  if (do.max_abund_low){
    get_checked[[1]] = c(get_checked[[1]], "max_abund_low")
  }
  if (do.max_abund_medium){
    get_checked[[1]] = c(get_checked[[1]], "max_abund_medium")
  }
  if (do.max_abund_high){
    get_checked[[1]] = c(get_checked[[1]], "max_abund_high")
  }
  if (do.seeding_duration){
    get_checked[[2]] = c(get_checked[[2]], "seeding_duration")
  }
  if (do.seeding_timestep){
    get_checked[[2]] = c(get_checked[[2]], "seeding_timestep")
  }
  if (do.seeding_input){
    get_checked[[2]] = c(get_checked[[2]], "seeding_input")
  }
  if (do.LIGHT.thresh_medium){
    get_checked[[3]] = c(get_checked[[3]], "light_thresh_medium")
  }
  if (do.LIGHT.thresh_low){
    get_checked[[3]] = c(get_checked[[3]], "light_thresh_low")
  }
  if (do.SOIL.init){
    get_checked[[4]] = c(get_checked[[4]], "soil_init")
  }
  if (do.SOIL.retention){
    get_checked[[4]] = c(get_checked[[4]], "soil_retention")
  }
  if (do.HABSUIT.mode){
    get_checked[[5]] = c(get_checked[[5]], "habsuit_mode")
  }
  if (do.DISPERSAL.mode){
    get_checked[[6]] = c(get_checked[[6]], "dispersal_mode")
  }
  if (do.no_strata){
    get_checked[[7]] = c(get_checked[[7]], "no_strata")
  }
  
  get_sliders = c(opt.percent_maxAbund, opt.percent_seeding
                  , opt.percent_light, opt.percent_soil)
  
  GLOBAL.names.params = c("max_abund_low" = "MAX_ABUND_LOW"
                          , "max_abund_medium" = "MAX_ABUND_MEDIUM"
                          , "max_abund_high" = "MAX_ABUND_HIGH"
                          , "seeding_duration" = "SEEDING_DURATION"
                          , "seeding_timestep" = "SEEDING_TIMESTEP"
                          , "seeding_input" = "SEEDING_INPUT"
                          , "light_thresh_medium" = "LIGHT_THRESH_MEDIUM"
                          , "light_thresh_low" = "LIGHT_THRESH_LOW"
                          , "soil_init" = "SOIL_INIT"
                          , "soil_retention" = "SOIL_RETENTION"
                          , "habsuit_mode" = "HABSUIT_MODE"
                          , "dispersal_mode" = "DISPERSAL_MODE"
                          , "no_strata" = "NO_STRATA")
  
  
  get_toSuppr = c("GLOBAL_PARAMS", "SAVE_DIR", "END_OF_FILE")
  if ("no_strata" %in% get_checked)
  {
    get_toSuppr = c(get_toSuppr, "PFG_PARAMS_LIFE_HISTORY", "PFG_PARAMS_LIGHT")
  }
  for (i in get_checked)
  {
    get_toSuppr = c(get_toSuppr, as.vector(GLOBAL.names.params[i]))
  }
  
  #############################################################################
  
  get_PARAMS = function(path_folder, file_simul, params)
  {
    ## GET FILE informations
    
    ## Simulation parameter file
    abs.simulParam = paste0(path_folder, "/PARAM_SIMUL/", file_simul)
    lines.simulParam = readLines(abs.simulParam)
    if (length(lines.simulParam) == 0)
    {
      stop(paste0("The file ", abs.simulParam, " is empty. Please check."))
    }
    
    ind = grep("^--.*--$", lines.simulParam)
    if (length(ind) == 0)
    {
      stop(paste0("The file ", abs.simulParam
                  , " does not contain any parameter values with "
                  , "the --PARAM-- flag. Please check."))
    }
    if (length(grep("--END_OF_FILE--", lines.simulParam)) == 0)
    {
      stop(paste0("The file ", abs.simulParam
                  , " does not contain the --END_OF_FILE-- flag. Please check."))
    }
    
    params.simulParam = lines.simulParam[ind]
    params.simulParam = gsub("--", "", params.simulParam)
    params.simulParam.TOKEEP = params.simulParam[which(!(params.simulParam %in% get_toSuppr))]
    
    if (length(params.simulParam.TOKEEP) > 0)
    {
      params.simulParam.TOKEEP = paste0("--", params.simulParam.TOKEEP, "--")
      toKeep = c()
      for (i in sapply(params.simulParam.TOKEEP, function(x) grep(x, lines.simulParam)))
      {
        toKeep = c(toKeep, lines.simulParam[i:(ind[which(ind == i) + 1] - 1)])
      }
      params.simulParam.TOKEEP = toKeep
    }
    
    ## Get succession and light PFG files
    ind1 = ifelse(length(grep("PFG_PARAMS_LIFE_HISTORY", lines.simulParam)) > 0
                  , grep("PFG_PARAMS_LIFE_HISTORY", lines.simulParam) + 1
                  , 0) 
    ind2 = ifelse(ind1 > 0, ind[which(ind == (ind1 - 1)) + 1] - 1, 0)
    ind3 = ifelse(length(grep("PFG_PARAMS_LIGHT", lines.simulParam)) > 0
                  , grep("PFG_PARAMS_LIGHT", lines.simulParam) + 1
                  , 0) 
    ind4 = ifelse(ind3 > 0, ind[which(ind == (ind3 - 1)) + 1] - 1, 0)
    params.simulParam.SUCC_LIGHT = list(SUCC = lines.simulParam[ind1:ind2]
                                        , LIGHT = lines.simulParam[ind3:ind4])
    
    ## Global parameter file
    file.globalParam = .getParam(params.lines = abs.simulParam
                                 , flag = "GLOBAL_PARAMS"
                                 , flag.split = "^--.*--$"
                                 , is.num = FALSE)
    file.globalParam = paste0(dirname(path_folder), "/", file.globalParam)
    if (!file.exists(file.globalParam))
    {
      .stopMessage_existFile(file.globalParam)
    }
    
    lines.globalParam = readLines(file.globalParam)
    if (length(lines.globalParam) == 0)
    {
      stop(paste0("The file ", file.globalParam, " is empty. Please check."))
    }
    
    params.globalParam = sapply(lines.globalParam
                                , function(x) strsplit(as.character(x), " ")[[1]][1])
    params.globalParam = as.vector(params.globalParam)
    params.globalParam.TOKEEP = lines.globalParam[which(!(params.globalParam %in% get_toSuppr))]
    if (length(params.globalParam.TOKEEP) == 0)
    {
      stop(paste0("The file ", file.globalParam
                  , " does not contain any of the required parameter values "
                  , "(NO_PFG, SIMULATION_DURATION, ...). Please check."))
    } else if (length(grep("##", params.globalParam.TOKEEP)) > 0)
    {
      params.globalParam.TOKEEP = params.globalParam.TOKEEP[-grep("##", params.globalParam.TOKEEP)]
    }
    
    ## Remove parameters not present in global file
    for (i in unlist(params))
    {
      if (length(grep(GLOBAL.names.params[i], params.globalParam)) == 0)
      {
        for (y in 1:length(params))
        {
          if (length(params[[y]]) > 0)
          {
            toSuppr = c()
            for (x in 1:length(params[[y]]))
            {
              if (!is.null(params[[y]][x]) && params[[y]][x] == i)
              {
                toSuppr = c(toSuppr, x)
                warning(paste0("The parameter '"
                               , i
                               , "' is not defined in the global file :\n"
                               , basename(file.globalParam)
                               , "\n from the simulation file :\n"
                               , basename(abs.simulParam)
                               , "\n\nIt will not be considered."))
              }
            }
            if (length(toSuppr) > 0)
            {
              params[[y]] = params[[y]][-toSuppr]
            }
          }
        }
      }
    }
    
    ## Get parameters value
    PARAMS = lapply(params[c(1:4, 7)], function(y) {
      sapply(y, function(x) {
        if (!is.null(x))
        {
          return(.getParam(params.lines = file.globalParam
                           , flag = as.vector(GLOBAL.names.params[x])
                           , flag.split = " "
                           , is.num = TRUE))
        }
      })
    })
    
    return(list(PARAMS = PARAMS
                , TOKEEP.simul = params.simulParam.TOKEEP
                , TOKEEP.global = params.globalParam.TOKEEP
                , SUCC_LIGHT.simul = params.simulParam.SUCC_LIGHT))
  }
  
  #############################################################################
  
  get_ranges = function()
  {
    ## GET FILE 1 informations
    PARAMS1 = get_PARAMS(path_folder = name.simulation.1
                         , file_simul = basename(file.simulParam.1)
                         , params = get_checked)
    
    TOKEEP1.simul = PARAMS1$TOKEEP.simul
    TOKEEP1.global = PARAMS1$TOKEEP.global
    SUCC_LIGHT1.simul = PARAMS1$SUCC_LIGHT.simul
    PARAMS1 = PARAMS1$PARAMS
    
    if (is.null(unlist(PARAMS1)))
    { 
      if ("dispersal_mode" %in% get_checked ||
          "habsuit_mode" %in% get_checked)
      {
        return(list(PARAMS.range = data.frame()
                    , TOKEEP.global = TOKEEP1.global
                    , TOKEEP.simul = TOKEEP1.simul
                    , SUCC_LIGHT.simul = SUCC_LIGHT1.simul))
      } else
      {
        stop(paste0("The global parameter file indicated in ", file.simulParam.1
                    , " does not contain any of the required parameter values ("
                    , paste0(as.vector(GLOBAL.names.params[unlist(get_checked)])
                             , collapse = ", ")
                    , "). Please check."))
      }
    }
    
    if (scenario1)
    {
      ## ---------------------------------------------------------------------- 
      ff = function()
      {
        lapply(1:length(PARAMS1), function(y) {
          if (length(PARAMS1[[y]]) > 0)
          {
            sapply(1:length(PARAMS1[[y]]), function(x) {
              if (!is.null(PARAMS1[[y]][x]))
              {
                res = todo(x, y)
                names(res) = names(PARAMS1[[y]][x])
                return(res)
              }
            })
          }
        })
      }
      
      todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) * get_sliders[y] / 100) }
      PARAMS.ecart = ff()
      todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) - PARAMS.ecart[[y]][x]) }
      PARAMS.min = ff()
      todo = function(x, y) { return(as.vector(PARAMS1[[y]][x]) + PARAMS.ecart[[y]][x]) }
      PARAMS.max = ff()
      
      PARAMS.range = rbind(as.integer(unlist(PARAMS.min))
                           , as.integer(unlist(PARAMS.max)))
      PARAMS.range = as.data.frame(PARAMS.range)
      colnames(PARAMS.range) = names(unlist(PARAMS.min))
      rownames(PARAMS.range) = c("min", "max")
      # if ("soil_init" %in% colnames(PARAMS.range))
      # {
      #   if (PARAMS.range[1, "soil_init"] < 0)
      #   {
      #     PARAMS.range[1, "soil_init"] = 0
      #   }
      # }
      if ("soil_retention" %in% colnames(PARAMS.range))
      {
        if (PARAMS.range[1, "soil_retention"] < 0)
        {
          PARAMS.range[1, "soil_retention"] = 0
        }
        if (PARAMS.range[1, "soil_retention"] > 1)
        {
          PARAMS.range[1, "soil_retention"] = 1
        }
      }
      # ind_notSoil = which(!(colnames(PARAMS.range) %in% c("soil_init", "soil_retention")))
      ind_notSoil = which(!(colnames(PARAMS.range) %in% c("soil_retention")))
      if (length(which(PARAMS.range[1, ind_notSoil] < 1)) > 0)
      {
        PARAMS.range[1, which(PARAMS.range[1, ind_notSoil] < 1)] = 1
      }
      if ("no_strata" %in% colnames(PARAMS.range))
      {
        PARAMS.range[, "no_strata"] = c(1, PARAMS1[[5]][1])
      }
      return(list(PARAMS.range = PARAMS.range
                  , TOKEEP.global = TOKEEP1.global
                  , TOKEEP.simul = TOKEEP1.simul
                  , SUCC_LIGHT.simul = SUCC_LIGHT1.simul))
      
      ## END STRATEGY 1
    } else
    {
      ## GET FILE 2 informations
      PARAMS2 = get_PARAMS(path_folder = ifelse(scenario2
                                                , name.simulation.1
                                                , name.simulation.2)
                           , file_simul = basename(file.simulParam.2)
                           , params = get_checked)
      
      ## ---------------------------------------------------------------------- 
      TOKEEP2.simul = PARAMS2$TOKEEP.simul
      TOKEEP2.global = PARAMS2$TOKEEP.global
      PARAMS2 = PARAMS2$PARAMS
      
      if (length(unlist(PARAMS1)) != length(unlist(PARAMS2)) ||
          sum(names(unlist(PARAMS1)) == names(unlist(PARAMS2))) != length(unlist(PARAMS1)))
      {
        stop(paste0("The files do not contain the same parameters to be evaluated.\n"
                    , "\n File 1 : '"
                    , paste0(names(unlist(PARAMS1)), collapse = "', '")
                    , "'\n File 2 : '"
                    , paste0(names(unlist(PARAMS2)), collapse = "', '")
                    , "'\n\nPlease check."))
      } else if (length(TOKEEP1.global) != length(TOKEEP2.global) ||
                 sum(TOKEEP1.global == TOKEEP2.global) != length(TOKEEP1.global))
      {
        stop("The global files have different fixed parameter values.\nPlease check.")
      } else if (length(TOKEEP1.simul) != length(TOKEEP2.simul) ||
                 sum(sub(name.simulation.1, "", TOKEEP1.simul) == 
                     sub(ifelse(scenario2, name.simulation.1, name.simulation.2)
                         , "", TOKEEP2.simul)) != length(TOKEEP1.simul))
      {
        stop("The simulation files have different fixed parameter values.\nPlease check.")
      } else
      {
        PARAMS.min = sapply(1:length(unlist(PARAMS1))
                            , function(x) { min(c(unlist(PARAMS1)[x]
                                                  , unlist(PARAMS2)[x])) })
        PARAMS.max = sapply(1:length(unlist(PARAMS1))
                            , function(x) { max(c(unlist(PARAMS1)[x]
                                                  , unlist(PARAMS2)[x])) })
        names(PARAMS.min) = names(PARAMS.max) = names(unlist(PARAMS1))
        
        PARAMS.range = rbind(as.integer(unlist(PARAMS.min))
                             , as.integer(unlist(PARAMS.max)))
        PARAMS.range = as.data.frame(PARAMS.range)
        colnames(PARAMS.range) = names(unlist(PARAMS.min))
        rownames(PARAMS.range) = c("min", "max")
        # if ("soil_init" %in% colnames(PARAMS.range))
        # {
        #   if (PARAMS.range[1, "soil_init"] < 0)
        #   {
        #     PARAMS.range[1, "soil_init"] = 0
        #   }
        # }
        if ("soil_retention" %in% colnames(PARAMS.range))
        {
          if (PARAMS.range[1, "soil_retention"] < 0)
          {
            PARAMS.range[1, "soil_retention"] = 0
          }
          if (PARAMS.range[1, "soil_retention"] > 1)
          {
            PARAMS.range[1, "soil_retention"] = 1
          }
        }
        # ind_notSoil = which(!(colnames(PARAMS.range) %in% c("soil_init", "soil_retention")))
        ind_notSoil = which(!(colnames(PARAMS.range) %in% c("soil_retention")))
        if (length(which(PARAMS.range[1, ind_notSoil] < 1)) > 0)
        {
          PARAMS.range[1, which(PARAMS.range[1, ind_notSoil] < 1)] = 1
        }
        if ("no_strata" %in% colnames(PARAMS.range))
        {
          PARAMS.range[, "no_strata"] = c(1, max(c(PARAMS1[[5]][1]
                                                   , PARAMS2[[5]][1])))
        }
        return(list(PARAMS.range = PARAMS.range
                    , TOKEEP.global = TOKEEP1.global
                    , TOKEEP.simul = TOKEEP1.simul
                    , SUCC_LIGHT.simul = SUCC_LIGHT1.simul))
        
      }
    } ## END STRATEGY 2 & 3
  }
  
  #############################################################################
  
  cat("\n ---------- CREATION of multiple set of parameters \n")
  cat("\n  1. Get the range of parameters to be varied...\n")
  params.ranges = get_ranges()
  
  TOKEEP.simul = params.ranges$TOKEEP.simul
  TOKEEP.global = params.ranges$TOKEEP.global
  SUCC_LIGHT.simul = params.ranges$SUCC_LIGHT.simul
  params.ranges = params.ranges$PARAMS.range
  
  print(params.ranges)
  
  ## ---------------------------------------------------------------------- 
  cat("\n  2. Apply Latin Hypercube Sampling...\n")
  
  if (sum(c("max_abund_low"
            , "max_abund_medium"
            , "max_abund_high"
            , "seeding_duration"
            , "seeding_timestep"
            , "seeding_input"
            , "light_thresh_medium"
            , "light_thresh_low"
            , "soil_init"
            , "soil_retention"
            , "no_strata") %in% colnames(params.ranges)) > 0)
  {
    NO_SIMUL_LHS = no_simulations
    if ("habsuit_mode" %in% unlist(get_checked))
    {
      NO_SIMUL_LHS = trunc(NO_SIMUL_LHS / 2)
    }
    if ("dispersal_mode" %in% unlist(get_checked))
    {
      NO_SIMUL_LHS = trunc(NO_SIMUL_LHS / 3)
    }
    if (NO_SIMUL_LHS <= 1)
    {
      stop(paste0("The number of data sets requested (`no_simulations`) "
                  , "is too small compared to the number of parameters "
                  , "that must vary. \nPlease check."))
    }
    
    ## Round some parameters to avoid too much precision
    # ind = which(colnames(params.ranges) %in% c("max_abund_low"
    #                                            , "max_abund_medium"
    #                                            , "max_abund_high"
    #                                            , "light_thresh_medium"
    #                                            , "light_thresh_low"))
    # params.ranges[, ind] = round(params.ranges[, ind] / 1000)
    # ind = which(colnames(params.ranges) %in% c("seeding_duration", "seeding_input"))
    # params.ranges[, ind] = round(params.ranges[, ind] / 10)
    
    if (sum(c("max_abund_low"
              , "max_abund_medium"
              , "max_abund_high"
              , "seeding_duration"
              , "seeding_timestep"
              , "seeding_input"
              , "light_thresh_medium"
              , "light_thresh_low"
              , "soil_init"
              , "soil_retention"
              , "no_strata") %in% colnames(params.ranges)) == 1)
    {
      params.space = data.frame(sort(sample(x = seq(params.ranges[1, ]
                                                    , params.ranges[2, ]
                                                    , 1)
                                            , size = NO_SIMUL_LHS
                                            , replace = TRUE)))
      colnames(params.space) = colnames(params.ranges)
    } else
    {
      
      ## Create LHS constraint
      lhs_constraint = function(xx)
      {
        ff = function(param1, param2)
        {
          if (sum(c(param1, param2) %in% names(xx)) == 2)
          {
            return(xx[param1] <= xx[param2])
          } else { return(TRUE) }
        }
        return(ifelse(ff("max_abund_low", "max_abund_medium") &&
                        ff("max_abund_medium", "max_abund_high") &&
                        ff("seeding_timestep", "seeding_duration") &&
                        ff("light_thresh_medium", "light_thresh_low"), 0, 1))
      }
      
      ## Run Latin Hypercube Sampling
      set.seed(sample(1:1000000, 1)) ## needed everytime as lhs is also a random value generator.
      params.space = designLHD(x = NULL
                               , lower = unlist(params.ranges[1, , drop = FALSE])
                               , upper = unlist(params.ranges[2, , drop = FALSE])
                               , control = list(size = NO_SIMUL_LHS
                                                , types = c("max_abund_low" = "integer"
                                                            , "max_abund_medium" = "integer"
                                                            , "max_abund_high" = "integer"
                                                            , "seeding_duration" = "integer"
                                                            , "seeding_timestep" = "integer"
                                                            , "seeding_input" = "integer"
                                                            , "light_thresh_medium" = "integer"
                                                            , "light_thresh_low" = "integer"
                                                            , "soil_init" = "double"
                                                            , "soil_retention" = "double"
                                                            , "no_strata" = "integer")
                                                , inequalityConstraint = lhs_constraint
                               )
      )
      colnames(params.space) = colnames(params.ranges)
      params.space = as.data.frame(params.space, stringsAsFactors = FALSE)
    }
    
    ## Upscale rounded parameters to have correct ranges
    # ind = which(colnames(params.space) %in% c("max_abund_low"
    #                                           , "max_abund_medium"
    #                                           , "max_abund_high"
    #                                           , "light_thresh_medium"
    #                                           , "light_thresh_low"))
    # if (length(ind) > 0) params.space[, ind] = params.space[, ind] * 1000
    # ind = which(colnames(params.space) %in% c("seeding_duration", "seeding_input"))
    # if (length(ind) > 0) params.space[, ind] = params.space[, ind] * 10
  }
  if ("habsuit_mode" %in% unlist(get_checked))
  {
    params.space.BIS = data.frame(habsuit_mode = c(1, 2))
    if (exists("params.space"))
    {
      params.space = merge(params.space, params.space.BIS)
    } else
    {
      params.space = params.space.BIS
    }
  }
  if ("dispersal_mode" %in% unlist(get_checked))
  {
    params.space.BIS = data.frame(dispersal_mode = c(1, 2, 3))
    if (exists("params.space"))
    {
      params.space = merge(params.space, params.space.BIS)
    } else
    {
      params.space = params.space.BIS
    }
  }
  rownames(params.space) = paste0("REP-", 1:nrow(params.space))
  
  
  ## ---------------------------------------------------------------------- 
  cat("\n  3. Create new simulation folder... \n")
  
  ## CREATE NEW FOLDER
  PRE_FATE.skeletonDirectory(name.simulation = "FATE_simulation_MULTIPLE_SET")
  
  ## Copy simulation files
  cat("\n ---------- Copy files that do not change...")
  ind = grep("^--.*--$", TOKEEP.simul)
  for (fi in TOKEEP.simul[-ind])
  {
    cat("\n ", paste0(dirname(name.simulation.1), "/", fi))
    file.copy(from = paste0(dirname(name.simulation.1), "/", fi)
              , to = paste0("FATE_simulation_MULTIPLE_SET/"
                            , paste0(strsplit(fi, "/")[[1]][-1]
                                     , collapse = "/")))
  }
  cat("\n")
  
  ## SUCC - LIGHT FILES
  if ("no_strata" %in% unlist(get_checked))
  {
    if (is.null(SUCC_LIGHT.simul$SUCC) || length(SUCC_LIGHT.simul$SUCC) == 0)
    {
      stop(paste0("The flag --PFG_PARAMS_LIFE_HISTORY-- in the file "
                  , file.simulParam.1
                  , " does not contain any value. Please check."))
    }
    
    cat("\n ---------- Get PFG attribute values...")
    SUCC_table = foreach(fi = SUCC_LIGHT.simul$SUCC, .combine = "rbind") %do%
    {
      cat("\n ", fi)
      combi = data.frame(param = c("NAME", "TYPE", "HEIGHT", "MATURITY", "LONGEVITY")
                         , is.num = c(FALSE, FALSE, TRUE, TRUE, TRUE)
                         , stringsAsFactors = FALSE)
      res = foreach(i = 1:nrow(combi)) %do%
      {
        return(.getParam(params.lines = paste0(dirname(name.simulation.1), "/", fi)
                         , flag = combi$param[i]
                         , flag.split = " "
                         , is.num = combi$is.num[i]))
      }
      return(data.frame(PFG = res[[1]]
                        , type = res[[2]]
                        , height = res[[3]]
                        , maturity = res[[4]]
                        , longevity = res[[5]]
                        , stringsAsFactors = FALSE))
    }
    cat("\n")
    if ("DO_LIGHT_COMPETITION 1" %in% TOKEEP.global)
    {
      if (is.null(SUCC_LIGHT.simul$LIGHT) || length(SUCC_LIGHT.simul$LIGHT) == 0)
      {
        stop(paste0("The flag --PFG_PARAMS_LIGHT-- in the file "
                    , file.simulParam.1
                    , " does not contain any value. Please check."))
      }
      
      LIGHT_table = foreach(fi = SUCC_LIGHT.simul$LIGHT, .combine = "rbind") %do%
      {
        cat("\n ", fi)
        fi = paste0(dirname(name.simulation.1), "/", fi)
        PFG = .getParam(params.lines = fi
                        , flag = "NAME"
                        , flag.split = " "
                        , is.num = FALSE)
        light_need = .getParam(params.lines = fi
                               , flag = "LIGHT"
                               , flag.split = " "
                               , is.num = TRUE)
        return(data.frame(PFG, light_need, stringsAsFactors = FALSE))
      }
      cat("\n")
      LIGHT_table = merge(SUCC_table[, c("PFG", "type")]
                          , LIGHT_table, by = "PFG")
    }
    print(SUCC_table)
    
    cat("\n ---------- Create multiple PFG succession / light files...")
    for (i in 1:nrow(params.space))
    {
      strata.limits = sort(sample(x = c(20, 50, 150, 400, 1000, 2000, 5000, 10000)
                                  , size = params.space$no_strata[i]))
      strata.limits = c(0, strata.limits)
      cat("\n Selected strata.limits :", strata.limits)
      
      .quiet(
        PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation_MULTIPLE_SET"
                                      , mat.PFG.succ = SUCC_table
                                      , strata.limits = strata.limits
                                      , strata.limits_reduce = FALSE
                                      , opt.folder.name = paste0(rownames(params.space)[i])
        ))
      
      if ("DO_LIGHT_COMPETITION 1" %in% TOKEEP.global)
      {
        .quiet(
          PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation_MULTIPLE_SET"
                                   , mat.PFG.light = LIGHT_table
                                   , opt.folder.name = paste0(rownames(params.space)[i])
          ))
      }
    }
    cat("\n")
  }
  
  #############################################################################
  
  ## Get fixed global parameters
  tmp_global_param = "FATE_simulation_MULTIPLE_SET/tmp_global_param.txt"
  writeLines(text = TOKEEP.global, con = tmp_global_param)
  
  cat("\n ---------- Create multiple global parameter files... \n")
  for (i in 1:nrow(params.space))
  {
    doDispersal = .getParam(params.lines = tmp_global_param
                            , flag = "DO_DISPERSAL"
                            , flag.split = " "
                            , is.num = TRUE)
    doHabSuitability = .getParam(params.lines = tmp_global_param
                                 , flag = "DO_HAB_SUITABILITY"
                                 , flag.split = " "
                                 , is.num = TRUE)
    doLight = .getParam(params.lines = tmp_global_param
                        , flag = "DO_LIGHT_COMPETITION"
                        , flag.split = " "
                        , is.num = TRUE)
    doSoil = .getParam(params.lines = tmp_global_param
                       , flag = "DO_SOIL_COMPETITION"
                       , flag.split = " "
                       , is.num = TRUE)
    doDisturbances = .getParam(params.lines = tmp_global_param
                               , flag = "DO_DISTURBANCES"
                               , flag.split = " "
                               , is.num = TRUE)
    
    suppressWarnings(
      PRE_FATE.params_globalParameters(
        name.simulation = "FATE_simulation_MULTIPLE_SET"
        , opt.replacePrevious = FALSE
        , opt.no_CPU = ifelse(length(grep("NO_CPU", TOKEEP.global)) > 0
                              , .getParam(params.lines = tmp_global_param
                                          , flag = "NO_CPU"
                                          , flag.split = " "
                                          , is.num = TRUE)
                              , 1)
        , required.no_PFG = .getParam(params.lines = tmp_global_param
                                      , flag = "NO_PFG"
                                      , flag.split = " "
                                      , is.num = TRUE)
        , required.no_strata = ifelse("no_strata" %in% colnames(params.space)
                                      , params.space$no_strata[i]
                                      , .getParam(params.lines = tmp_global_param
                                                  , flag = "NO_STRATA"
                                                  , flag.split = " "
                                                  , is.num = TRUE))
        , required.simul_duration = .getParam(params.lines = tmp_global_param
                                              , flag = "SIMULATION_DURATION"
                                              , flag.split = " "
                                              , is.num = TRUE)
        , required.seeding_duration = ifelse("seeding_duration" %in% colnames(params.space)
                                             , params.space$seeding_duration[i]
                                             , .getParam(params.lines = tmp_global_param
                                                         , flag = "SEEDING_DURATION"
                                                         , flag.split = " "
                                                         , is.num = TRUE))
        , required.seeding_timestep = ifelse("seeding_timestep" %in% colnames(params.space)
                                             , params.space$seeding_timestep[i]
                                             , .getParam(params.lines = tmp_global_param
                                                         , flag = "SEEDING_TIMESTEP"
                                                         , flag.split = " "
                                                         , is.num = TRUE))
        , required.seeding_input = ifelse("seeding_input" %in% colnames(params.space)
                                          , params.space$seeding_input[i]
                                          , .getParam(params.lines = tmp_global_param
                                                      , flag = "SEEDING_INPUT"
                                                      , flag.split = " "
                                                      , is.num = TRUE))
        , required.max_abund_low = ifelse("max_abund_low" %in% colnames(params.space)
                                          , params.space$max_abund_low[i]
                                          , .getParam(params.lines = tmp_global_param
                                                      , flag = "MAX_ABUND_LOW"
                                                      , flag.split = " "
                                                      , is.num = TRUE))
        , required.max_abund_medium = ifelse("max_abund_medium" %in% colnames(params.space)
                                             , params.space$max_abund_medium[i]
                                             , .getParam(params.lines = tmp_global_param
                                                         , flag = "MAX_ABUND_MEDIUM"
                                                         , flag.split = " "
                                                         , is.num = TRUE))
        , required.max_abund_high = ifelse("max_abund_high" %in% colnames(params.space)
                                           , params.space$max_abund_high[i]
                                           , .getParam(params.lines = tmp_global_param
                                                       , flag = "MAX_ABUND_HIGH"
                                                       , flag.split = " "
                                                       , is.num = TRUE))
        , doLight = doLight
        , LIGHT.thresh_medium = ifelse(doLight &&
                                         "light_thresh_medium" %in% colnames(params.space)
                                       , params.space$light_thresh_medium[i]
                                       , .getParam(params.lines = tmp_global_param
                                                   , flag = "LIGHT_THRESH_MEDIUM"
                                                   , flag.split = " "
                                                   , is.num = TRUE))
        , LIGHT.thresh_low = ifelse(doLight &&
                                      "light_thresh_low" %in% colnames(params.space)
                                    , params.space$light_thresh_low[i]
                                    , .getParam(params.lines = tmp_global_param
                                                , flag = "LIGHT_THRESH_LOW"
                                                , flag.split = " "
                                                , is.num = TRUE))
        , doSoil = doSoil
        , SOIL.init = ifelse(doSoil && "soil_init" %in% colnames(params.space)
                             , params.space$soil_init[i]
                             , .getParam(params.lines = tmp_global_param
                                         , flag = "SOIL_INIT"
                                         , flag.split = " "
                                         , is.num = TRUE))
        , SOIL.retention = ifelse(doSoil &&
                                    "soil_retention" %in% colnames(params.space)
                                  , params.space$soil_retention[i]
                                  , .getParam(params.lines = tmp_global_param
                                              , flag = "SOIL_RETENTION"
                                              , flag.split = " "
                                              , is.num = TRUE))
        , doDispersal = doDispersal
        , DISPERSAL.mode = ifelse(doDispersal &&
                                    "dispersal_mode" %in% colnames(params.space)
                                  , params.space$dispersal_mode[i]
                                  , .getParam(params.lines = tmp_global_param
                                              , flag = "DISPERSAL_MODE"
                                              , flag.split = " "
                                              , is.num = TRUE))
        , doHabSuitability = doHabSuitability
        , HABSUIT.mode = ifelse(doHabSuitability &&
                                  "habsuit_mode" %in% colnames(params.space)
                                , params.space$habsuit_mode[i]
                                , .getParam(params.lines = tmp_global_param
                                            , flag = "HABSUIT_MODE"
                                            , flag.split = " "
                                            , is.num = TRUE))
        , doDisturbances = doDisturbances
        , DIST.no = ifelse(doDisturbances
                           , .getParam(params.lines = tmp_global_param
                                       , flag = "NO_DISTURBANCES"
                                       , flag.split = " "
                                       , is.num = TRUE)
                           , NULL)
        , DIST.no_sub = ifelse(doDisturbances
                               , .getParam(params.lines = tmp_global_param
                                           , flag = "NO_SUBDISTURBANCES"
                                           , flag.split = " "
                                           , is.num = TRUE)
                               , NULL)
        , DIST.freq = unlist(ifelse(doDisturbances
                                    , list(.getParam(params.lines = tmp_global_param
                                                     , flag = "FREQ_DISTURBANCES"
                                                     , flag.split = " "
                                                     , is.num = TRUE))
                                    , NULL))
      ))
  }
  
  #############################################################################
  
  cat("\n ---------- Create multiple simulation parameter files... \n")
  if (length(which(TOKEEP.simul == "--MASK--")) == 0)
  {
    stop(paste0("The flag --MASK-- in the file ", file.simulParam.1
                , " does not contain any value. Please check."))
  }
  for (i in 1:nrow(params.space))
  {
    suppressWarnings(
      PRE_FATE.params_simulParameters(
        name.simulation = "FATE_simulation_MULTIPLE_SET"
        , name.MASK = basename(TOKEEP.simul[which(TOKEEP.simul == "--MASK--") + 1])
        , name.DIST = ifelse(length(grep("DIST_MASK", TOKEEP.simul)) > 0
                             , basename(TOKEEP.simul[which(TOKEEP.simul == "--DIST_MASK--") + 1])
                             , "")
        , opt.global.name = paste0("Global_parameters_V", i, ".txt")
        , opt.folder.name = ifelse("no_strata" %in% colnames(params.space)
                                   , paste0(rownames(params.space)[i])
                                   , "")
      ))
  }
  
  cat("\n\n> Done!\n")
  
}
