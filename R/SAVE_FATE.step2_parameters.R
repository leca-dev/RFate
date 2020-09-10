### HEADER #####################################################################
##' @title Save data to reproduce building of parameter files
##'
##' @name SAVE_FATE.step2_parameters
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to gather all data and parameters 
##' used to build a \code{FATE} simulation folder.
##' 
##' @param name.dataset a \code{string} corresponding to the name to give to 
##' archive folder
##' @param name.simulation (\emph{optional}) default \code{NA}. \cr 
##' A \code{string} corresponding to the name of the simulation folder
##' @param strata.limits a \code{vector} of \code{integer} containing height 
##' strata limits
##' @param mat.PFG.succ a \code{data.frame} with at least 5 columns : \cr 
##' \code{PFG}, \code{type}, \code{height}, \code{maturity}, \code{longevity} 
##' \cr (\emph{and optionally, \code{max_abundance}, \code{potential_fecundity}, 
##' \code{immature_size}, \code{is_alien}, \code{flammability}}) 
##' \cr (see \code{\link{PRE_FATE.params_PFGsuccession}})
##' @param mat.PFG.light (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 2 to 6 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{type}, (\emph{or \code{active_germ_low}, 
##'   \code{active_germ_medium}, \code{active_germ_high}}) (\emph{or
##'   \code{strategy_ag}})
##'   \item \emph{\code{type}, \code{light_need}}
##' }
##' (see \code{\link{PRE_FATE.params_PFGlight}})
##' @param mat.PFG.light.tol (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 2 to 4 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{lifeStage}, \code{resources}, \code{tolerance} 
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGlight}})
##' @param mat.PFG.soil (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{type}, (\emph{or \code{active_germ_low}, 
##'   \code{active_germ_medium}, \code{active_germ_high}}) (\emph{or
##'   \code{strategy_ag}})
##'   \item \code{soil_contrib}, \code{soil_tol_min}, \code{soil_tol_max} 
##'   (\emph{or \code{strategy_contrib}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGsoil}})
##' @param mat.PFG.soil.tol (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 2 to 4 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{lifeStage}, \code{resources}, \code{tolerance} 
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGsoil}})
##' @param mat.PFG.disp (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 4 columns : \code{PFG}, \code{d50}, \code{d99}, 
##' \code{ldd} (see \code{\link{PRE_FATE.params_PFGdispersal}})
##' @param mat.PFG.dist (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 5 columns : \cr 
##' \code{PFG}, \code{type}, \code{maturity}, \code{longevity}, 
##' \code{age_above_150cm} (see \code{\link{PRE_FATE.params_PFGdisturbance}})
##' @param mat.PFG.dist.tol (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{nameDist},
##'   \item \code{PFG},
##'   \item (\emph{\code{responseStage}, \code{breakAge}, \code{resproutAge}}), 
##'   \item \code{responseStage}, \code{killedIndiv}, \code{resproutIndiv}  
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGdisturbance}})
##' @param mat.PFG.drought (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 4 or 6 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{threshold_moderate}, \code{threshold_severe},
##'   \item \code{counter_recovery}, \code{counter_sens}, \code{counter_cum}
##'   (\emph{or \code{strategy_drou}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGdrought}})
##' @param mat.PFG.drought.tol (\emph{optional}) default \code{NA}. \cr 
##' A \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{nameDist},
##'   \item \code{PFG},
##'   \item (\emph{\code{responseStage}, \code{breakAge}, \code{resproutAge}}), 
##'   \item \code{responseStage}, \code{killedIndiv}, \code{resproutIndiv}  
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \code{\link{PRE_FATE.params_PFGdrought}})
##' @param rasters a \code{list} containing all the rasters given to the 
##' \code{\link{PRE_FATE.params_simulParameters}} function, if used 
##' (\code{name.MASK}, \code{name.DIST}, \code{name.DROUGHT}, \code{name.FIRE}, 
##' \code{name.ELEVATION}, \code{name.SLOPE})
##' @param multipleSet a \code{list} containing all the parameter values given 
##' to the \code{\link{PRE_FATE.params_multipleSet}} function, if used 
##' (\code{name.simulation.1}, \code{name.simulation.2}, 
##' \code{file.simulParam.1}, \code{file.simulParam.2}, 
##' \code{no_simulations}, \code{opt.percent_maxAbund}, 
##' \code{opt.percent_seeding}, \code{opt.percent_light}, 
##' \code{opt.percent_soil}, \code{do.max_abund_low}, 
##' \code{do.max_abund_medium}, \code{do.max_abund_high}, 
##' \code{do.seeding_duration}, \code{do.seeding_timestep}, 
##' \code{do.seeding_input}, \code{do.no_strata}, 
##' \code{do.LIGHT.thresh_medium}, \code{do.LIGHT.thresh_low}, 
##' \code{do.SOIL.init}, \code{do.SOIL.retention}, 
##' \code{do.DISPERSAL.mode}, \code{do.HABSUIT.mode})
##' 
##' 
##' 
##' @return A \code{list} containing all the elements given to the function and 
##' checked, and two archive files :
##' 
##' \describe{
##'   \item{name.dataset}{name of the dataset}
##'   \item{strata.limits}{height strata limits}
##'   \item{mat.PFG.succ}{}
##'   \item{(mat.PFG.light)}{}
##'   \item{(mat.PFG.light.tol)}{}
##'   \item{(mat.PFG.soil)}{}
##'   \item{(mat.PFG.soil.tol)}{}
##'   \item{(mat.PFG.disp)}{}
##'   \item{(mat.PFG.dist)}{}
##'   \item{(mat.PFG.dist.tol)}{}
##'   \item{(mat.PFG.drought)}{}
##'   \item{(mat.PFG.drought.tol)}{}
##'   \item{rasters}{raster files of all simulation masks}
##'   \item{(multipleSet)}{ \cr \cr}
##'   \item{(name.simulation)}{name of the simulation folder}
##'   \item{(\code{DATA} folder)}{contained in \code{name.simulation} folder 
##'   and archived}
##'   \item{(\code{PARAM_SIMUL} folder)}{contained in \code{name.simulation} 
##'   folder and archived \cr \cr}
##' }
##' 
##' The information is written in \file{FATE_dataset_[name.dataset]_step2_parameters.RData} file.
##' 
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_PFGsuccession}}, 
##' \code{\link{PRE_FATE.params_PFGlight}}, 
##' \code{\link{PRE_FATE.params_PFGsoil}}, 
##' \code{\link{PRE_FATE.params_PFGdispersal}}, 
##' \code{\link{PRE_FATE.params_PFGdisturbance}}, 
##' \code{\link{PRE_FATE.params_PFGdrought}}, 
##' \code{\link{PRE_FATE.params_changingYears}}, 
##' \code{\link{PRE_FATE.params_savingYears}}, 
##' \code{\link{PRE_FATE.params_globalParameters}}, 
##' \code{\link{PRE_FATE.params_simulParameters}}, 
##' \code{\link{PRE_FATE.params_multipleSet}}
##' 
##' @examples
##' 
##' ## Load example data
##' 
##' @export
##' 
##' @importFrom utils zip
##' 
## END OF HEADER ###############################################################


SAVE_FATE.step2_parameters = function(name.dataset
                                      , name.simulation = NA
                                      , strata.limits
                                      , mat.PFG.succ
                                      , mat.PFG.light = NA
                                      , mat.PFG.light.tol = NA
                                      , mat.PFG.soil = NA
                                      , mat.PFG.soil.tol = NA
                                      , mat.PFG.disp = NA
                                      , mat.PFG.dist = NA
                                      , mat.PFG.dist.tol = NA
                                      , mat.PFG.drought = NA
                                      , mat.PFG.drought.tol = NA
                                      , rasters = list("name.MASK" = NA
                                                       , "name.DIST" = NA
                                                       , "name.DROUGHT" = NA
                                                       , "name.FIRE" = NA
                                                       , "name.ELEVATION" = NA
                                                       , "name.SLOPE" = NA)
                                      , multipleSet = list("name.simulation.1" = NA
                                                           , "name.simulation.2" = NA
                                                           , "file.simulParam.1" = NA
                                                           , "file.simulParam.2" = NA
                                                           , "no_simulations" = NA
                                                           , "opt.percent_maxAbund" = NA
                                                           , "opt.percent_seeding" = NA
                                                           , "opt.percent_light" = NA
                                                           , "opt.percent_soil" = NA
                                                           , "do.max_abund_low" = NA
                                                           , "do.max_abund_medium" = NA
                                                           , "do.max_abund_high" = NA
                                                           , "do.seeding_duration" = NA
                                                           , "do.seeding_timestep" = NA
                                                           , "do.seeding_input" = NA
                                                           , "do.no_strata" = NA
                                                           , "do.LIGHT.thresh_medium" = NA
                                                           , "do.LIGHT.thresh_low" = NA
                                                           , "do.SOIL.init" = NA
                                                           , "do.SOIL.retention" = NA
                                                           , "do.DISPERSAL.mode" = NA
                                                           , "do.HABSUIT.mode" = NA)
){
  
  #############################################################################
  
  .testParam_notChar.m("name.dataset", name.dataset)
  
  if (!is.na(name.simulation))
  {
    ## CHECK parameter name.simulation
    .testParam_existFolder(name.simulation, "DATA/")
    .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
    ## ARCHIVE folders
    name.arch_data = paste0(paste0(unique(c(name.dataset, name.simulation)), collapse = "_"), "_DATA.zip")
    name.arch_paramsimul = paste0(name.simulation, "_PARAM_SIMUL.zip")
    zip(zipfile  = name.arch_data, files = paste0(name.simulation, "DATA/"), flags = "-r")
    zip(zipfile  = name.arch_paramsimul, files = paste0(name.simulation, "PARAM_SIMUL/"), flags = "-r")
  }
  
  ## CHECK parameter strata.limits
  strata.limits = sort(unique(na.exclude(strata.limits)))
  .testParam_notInteger.m("strata.limits", strata.limits)
  ## CHECK parameter mat.PFG.succ
  if (.testParam_notDf(mat.PFG.succ))
  {
    .stopMessage_beDataframe("mat.PFG.succ")
  } else
  {
    if (nrow(mat.PFG.succ) == 0 || !(ncol(mat.PFG.succ) %in% c(5, 6, 7, 8, 9, 10)))
    {
      .stopMessage_numRowCol("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"
                                               , "(max_abundance)", "(potential_fecundity)"
                                               , "(immature_size)", "(is_alien)", "(flammability)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.succ))
                          , "5" = .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"))
                          , "6" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "flammability")))
                          , "7" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance"
                                                                          , "potential_fecundity")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size"
                                                                            , "flammability")))
                          , "8" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance"
                                                                          , "potential_fecundity", "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "potential_fecundity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "potential_fecundity", "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size", "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "potential_fecundity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "potential_fecundity", "flammability")))
                          , "9" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                          , "max_abundance", "potential_fecundity"
                                                                          , "immature_size", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                            , "max_abundance", "potential_fecundity"
                                                                            , "immature_size", "flammability")))
                          , "10" = .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                          , "max_abundance", "potential_fecundity"
                                                                          , "immature_size", "is_alien", "flammability"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"
                                                   , "(max_abundance)", "(potential_fecundity)"
                                                   , "(immature_size)", "(is_alien)", "(flammability)"))
      }
    }
    mat.PFG.succ$PFG = as.character(mat.PFG.succ$PFG)
    .testParam_samevalues.m("mat.PFG.succ$PFG", mat.PFG.succ$PFG)
    .testParam_notChar.m("mat.PFG.succ$PFG", mat.PFG.succ$PFG)
    mat.PFG.succ$type = as.character(mat.PFG.succ$type)
    .testParam_notInValues.m("mat.PFG.succ$type", mat.PFG.succ$type, c("H", "C", "P"))
    .testParam_notNum.m("mat.PFG.succ$height", mat.PFG.succ$height)
    .testParam_NAvalues.m("mat.PFG.succ$height", mat.PFG.succ$height)
    .testParam_notNum.m("mat.PFG.succ$maturity", mat.PFG.succ$maturity)
    .testParam_NAvalues.m("mat.PFG.succ$maturity", mat.PFG.succ$maturity)
    .testParam_notNum.m("mat.PFG.succ$longevity", mat.PFG.succ$longevity)
    .testParam_NAvalues.m("mat.PFG.succ$longevity", mat.PFG.succ$longevity)
    if (sum(mat.PFG.succ$maturity > mat.PFG.succ$longevity) > 0){
      stop(paste0("Wrong type of data!\n `mat.PFG.succ$maturity` must contain "
                  , "values equal or inferior to `mat.PFG.succ$longevity`"))
    }
    
    if (sum(colnames(mat.PFG.succ) == "max_abundance") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$max_abundance", mat.PFG.succ$max_abundance)
      .testParam_notInValues.m("mat.PFG.succ$max_abundance", mat.PFG.succ$max_abundance, 1:3)
    }
    if (sum(colnames(mat.PFG.succ) == "potential_fecundity") == 1)
    {
      .testParam_notNum.m("mat.PFG.succ$potential_fecundity", mat.PFG.succ$potential_fecundity)
      .testParam_NAvalues.m("mat.PFG.succ$potential_fecundity", mat.PFG.succ$potential_fecundity)
    }
    if (sum(colnames(mat.PFG.succ) == "immature_size") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$immature_size", mat.PFG.succ$immature_size)
      .testParam_notInValues.m("mat.PFG.succ$immature_size", mat.PFG.succ$immature_size, 0:10)
    }
    if (sum(colnames(mat.PFG.succ) == "is_alien") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$is_alien", mat.PFG.succ$is_alien)
      .testParam_notInValues.m("mat.PFG.succ$is_alien", mat.PFG.succ$is_alien, 0:1)
    }
    if (sum(colnames(mat.PFG.succ) == "flammability") == 1)
    {
      .testParam_notNum.m("mat.PFG.succ$flammability", mat.PFG.succ$flammability)
      .testParam_NAvalues.m("mat.PFG.succ$flammability", mat.PFG.succ$flammability)
    }
  }
  ## CHECK parameter mat.PFG.light
  if (.testParam_notDf(mat.PFG.light))
  {
    .stopMessage_beDataframe("mat.PFG.light")
  } else
  {
    if (nrow(mat.PFG.light) == 0 || !(ncol(mat.PFG.light) %in% c(2, 3, 4, 6)))
    {
      .stopMessage_numRowCol("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.light))
                          , "2" = (.testParam_notColnames(mat.PFG.light, c("PFG", "type")) &&
                                     .testParam_notColnames(mat.PFG.light, c("PFG", "strategy_ag")))
                          , "3" = .testParam_notColnames(mat.PFG.light, c("PFG", "type", "light_need"))
                          , "4" = (.testParam_notColnames(mat.PFG.light, c("PFG", "active_germ_low"
                                                                           , "active_germ_medium"
                                                                           , "active_germ_high")) &&
                                     .testParam_notColnames(mat.PFG.light, c("PFG", "strategy_ag"
                                                                             , "type", "light_need")))
                          , "6" = .testParam_notColnames(mat.PFG.light, c("PFG", "active_germ_low"
                                                                          , "active_germ_medium"
                                                                          , "active_germ_high"
                                                                          , "type", "light_need"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.light", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                    , "(active_germ_high)", "(strategy_ag)", "(light_need)"))
      }
    }
    mat.PFG.light$PFG = as.character(mat.PFG.light$PFG)
    .testParam_samevalues.m("mat.PFG.light$PFG", mat.PFG.light$PFG)
    .testParam_notChar.m("mat.PFG.light$PFG", mat.PFG.light$PFG)
    if (sum(colnames(mat.PFG.light) == "type") == 1)
    {
      mat.PFG.light$type = as.character(mat.PFG.light$type)
      .testParam_notInValues.m("mat.PFG.light$type", mat.PFG.light$type, c("H", "C", "P"))
    }
    if (sum(colnames(mat.PFG.light) == "light_need") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.light$light_need", mat.PFG.light$light_need)
      .testParam_notInValues.m("mat.PFG.light$light_need", mat.PFG.light$light_need, 0:5)
    }
    if (sum(colnames(mat.PFG.light) == "active_germ_low") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.light$active_germ_low", mat.PFG.light$active_germ_low)
      .testParam_notInValues.m("mat.PFG.light$active_germ_low", mat.PFG.light$active_germ_low, 0:10)
      .testParam_NAvalues.m("mat.PFG.light$active_germ_medium", mat.PFG.light$active_germ_medium)
      .testParam_notInValues.m("mat.PFG.light$active_germ_medium", mat.PFG.light$active_germ_medium, 0:10)
      .testParam_NAvalues.m("mat.PFG.light$active_germ_high", mat.PFG.light$active_germ_high)
      .testParam_notInValues.m("mat.PFG.light$active_germ_high", mat.PFG.light$active_germ_high, 0:10)
    }
    if (sum(colnames(mat.PFG.light) == "strategy_ag") == 1)
    {
      mat.PFG.light$strategy_ag = as.character(mat.PFG.light$strategy_ag)
      .testParam_notInValues.m("mat.PFG.light$strategy_ag", mat.PFG.light$strategy_ag
                               , c("light_lover", "indifferent", "shade_lover"))
    }
  }
  ## CHECK parameter mat.PFG.light.tol
  if (!is.null(mat.PFG.light.tol))
  {
    if (.testParam_notDf(mat.PFG.light.tol))
    {
      .stopMessage_beDataframe("mat.PFG.light.tol")
    } else
    {
      if (nrow(mat.PFG.light.tol) == 0 || !(ncol(mat.PFG.light.tol) %in% c(2, 4)))
      {
        .stopMessage_numRowCol("mat.PFG.light.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
      } else
      {
        notCorrect = switch(as.character(ncol(mat.PFG.light.tol))
                            , "2" = .testParam_notColnames(mat.PFG.light.tol, c("PFG", "strategy_tol"))
                            , "4" = .testParam_notColnames(mat.PFG.light.tol, c("PFG", "lifeStage", "resources", "tolerance"))
                            , TRUE)
        if (notCorrect){
          .stopMessage_columnNames("mat.PFG.light.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
        }
      }
      mat.PFG.light.tol$PFG = as.character(mat.PFG.light.tol$PFG)
      .testParam_notChar.m("mat.PFG.light.tol$PFG", mat.PFG.light.tol$PFG)
      if (sum(colnames(mat.PFG.light.tol) == "lifeStage") == 1)
      {
        .testParam_notInValues.m("mat.PFG.light.tol$lifeStage", mat.PFG.light.tol$lifeStage, c("Germinant", "Immature", "Mature"))
        .testParam_notInValues.m("mat.PFG.light.tol$resources", mat.PFG.light.tol$resources, c("Low", "Medium", "High"))
        .testParam_NAvalues.m("mat.PFG.light.tol$tolerance", mat.PFG.light.tol$tolerance)
        .testParam_notInValues.m("mat.PFG.light.tol$tolerance", mat.PFG.light.tol$tolerance, 0:10)
      }
      if (sum(colnames(mat.PFG.light.tol) == "strategy_tol") == 1)
      {
        mat.PFG.light.tol$strategy_tol = as.character(mat.PFG.light.tol$strategy_tol)
        .testParam_notInValues.m("mat.PFG.light.tol$strategy_tol", mat.PFG.light.tol$strategy_tol
                                 , c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
      }
    }
  }
  ## CHECK parameter mat.PFG.soil
  if (.testParam_notDf(mat.PFG.soil))
  {
    .stopMessage_beDataframe("mat.PFG.soil")
  } else
  {
    if (nrow(mat.PFG.soil) == 0 || !(ncol(mat.PFG.soil) %in% c(3, 5, 7)))
    {
      .stopMessage_numRowCol("mat.PFG.soil", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                               , "(active_germ_high)", "(strategy_ag)", "soil_contrib"
                                               , "soil_tol_min", "soil_tol_max", "(strategy_contrib)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.soil))
                          , "3" = (.testParam_notColnames(mat.PFG.soil, c("PFG", "type", "strategy_contrib")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "strategy_ag", "strategy_contrib")))
                          , "5" = (.testParam_notColnames(mat.PFG.soil, c("PFG", "type", "soil_contrib"
                                                                          , "soil_tol_min", "soil_tol_max")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "active_germ_low"
                                                                            , "active_germ_medium"
                                                                            , "active_germ_high"
                                                                            , "strategy_contrib")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "strategy_ag", "soil_contrib"
                                                                            , "soil_tol_min", "soil_tol_max")))
                          , "7" = .testParam_notColnames(mat.PFG.soil, c("PFG", "active_germ_low"
                                                                         , "active_germ_medium"
                                                                         , "active_germ_high", "soil_contrib"
                                                                         , "soil_tol_min", "soil_tol_max"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.soil", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                   , "(active_germ_high)", "(strategy_ag)", "soil_contrib"
                                                   , "soil_tol_min", "soil_tol_max", "(strategy_contrib)"))
      }
    }
    mat.PFG.soil$PFG = as.character(mat.PFG.soil$PFG)
    .testParam_samevalues.m("mat.PFG.soil$PFG", mat.PFG.soil$PFG)
    .testParam_notChar.m("mat.PFG.soil$PFG", mat.PFG.soil$PFG)
    if (sum(colnames(mat.PFG.soil) == "type") == 1)
    {
      mat.PFG.soil$type = as.character(mat.PFG.soil$type)
      .testParam_notInValues.m("mat.PFG.soil$type", mat.PFG.soil$type, c("H", "C", "P"))
    }
    if (sum(colnames(mat.PFG.soil) == "active_germ_low") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low, 0:10)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium, 0:10)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high, 0:10)
    }
    if (sum(colnames(mat.PFG.soil) == "strategy_ag") == 1)
    {
      mat.PFG.soil$strategy_ag = as.character(mat.PFG.soil$strategy_ag)
      .testParam_notInValues.m("mat.PFG.soil$strategy_ag", mat.PFG.soil$strategy_ag
                               , c("poor_lover", "indifferent", "rich_lover"))
    }
    if (sum(colnames(mat.PFG.soil) == "soil_contrib") == 1)
    {
      .testParam_notNum.m("mat.PFG.soil$soil_contrib", mat.PFG.soil$soil_contrib)
      .testParam_NAvalues.m("mat.PFG.soil$soil_contrib", mat.PFG.soil$soil_contrib)
      .testParam_notNum.m("mat.PFG.soil$soil_tol_min", mat.PFG.soil$soil_tol_min)
      .testParam_NAvalues.m("mat.PFG.soil$soil_tol_min", mat.PFG.soil$soil_tol_min)
      .testParam_notNum.m("mat.PFG.soil$soil_tol_max", mat.PFG.soil$soil_tol_max)
      .testParam_NAvalues.m("mat.PFG.soil$soil_tol_max", mat.PFG.soil$soil_tol_max)
      if (sum(mat.PFG.soil$soil_tol_min > mat.PFG.soil$soil_contrib) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.soil$soil_tol_min` must contain "
                    , "values equal or inferior to `mat.PFG.soil$soil_contrib`"))
      }
      if (sum(mat.PFG.soil$soil_tol_max < mat.PFG.soil$soil_contrib) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.soil$soil_tol_max` must contain "
                    , "values equal or superior to `mat.PFG.soil$soil_contrib`"))
      }
    }
    if (sum(colnames(mat.PFG.soil) == "strategy_contrib") == 1)
    {
      mat.PFG.soil$strategy_contrib = as.character(mat.PFG.soil$strategy_contrib)
      .testParam_notInValues.m("mat.PFG.soil$strategy_contrib", mat.PFG.soil$strategy_contrib
                               , c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
    }
  }
  ## CHECK parameter mat.PFG.soil.tol
  if (!is.null(mat.PFG.soil.tol))
  {
    if (.testParam_notDf(mat.PFG.soil.tol))
    {
      .stopMessage_beDataframe("mat.PFG.soil.tol")
    } else
    {
      if (nrow(mat.PFG.soil.tol) == 0 || !(ncol(mat.PFG.soil.tol) %in% c(2, 4)))
      {
        .stopMessage_numRowCol("mat.PFG.soil.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
      } else
      {
        notCorrect = switch(as.character(ncol(mat.PFG.soil.tol))
                            , "2" = .testParam_notColnames(mat.PFG.soil.tol, c("PFG", "strategy_tol"))
                            , "4" = .testParam_notColnames(mat.PFG.soil.tol, c("PFG", "lifeStage", "resources", "tolerance"))
                            , TRUE)
        if (notCorrect){
          .stopMessage_columnNames("mat.PFG.soil.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
        }
      }
      mat.PFG.soil.tol$PFG = as.character(mat.PFG.soil.tol$PFG)
      .testParam_notChar.m("mat.PFG.soil.tol$PFG", mat.PFG.soil.tol$PFG)
      if (sum(colnames(mat.PFG.soil.tol) == "lifeStage") == 1)
      {
        .testParam_notInValues.m("mat.PFG.soil.tol$lifeStage", mat.PFG.soil.tol$lifeStage, c("Germinant", "Immature", "Mature"))
        .testParam_notInValues.m("mat.PFG.soil.tol$resources", mat.PFG.soil.tol$resources, c("Low", "Medium", "High"))
        .testParam_NAvalues.m("mat.PFG.soil.tol$tolerance", mat.PFG.soil.tol$tolerance)
        .testParam_notInValues.m("mat.PFG.soil.tol$tolerance", mat.PFG.soil.tol$tolerance, 0:10)
      }
      if (sum(colnames(mat.PFG.soil.tol) == "strategy_tol") == 1)
      {
        mat.PFG.soil.tol$strategy_tol = as.character(mat.PFG.soil.tol$strategy_tol)
        .testParam_notInValues.m("mat.PFG.soil.tol$strategy_tol", mat.PFG.soil.tol$strategy_tol
                                 , c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
      }
    }
  }
  ## CHECK parameter mat.PFG.disp
  if (.testParam_notDf(mat.PFG.disp))
  {
    .stopMessage_beDataframe("mat.PFG.disp")
  } else
  {
    if (nrow(mat.PFG.disp) == 0 || ncol(mat.PFG.disp) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
    } else if (.testParam_notColnames(mat.PFG.disp, c("PFG", "d50", "d99", "ldd")))
    {
      .stopMessage_columnNames("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
    }
    mat.PFG.disp$PFG = as.character(mat.PFG.disp$PFG)
    .testParam_samevalues.m("mat.PFG.disp$PFG", mat.PFG.disp$PFG)
    .testParam_notChar.m("mat.PFG.disp$PFG", mat.PFG.disp$PFG)
    .testParam_notNum.m("mat.PFG.disp$d50", mat.PFG.disp$d50)
    .testParam_NAvalues.m("mat.PFG.disp$d50", mat.PFG.disp$d50)
    .testParam_notNum.m("mat.PFG.disp$d99", mat.PFG.disp$d99)
    .testParam_NAvalues.m("mat.PFG.disp$d99", mat.PFG.disp$d99)
    .testParam_notNum.m("mat.PFG.disp$ldd", mat.PFG.disp$ldd)
    .testParam_NAvalues.m("mat.PFG.disp$ldd", mat.PFG.disp$ldd)
  }
  ## CHECK parameter mat.PFG.dist
  if (!is.null(mat.PFG.dist))
  {
    if (.testParam_notDf(mat.PFG.dist))
    {
      .stopMessage_beDataframe("mat.PFG.dist")
    } else
    {
      if (nrow(mat.PFG.dist) == 0 || ncol(mat.PFG.dist) != 5)
      {
        .stopMessage_numRowCol("mat.PFG.dist", c("PFG", "type", "maturity", "longevity", "age_above_150cm"))
      } else if (.testParam_notColnames(mat.PFG.dist, c("PFG", "type", "maturity", "longevity", "age_above_150cm")))
      {
        .stopMessage_columnNames("mat.PFG.dist", c("PFG", "type", "maturity", "longevity", "age_above_150cm"))
      }
      mat.PFG.dist$PFG = as.character(mat.PFG.dist$PFG)
      .testParam_samevalues.m("mat.PFG.dist$PFG", mat.PFG.dist$PFG)
      .testParam_notChar.m("mat.PFG.dist$PFG", mat.PFG.dist$PFG)
      mat.PFG.dist$type = as.character(mat.PFG.dist$type)
      .testParam_notInValues.m("mat.PFG.dist$type", mat.PFG.dist$type, c("H", "C", "P"))
      .testParam_notNum.m("mat.PFG.dist$maturity", mat.PFG.dist$maturity)
      .testParam_NAvalues.m("mat.PFG.dist$maturity", mat.PFG.dist$maturity)
      .testParam_notNum.m("mat.PFG.dist$longevity", mat.PFG.dist$longevity)
      .testParam_NAvalues.m("mat.PFG.dist$longevity", mat.PFG.dist$longevity)
      .testParam_notNum.m("mat.PFG.dist$age_above_150cm", mat.PFG.dist$age_above_150cm)
      .testParam_NAvalues.m("mat.PFG.dist$age_above_150cm", mat.PFG.dist$age_above_150cm)
      if (sum(mat.PFG.dist$maturity > mat.PFG.dist$longevity) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.dist$maturity` must contain "
                    , "values equal or inferior to `mat.PFG.dist$longevity`"))
      }
      mat.PFG.dist$longevity = mat.PFG.dist$longevity - 1
    }
  }
  ## CHECK parameter mat.PFG.dist.tol
  if (.testParam_notDf(mat.PFG.dist.tol))
  {
    .stopMessage_beDataframe("mat.PFG.dist.tol")
  } else
  {
    if (nrow(mat.PFG.dist.tol) == 0 || !(ncol(mat.PFG.dist.tol) %in% c(3, 5, 6, 7)))
    {
      .stopMessage_numRowCol("mat.PFG.dist.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                                   , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.dist.tol))
                          , "3" = .testParam_notColnames(mat.PFG.dist.tol, c("nameDist", "PFG", "strategy_tol"))
                          , "5" = .testParam_notColnames(mat.PFG.dist.tol, c("nameDist", "PFG", "responseStage"
                                                                             , "killedIndiv", "resproutIndiv"))
                          , "6" = .testParam_notColnames(mat.PFG.dist.tol, c("nameDist", "PFG", "responseStage"
                                                                             , "breakAge", "resproutAge"
                                                                             , "strategy_tol"))
                          , "7" = .testParam_notColnames(mat.PFG.dist.tol, c("nameDist", "PFG", "responseStage"
                                                                             , "breakAge", "resproutAge"
                                                                             , "killedIndiv", "resproutIndiv"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.dist.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                                       , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
      }
    }
    mat.PFG.dist.tol$nameDist = as.character(mat.PFG.dist.tol$nameDist)
    .testParam_notChar.m("mat.PFG.dist.tol$nameDist", mat.PFG.dist.tol$nameDist)
    mat.PFG.dist.tol$PFG = as.character(mat.PFG.dist.tol$PFG)
    .testParam_notChar.m("mat.PFG.dist.tol$PFG", mat.PFG.dist.tol$PFG)
    if (!is.null(mat.PFG.dist))
    {
      .testParam_notInValues.m("mat.PFG.dist.tol$PFG", mat.PFG.dist.tol$PFG, c("H", "C", "P", mat.PFG.dist$PFG))
    }
    if (sum(colnames(mat.PFG.dist.tol) == "responseStage") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.dist.tol$responseStage", mat.PFG.dist.tol$responseStage)
      .testParam_notInValues.m("mat.PFG.dist.tol$responseStage", mat.PFG.dist.tol$responseStage, 0:10)
      if (sum(colnames(mat.PFG.dist.tol) == "breakAge") == 1)
      {
        .testParam_notNum.m("mat.PFG.dist.tol$breakAge", mat.PFG.dist.tol$breakAge)
        .testParam_NAvalues.m("mat.PFG.dist.tol$breakAge", mat.PFG.dist.tol$breakAge)
        .testParam_notNum.m("mat.PFG.dist.tol$resproutAge", mat.PFG.dist.tol$resproutAge)
        .testParam_NAvalues.m("mat.PFG.dist.tol$resproutAge", mat.PFG.dist.tol$resproutAge)
      }
      if (sum(colnames(mat.PFG.dist.tol) == "killedIndiv") == 1)
      {
        .testParam_NAvalues.m("mat.PFG.dist.tol$killedIndiv", mat.PFG.dist.tol$killedIndiv)
        .testParam_notInValues.m("mat.PFG.dist.tol$killedIndiv", mat.PFG.dist.tol$killedIndiv, 0:10)
        .testParam_NAvalues.m("mat.PFG.dist.tol$resproutIndiv", mat.PFG.dist.tol$resproutIndiv)
        .testParam_notInValues.m("mat.PFG.dist.tol$resproutIndiv", mat.PFG.dist.tol$resproutIndiv, 0:10)
      }
    }
    if (sum(colnames(mat.PFG.dist.tol) == "strategy_tol") == 1)
    {
      mat.PFG.dist.tol$strategy_tol = as.character(mat.PFG.dist.tol$strategy_tol)
      .testParam_notInValues.m("mat.PFG.dist.tol$strategy_tol", mat.PFG.dist.tol$strategy_tol
                               , c("indifferent", "mowing_herbs", "mowing_trees"
                                   , "grazing_herbs_1", "grazing_herbs_2", "grazing_herbs_3"
                                   , "grazing_trees_1", "grazing_trees_2", "grazing_trees_3"))
    }
  }
  ## CHECK parameter mat.PFG.drought
  if (.testParam_notDf(mat.PFG.drought))
  {
    .stopMessage_beDataframe("mat.PFG.drought")
  } else
  {
    if (nrow(mat.PFG.drought) == 0 || !(ncol(mat.PFG.drought) %in% c(4, 6)))
    {
      .stopMessage_numRowCol("mat.PFG.drought", c("PFG", "threshold_moderate"
                                                  , "threshold_severe", "counter_recovery"
                                                  , "counter_sens", "counter_cum"
                                                  , "(strategy_drou)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.drought))
                          , "4" = .testParam_notColnames(mat.PFG.drought
                                                         , c("PFG", "threshold_moderate"
                                                             , "threshold_severe","strategy_drou"))
                          , "6" = .testParam_notColnames(mat.PFG.drought
                                                         , c("PFG", "threshold_moderate"
                                                             , "threshold_severe", "counter_recovery"
                                                             , "counter_sens", "counter_cum"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.drought", c("PFG", "threshold_moderate"
                                                      , "threshold_severe", "counter_recovery"
                                                      , "counter_sens", "counter_cum"
                                                      , "(strategy_drou)"))
      }
    }
    mat.PFG.drought$PFG = as.character(mat.PFG.drought$PFG)
    .testParam_notChar.m("mat.PFG.drought$PFG", mat.PFG.drought$PFG)
    .testParam_notNum.m("mat.PFG.drought$threshold_moderate", mat.PFG.drought$threshold_moderate)
    .testParam_NAvalues.m("mat.PFG.drought$threshold_moderate", mat.PFG.drought$threshold_moderate)
    .testParam_notNum.m("mat.PFG.drought$threshold_severe", mat.PFG.drought$threshold_severe)
    .testParam_NAvalues.m("mat.PFG.drought$threshold_severe", mat.PFG.drought$threshold_severe)
    if (sum(mat.PFG.drought$threshold_severe > mat.PFG.drought$threshold_moderate) > 0){
      stop(paste0("Wrong type of data!\n `mat.PFG.drought$threshold_severe` must contain "
                  , "values equal or inferior to `mat.PFG.drought$threshold_moderate`"))
    }
    if (ncol(mat.PFG.drought) == 6)
    {
      .testParam_NAvalues.m("mat.PFG.drought$counter_recovery", mat.PFG.drought$counter_recovery)
      .testParam_notInteger.m("mat.PFG.drought$counter_recovery", mat.PFG.drought$counter_recovery)
      .testParam_NAvalues.m("mat.PFG.drought$counter_sens", mat.PFG.drought$counter_sens)
      .testParam_notInteger.m("mat.PFG.drought$counter_sens", mat.PFG.drought$counter_sens)
      .testParam_NAvalues.m("mat.PFG.drought$counter_cum", mat.PFG.drought$counter_cum)
      .testParam_notInteger.m("mat.PFG.drought$counter_cum", mat.PFG.drought$counter_cum)
      if (sum(mat.PFG.drought$counter_sens > mat.PFG.drought$counter_cum) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.drought$counter_sens` must contain "
                    , "values equal or inferior to `mat.PFG.drought$counter_cum`"))
      }
    }
    if (sum(colnames(mat.PFG.drought) == "strategy_drou") == 1)
    {
      mat.PFG.drought$strategy_drou = as.character(mat.PFG.drought$strategy_drou)
      .testParam_notInValues.m("mat.PFG.drought$strategy_drou", mat.PFG.drought$strategy_drou
                               , c("herbs", "chamaephytes", "trees_shrubs"))
    }
  }
  ## CHECK parameter mat.PFG.drought.tol
  if (.testParam_notDf(mat.PFG.drought.tol))
  {
    .stopMessage_beDataframe("mat.PFG.drought.tol")
  } else
  {
    if (nrow(mat.PFG.drought.tol) == 0 || !(ncol(mat.PFG.drought.tol) %in% c(3, 5, 6, 7)))
    {
      .stopMessage_numRowCol("mat.PFG.drought.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                                      , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.drought.tol))
                          , "3" = .testParam_notColnames(mat.PFG.drought.tol, c("nameDist", "PFG", "strategy_tol"))
                          , "5" = .testParam_notColnames(mat.PFG.drought.tol, c("nameDist", "PFG", "responseStage"
                                                                                , "killedIndiv", "resproutIndiv"))
                          , "6" = .testParam_notColnames(mat.PFG.drought.tol, c("nameDist", "PFG", "responseStage"
                                                                                , "breakAge", "resproutAge"
                                                                                , "strategy_tol"))
                          , "7" = .testParam_notColnames(mat.PFG.drought.tol, c("nameDist", "PFG", "responseStage"
                                                                                , "breakAge", "resproutAge"
                                                                                , "killedIndiv", "resproutIndiv"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.drought.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                                          , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
      }
    }
    mat.PFG.drought.tol$nameDist = as.character(mat.PFG.drought.tol$nameDist)
    .testParam_notInValues.m("mat.PFG.drought.tol$nameDist", mat.PFG.drought.tol$nameDist, c("immediate", "delayed"))
    mat.PFG.drought.tol$PFG = as.character(mat.PFG.drought.tol$PFG)
    .testParam_notChar.m("mat.PFG.drought.tol$PFG", mat.PFG.drought.tol$PFG)
    if (!is.null(mat.PFG.dist))
    {
      .testParam_notInValues.m("mat.PFG.drought.tol$PFG", mat.PFG.drought.tol$PFG, c("H", "C", "P", mat.PFG.dist$PFG))
    }
    if (sum(colnames(mat.PFG.drought.tol) == "responseStage") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.drought.tol$responseStage", mat.PFG.drought.tol$responseStage)
      .testParam_notInValues.m("mat.PFG.drought.tol$responseStage", mat.PFG.drought.tol$responseStage, 0:10)
      if (sum(colnames(mat.PFG.drought.tol) == "breakAge") == 1)
      {
        .testParam_notNum.m("mat.PFG.drought.tol$breakAge", mat.PFG.drought.tol$breakAge)
        .testParam_NAvalues.m("mat.PFG.drought.tol$breakAge", mat.PFG.drought.tol$breakAge)
        .testParam_notNum.m("mat.PFG.drought.tol$resproutAge", mat.PFG.drought.tol$resproutAge)
        .testParam_NAvalues.m("mat.PFG.drought.tol$resproutAge", mat.PFG.drought.tol$resproutAge)
      }
      if (sum(colnames(mat.PFG.drought.tol) == "killedIndiv") == 1)
      {
        .testParam_NAvalues.m("mat.PFG.drought.tol$killedIndiv", mat.PFG.drought.tol$killedIndiv)
        .testParam_notInValues.m("mat.PFG.drought.tol$killedIndiv", mat.PFG.drought.tol$killedIndiv, 0:10)
        .testParam_NAvalues.m("mat.PFG.drought.tol$resproutIndiv", mat.PFG.drought.tol$resproutIndiv)
        .testParam_notInValues.m("mat.PFG.drought.tol$resproutIndiv", mat.PFG.drought.tol$resproutIndiv, 0:10)
      }
    }
    if (sum(colnames(mat.PFG.drought.tol) == "strategy_tol") == 1)
    {
      mat.PFG.drought.tol$strategy_tol = as.character(mat.PFG.drought.tol$strategy_tol)
      .testParam_notInValues.m("mat.PFG.drought.tol$strategy_tol", mat.PFG.drought.tol$strategy_tol
                               , c("herbs_cham_1", "herbs_cham_2", "herbs_cham_3"
                                   , "trees_1", "trees_2", "trees_3"))
    }
  }
  ## CHECK parameter rasters
  names.rasters = c("name.MASK", "name.DIST", "name.DROUGHT", "name.FIRE", "name.ELEVATION", "name.SLOPE")
  .testParam_notInValues.m(names(rasters), names.rasters)
  if (length(rasters) != 6)
  {
    for (name.i in names.rasters)
    {
      if (!(name.i %in% names(rasters)))
      {
        rasters[name.i] = NA
      }
    }
  }
  rasters = rasters[names.rasters]
  ## CHECK parameter multipleSet
  names.multipleSet = c("name.simulation.1", "name.simulation.2", "file.simulParam.1", "file.simulParam.2"
                        , "no_simulations", "opt.percent_maxAbund", "opt.percent_seeding", "opt.percent_light"
                        , "opt.percent_soil", "do.max_abund_low", "do.max_abund_medium", "do.max_abund_high"
                        , "do.seeding_duration", "do.seeding_timestep", "do.seeding_input", "do.no_strata"
                        , "do.LIGHT.thresh_medium", "do.LIGHT.thresh_low", "do.SOIL.init", "do.SOIL.retention"
                        , "do.DISPERSAL.mode", "do.HABSUIT.mode")
  .testParam_notInValues.m(names(multipleSet), names.multipleSet)
  if (length(multipleSet) != 22)
  {
    for (name.i in names.multipleSet)
    {
      if (!(name.i %in% names(multipleSet)))
      {
        multipleSet[name.i] = NA
      }
    }
  }
  multipleSet = multipleSet[names.multipleSet]
  if (length(which(is.na(multipleSet))) < 22)
  {
    ## CHECK parameter name.simulation.1
    .testParam_existFolder(multipleSet['name.simulation.1'], "PARAM_SIMUL/")
    .testParam_existFolder(multipleSet['name.simulation.1'], "DATA/GLOBAL_PARAMETERS/")
    multipleSet['name.simulation.1'] = sub("/$", "", multipleSet['name.simulation.1'])
    
    ## CHECK parameter file.simulParam.1
    if (.testParam_notChar(multipleSet['file.simulParam.1']))
    {
      abs.simulParams = list.files(paste0(multipleSet['name.simulation.1'], "/PARAM_SIMUL/"))
      if (length(abs.simulParams) == 0)
      {
        stop(paste0("Missing data!\n The folder ", multipleSet['name.simulation.1']
                    , "/PARAM_SIMUL/ does not contain adequate files"))
      } else
      {
        stop(paste0("Missing data!\n The folder "
                    , multipleSet['name.simulation.1']
                    , "/PARAM_SIMUL/ contain one or more files.\n"
                    , "You must select one with the `file.simulParam.1` parameter "))
      }
    } else
    {
      multipleSet['file.simulParam.1'] = basename(multipleSet['file.simulParam.1'])
      multipleSet['file.simulParam.1'] = paste0(multipleSet['name.simulation.1'], "/PARAM_SIMUL/", multipleSet['file.simulParam.1'])
      .testParam_existFile(multipleSet['file.simulParam.1'])
      scenario1 = TRUE
    }
    ## CHECK parameter file.simulParam.2
    if (!.testParam_notChar(multipleSet['file.simulParam.2']))
    {
      if (!.testParam_notChar(multipleSet['name.simulation.2']))
      {
        .testParam_existFolder(multipleSet['name.simulation.2'], "PARAM_SIMUL/")
        .testParam_existFolder(multipleSet['name.simulation.2'], "DATA/GLOBAL_PARAMETERS/")
        multipleSet['name.simulation.2'] = sub("/$", "", multipleSet['name.simulation.2'])
        
        multipleSet['file.simulParam.2'] = basename(multipleSet['file.simulParam.2'])
        multipleSet['file.simulParam.2'] = paste0(multipleSet['name.simulation.2'], "/PARAM_SIMUL/", multipleSet['file.simulParam.2'])
        .testParam_existFile(multipleSet['file.simulParam.2'])
        
        if (multipleSet['name.simulation.1'] == multipleSet['name.simulation.2'] &&
            multipleSet['file.simulParam.1'] == multipleSet['file.simulParam.2'])
        {
          stop(paste0("You must select different simulation parameter files !"))
        }
        scenario1 = FALSE
      } else
      {
        multipleSet['file.simulParam.2'] = basename(multipleSet['file.simulParam.2'])
        multipleSet['file.simulParam.2'] = paste0(multipleSet['name.simulation.1']
                                                  , "/PARAM_SIMUL/", multipleSet['file.simulParam.2'])
        .testParam_existFile(multipleSet['file.simulParam.2'])
        if (multipleSet['file.simulParam.1'] == multipleSet['file.simulParam.2'])
        {
          stop(paste0("You must select different simulation parameter files !"))
        }
        scenario1 = FALSE
      }
    }
    ## CHECK parameter no_simulations
    .testParam_notInteger.m("multipleSet['no_simulations']", multipleSet['no_simulations'])
    .testParam_notRound.m("multipleSet['no_simulations']", multipleSet['no_simulations'])
    ## CHECK parameters scenario1
    if (scenario1)
    {
      .testParam_notNum.m("multipleSet['opt.percent_maxAbund']", multipleSet['opt.percent_maxAbund'])
      .testParam_notBetween.m("multipleSet['opt.percent_maxAbund']", multipleSet['opt.percent_maxAbund'], 0, 1)
      .testParam_notNum.m("multipleSet['opt.percent_seeding']", multipleSet['opt.percent_seeding'])
      .testParam_notBetween.m("multipleSet['opt.percent_seeding']", multipleSet['opt.percent_seeding'], 0, 1)
      .testParam_notNum.m("multipleSet['opt.percent_light']", multipleSet['opt.percent_light'])
      .testParam_notBetween.m("multipleSet['opt.percent_light']", multipleSet['opt.percent_light'], 0, 1)
      .testParam_notNum.m("multipleSet['opt.percent_soil']", multipleSet['opt.percent_soil'])
      .testParam_notBetween.m("multipleSet['opt.percent_soil']", multipleSet['opt.percent_soil'], 0, 1)
    }
    ## CHECK parameters do.[...]
    .testParam_notInValues.m("multipleSet['do.max_abund_low']", multipleSet['do.max_abund_low'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.max_abund_medium']", multipleSet['do.max_abund_medium'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.max_abund_high']", multipleSet['do.max_abund_high'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.seeding_duration']", multipleSet['do.seeding_duration'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.seeding_timestep']", multipleSet['do.seeding_timestep'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.seeding_input']", multipleSet['do.seeding_input'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.no_strata']", multipleSet['do.no_strata'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.LIGHT.thresh_medium']", multipleSet['do.LIGHT.thresh_medium'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.LIGHT.thresh_low']", multipleSet['do.LIGHT.thresh_low'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.SOIL.init']", multipleSet['do.SOIL.init'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.SOIL.retention']", multipleSet['do.SOIL.retention'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.DISPERSAL.mode']", multipleSet['do.DISPERSAL.mode'], c(0, 1))
    .testParam_notInValues.m("multipleSet['do.HABSUIT.mode']", multipleSet['do.HABSUIT.mode'], c(0, 1))
  }
  
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # SAVE_FATE.step2_parameters")
  cat("\n #------------------------------------------------------------# \n")
  
  # {
  #   cat("\n ---------- INFORMATION : SAMPLING \n")
  #   cat("\n  Number of releves : ", MO.no_releves)
  #   cat("\n  Number of sites : ", MO.no_sites)
  #   cat("\n  Number of species : ", MO.no_species)
  #   cat("\n")
  # }
  
  #############################################################################
  
  results = list(name.dataset
                 , name.simulation
                 , strata.limits
                 , mat.PFG.succ
                 , mat.PFG.light
                 , mat.PFG.light.tol
                 , mat.PFG.soil
                 , mat.PFG.soil.tol
                 , mat.PFG.disp
                 , mat.PFG.dist
                 , mat.PFG.dist.tol
                 , mat.PFG.drought
                 , mat.PFG.drought.tol
                 , rasters
                 , multipleSet
                 , name.arch_data
                 , name.arch_paramsimul)
  
  name.dataset = paste0("FATE_dataset_", name.dataset, "_step2_parameters")
  assign(name.dataset, results)
  save(name.dataset, file = paste0(name.dataset, ".RData"))
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  return(results)
}

