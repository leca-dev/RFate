### HEADER ##########################################################################
##'
##' @title Compute validation data for habitat, PFG richness and composition for a 
##' \code{FATE} simulation.
##' 
##' @name POST_FATE.validation
##' 
##' @author Matthieu Combaud, Maxime Delprat, Maya Guéguen
##' 
##' @description This script is designed to compare observed and simulated PFG 
##' distribution for one specific \code{FATE} simulation year by computing 
##' a) difference in PFG richness ; 
##' b) similarity between distribution quantiles ; 
##' c) a random forest model to predict habitat classes.
##' 
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param year an \code{integer} corresponding to the simulation year that will 
##' be used to extract PFG abundance table
##' @param mat.obs a \code{data.frame} with at least 5 columns : \cr 
##' \code{site}, \code{x}, \code{y}, \code{PFG}, \code{abund} 
##' \cr (\emph{and optionally, \code{strata}, \code{code.habitat}}) 
##' (see \href{POST_FATE.validation#details}{\code{Details}})
##' @param mat.hab a \code{data.frame} with 2 columns : \code{code.habitat}, 
##' \code{habitat}
##' @param ras.habitat a \code{string} corresponding to the file name of a 
##' raster mask, with an \code{integer} value within each pixel, corresponding 
##' to a specific habitat 
##' @param doRichness (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, 
##' difference in observed and simulated PFG richness will be run.
##' @param doComposition (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, 
##' difference in observed and simulated PFG distribution over habitat will be run.
##' @param doHabitat (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, 
##' a random forest model to predict habitat from PFG abundances will be run.
##' @param RF.seed default \code{123}. \cr An \code{integer} to be given to 
##' \code{\link[base]{set.seed}} function, in order to fix the produced results 
##' if needed, as dataset will be divided randomly into training and testing 
##' datasets for the random forest model
##' @param RF.training default \code{0.7}. \cr A \code{numeric} between \code{0} 
##' and \code{1} corresponding to the percentage of data that will be used as 
##' training dataset in the random forest model
##' @param doHabitat.allMap (\code{logical}) default \code{TRUE}. \cr If 
##' \code{TRUE}, habitat prediction over the whole simulation map will be run.
##' @param opt.ras_validation (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster mask, with either 
##' \code{0} or \code{1} within each pixel, \code{1} corresponding to the cells 
##' of the studied area in which the validation will take place
##' @param opt.keep_PFG (\emph{optional}) default \code{NULL}. \cr 
##' A \code{vector} of \code{character} corresponding to the names of the PFG to 
##' keep for the validation
##' @param opt.keep_strata (\emph{optional}) default \code{NULL}. \cr 
##' A \code{list} with names corresponding to the strata to keep for the 
##' validation, and each element containing correspondence with \code{FATE} 
##' strata definition
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{PFG validation 
##' habitat model and predictions} and one preanalytical graphic. \cr \cr
##' 
##' \describe{
##'   \item{PFG richness}{
##'   check if all simulated PFG abundances are superior to 0 within the 
##'   simulated area (\emph{only a subset of PFG can be examined with the 
##'   \code{opt.keep_PFG} parameter}).
##'   }
##'   
##'   \item{PFG composition}{
##'   for each PFG and each habitat / stratum combination, abundances for each 
##'   quartiles (\code{25}, \code{50}, \code{75} and \code{100} \%) is 
##'   calculated for both observed and simulated distributions. \cr
##'   (\emph{Only a subset of PFG and strata can be examined with the 
##'   \code{opt.keep_PFG} and \code{opt.keep_strata} parameters.}) \cr
##'   
##'   Then, a composition similarity between each PFG / habitat / strata 
##'   combination is calculated as a pseudo-distance between observed and 
##'   simulated quartiles \code{Q} :
##'   \deqn{S_{\text{ Habitat}_j \text{, }\text{Stratum}_k} = 
##'   \sum S_{\text{ PFG}_i \text{, }\text{Habitat}_j \text{, }\text{Stratum}_k}}
##'   with
##'   \deqn{S_{\text{ PFG}_i \text{, }\text{Habitat}_j \text{, }\text{Stratum}_k} 
##'   = 1 - \frac{\text{1}}{4} * \sum abs(Q_{\text{ q} \text{, }sim} - Q_{\text{ q} \text{, }obs})}
##'   with \code{q} varying from 1 to 4.
##'   }
##' 
##'   \item{Habitat validation}{
##'   a \code{\link[randomForest]{randomForest}} model is built to predict 
##'   habitat (provided through \code{ras.habitat}) in function of observed PFG 
##'   abundances. Only habitat code in \code{mat.hab} are effectively used and 
##'   predicted. TSS (True Skill Statistic) is computed between observed and 
##'   simulated habitat, and averaged over each habitat. TSS can also be 
##'   weighted by the share of each habitat in the observed habitat distribution.
##'   }
##' }
##' 
##' @return A \code{list} containing two to ten elements depending on the 
##' options selected :
##' \describe{
##'   \item{PFG richness}{three \code{vector} objects :
##'   \describe{
##'     \item{\code{rich.obs}}{\code{vector} containing names of PFG in observed 
##'     data}
##'     \item{\code{rich.sim}}{\code{vector} containing names of PFG in 
##'     simulated data}
##'     \item{\code{rich.diff}}{\code{vector} containing names of PFG in 
##'     observed but not in simulated data}
##'   }
##'   }
##'   \item{PFG composition}{
##'   \describe{
##'     \item{\code{compo.distrib}}{\code{data.frame} object with the following 
##'     columns :
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group}
##'       \item{\code{code.habitat}}{concerned habitat code}
##'       \item{\code{strata}}{concerned height stratum}
##'       \item{\code{quantile.perc}}{concerned quantile}
##'       \item{\code{quantile.obs}}{corresponding observed abundance}
##'       \item{\code{quantile.sim}}{corresponding simulated abundance}
##'     }
##'     }
##'     \item{\code{compo.proximity}}{\code{data.frame} object with the 
##'     following columns :
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group}
##'       \item{\code{code.habitat}}{concerned habitat code}
##'       \item{\code{strata}}{concerned height stratum}
##'       \item{\code{proximity}}{composition similarity between each 
##'       habitat/strata combination}
##'     }
##'     }
##'   }
##'   }
##'   \item{Habitat}{
##'   \describe{
##'     \item{\code{hab.RF.model}}{\code{randomForest} object obtained from 
##'     \code{\link[randomForest]{randomForest}} function}
##'     \item{\code{hab.RF.perf}}{\code{data.frame} object with the following 
##'     columns :
##'     \describe{
##'       \item{\code{dataset}}{\code{train}, \code{test} or \code{valid}}
##'       \item{\code{habitat}}{concerned habitat code}
##'       \item{\code{sensitivity}}{true positive rate}
##'       \item{\code{specificity}}{true negative rate}
##'       \item{\code{weight}}{share of each habitat in the observed habitat 
##'       distribution}
##'       \item{\code{TSS}}{mean True Skill Statistic}
##'       \item{\code{TSSw}}{mean weighted True Skill Statistic}
##'     }
##'     }
##'     \item{\code{hab.tab.pred}}{\code{data.frame} object with the following columns :
##'     \describe{
##'       \item{\code{pixel}}{concerned pixel}
##'       \item{\code{x}}{corresponding x-coordinate}
##'       \item{\code{y}}{corresponding y-coordinate}
##'       \item{\code{code.habitat}}{concerned habitat code}
##'       \item{\code{habitat.obs}}{observed habitat}
##'       \item{\code{habitat.sim}}{simulated habitat through random forest model}
##'       \item{\code{habitat.final}}{simulated habitat through random forest 
##'       model, with wrong predictions indicated as \code{failure}}
##'       \item{\code{fail_succ}}{wether or not the simulated habitat is the 
##'       same as the observed}
##'       \item{\code{color}}{corresponding color code}
##'     }
##'     }
##'     \item{\code{hab.ras.pred}}{\code{raster} object containing habitat 
##'     predictions}
##'     \item{\code{hab.plot}}{\code{ggplot2} object, representing 
##'     \code{hab.ras.pred} raster}
##'   }
##'   }
##' }
##' 
##' 
##' One to four \file{POST_FATE_TABLE_[...].csv} files are created : 
##' \describe{
##'   \item{\file{HAB_validation_compo_distribution_}}{\emph{if composition 
##'   module was activated}, containing \code{compo.distrib}}
##'   \item{\file{HAB_validation_compo_proximity_}}{\emph{if composition module 
##'   was activated}, containing \code{compo.proximity}}
##'   \item{\file{HAB_validation_RF_performance_}}{\emph{if habitat module was 
##'   activated}, containing \code{hab.RF.perf}}
##'   \item{\file{PIXEL_validation_RF_prediction_}}{\emph{if habitat module was 
##'   activated}, containing \code{hab.tab.pred}}
##' }
##' 
##' \file{HabitatPrediction_YEAR_[...].tif} and 
##' \file{POST_FATE_GRAPHIC_D_map_habitat_[...].pdf} files are created 
##' containing random forest predicted habitat raster and plot respectively.
##' 
##' 
##' @examples 
##' 
##' library(raster)
##' 
##' ## Create a simulation folder
##' PRE_FATE.skeletonDirectory(name.simulation = "FATE_Champsaur")
##' 
##' ## Load example data
##' Champsaur_params = .loadData("Champsaur_params", "RData")
##' .loadData("Champsaur_results_V1", "7z")
##' 
##' ## Please extract results files in the 'FATE_Champsaur/RESULTS' folder
##' 
##' ## Define a vector to choose habitats taken into account
##' mat.hab = data.frame(ID = c(6, 5, 7, 8)
##'                      , habitat = c("coniferous.forest"
##'                                    , "deciduous.forest"
##'                                    , "natural.grassland"
##'                                    , "woody.heatland"))
##' 
##' ## Habitat & validation maps
##' ras_simulation = Champsaur_params$stk.mask$Champsaur
##' ras.habitat = Champsaur_params$stk.mask$habitat
##' ras.habitat = projectRaster(from = ras.habitat, to = ras_simulation, method = "ngb")
##' # writeRaster(ras_simulation, filename = "FATE_Champsaur/DATA/MASK/MASK_Champsaur.tif")
##' 
##' ## Observed data
##' mat.obs = Champsaur_params$tab.releves
##' 
##' ## Transform observed PFG abundances into relative abundances
##' mat.obs$abund = PRE_FATE.abundBraunBlanquet(mat.obs$abund) / 100
##' mat.obs = aggregate(abund ~ site + PFG + strata + x + y
##'                     , data = mat.obs, FUN = "sum")
##'                         
##' ## Create Global and Simulation parameters
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_Champsaur"
##'                                  , opt.saving_abund_PFG_stratum = TRUE
##'                                  , opt.saving_abund_PFG = TRUE
##'                                  , opt.saving_abund_stratum = FALSE
##'                                  , required.no_PFG = 15
##'                                  , required.no_strata = 7
##'                                  , required.simul_duration = 2000
##'                                  , required.seeding_duration = 1000
##'                                  , required.seeding_timestep = 1
##'                                  , required.seeding_input = 100
##'                                  , required.potential_fecundity = 1
##'                                  , required.max_abund_low = 1000
##'                                  , required.max_abund_medium = 2000
##'                                  , required.max_abund_high = 3000
##'                                  , doDispersal = TRUE
##'                                  , DISPERSAL.mode = 1
##'                                  , DISPERSAL.saving = FALSE
##'                                  , doHabSuitability = TRUE
##'                                  , HABSUIT.mode = 1)
##'                                 
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_Champsaur"
##'                                 , name.MASK = "MASK_Champsaur.tif")
##' 
##' simul.param = "Simul_parameters_V1.txt"
##' # simul.param = paste0("FATE_Champsaur/PARAM_SIMUL/", simul.param)
##' 
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                      , file.simulParam = simul.param
##'                      , year = 2000
##'                      , mat.obs = mat.obs
##'                      , mat.hab = mat.hab
##'                      , ras.habitat = ras.habitat
##'                      , doHabitat = TRUE
##'                      , doHabitat.allMap = TRUE
##'                      , doComposition = TRUE
##'                      , doRichness = TRUE)
##' 
##' 
##' @export
##' 
##' 
##' @importFrom stats aggregate
##' @importFrom utils write.csv
##' @importFrom foreach foreach %do%
##' @importFrom reshape2 melt
##' @importFrom data.table fread
##' @importFrom dplyr group_by
##' @importFrom randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom raster ratify levels writeRaster
##' 
### END OF HEADER ###################################################################


POST_FATE.validation = function(name.simulation
                                , file.simulParam
                                , year
                                , mat.obs
                                , mat.hab
                                , ras.habitat
                                , doRichness = TRUE
                                , doComposition = TRUE
                                , doHabitat = TRUE
                                , RF.seed = 123
                                , RF.training = 0.7
                                , doHabitat.allMap = FALSE
                                , opt.ras_validation = NULL
                                , opt.keep_PFG = NULL
                                , opt.keep_strata = NULL)
{
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  .testParam_existFolder(name.simulation, "VALIDATION/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  ## CHECK parameter year
  .testParam_notInteger.m("year", year)
  ## CHECK parameter mat.obs
  if (.testParam_notDf(mat.obs))
  {
    .stopMessage_beDataframe("mat.obs")
  } else
  {
    if (nrow(mat.obs) == 0 || !(ncol(mat.obs) %in% c(5, 6, 7)))
    {
      .stopMessage_numRowCol("mat.obs", c("site", "x", "y", "PFG", "abund", "(strata)", "(code.habitat)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.obs))
                          , "5" = .testParam_notColnames(mat.obs, c("site", "x", "y", "PFG", "abund"))
                          , "6" = (.testParam_notColnames(mat.obs, c("site", "x", "y", "PFG", "abund", "strata")) &&
                                     .testParam_notColnames(mat.obs, c("site", "x", "y", "PFG", "abund", "code.habitat")))
                          , "7" = .testParam_notColnames(mat.obs, c("site", "x", "y", "PFG", "abund", "strata", "code.habitat"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.obs", c("site", "x", "y", "PFG", "abund", "(strata)", "(code.habitat)"))
      }
      mat.obs$site = as.character(mat.obs$site)
      .testParam_notChar.m("mat.obs$site", mat.obs$site)
      .testParam_notNum.m("mat.obs$abund", mat.obs$abund)
      .testParam_NAvalues.m("mat.obs$abund", mat.obs$abund)
      .testParam_notBetween.m("mat.obs$abund", mat.obs$abund, 0, 1)
      if (sum(colnames(mat.obs) == "strata") == 1)
      {
        if(.testParam_notNum(mat.obs$strata) && .testParam_notChar(mat.obs$strata))
        {
          stop("Wrong type of data!\n 'mat.obs$strata' must contain numeric or character values")
        }
      } else {
        mat.obs$strata = "all"
      }
      if (sum(colnames(mat.obs) == "code.habitat") == 1)
      {
        .testParam_notNum.m("mat.obs$code.habitat", mat.obs$code.habitat)
      }
    }
  }
  ## CHECK parameter mat.hab
  if (doHabitat || doComposition)
  {
    if (.testParam_notDf(mat.hab))
    {
      .stopMessage_beDataframe("mat.hab")
    } else
    {
      if (nrow(mat.hab) == 0 || ncol(mat.hab) != 2)
      {
        .stopMessage_numRowCol("mat.hab", c("code.habitat", "habitat"))
      }
      mat.hab$habitat = as.character(mat.hab$habitat)
      .testParam_notNum.m("mat.hab$code.habitat", mat.hab$code.habitat)
      .testParam_notChar.m("mat.hab$habitat", mat.hab$habitat)
    }
  }
  ## CHECK parameter ras.habitat
  .testParam_notChar.m("ras.habitat", ras.habitat)
  .testParam_existFile(ras.habitat)
  ras.habitat = raster(ras.habitat)
  ## CHECK parameter opt.keep_PFG
  GLOB_SIM = .getGraphics_PFG(name.simulation  = name.simulation
                              , abs.simulParam = abs.simulParams[1])
  
  list.PFG = GLOB_SIM$PFG
  if (!is.null(opt.keep_PFG)) {
    .testParam_notChar.m("opt.keep_PFG", opt.keep_PFG)
    .testParam_notInValues.m("opt.keep_PFG", opt.keep_PFG, list.PFG)
    list.PFG = opt.keep_PFG
  }
  ## CHECK parameter opt.keep_strata
  list.strata = as.character(unique(mat.obs$strata))
  if (!is.null(opt.keep_strata)) {
    .testParam_notInValues.m("names(opt.keep_strata)", names(opt.keep_strata), as.character(unique(mat.obs$strata)))
    list.strata.obs = names(opt.keep_strata)
    list.strata.sim = unique(unlist(opt.keep_strata))
  }
  
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.validation")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ### Preliminary checks
  #############################################################################
  
  infos.simul = foreach (abs.simulParam = abs.simulParams) %do%
    {
      GLOB_DIR = .getGraphics_results(name.simulation = name.simulation
                                      , abs.simulParam = abs.simulParam)
      file.abund = paste0(name.simulation
                          , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                          , ifelse(length(list.strata) == 1 && list.strata == "all", "", "perStrata_")
                          , basename(GLOB_DIR$dir.save)
                          , ".csv")
      if (!file.exists(file.abund)) {
        warning(paste0("File `perStrata` (", file.abund, ") does not exist. Validation per stratum has been desactivated."))
        file.abund = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(GLOB_DIR$dir.save)
                            , ".csv")
        mat.obs$strata = "all"
        list.strata = "all"
      }
      .testParam_existFile(file.abund)
      return(list(dir.save = basename(GLOB_DIR$dir.save)
                  , file.abund = file.abund))
    }
  names(infos.simul) = abs.simulParams
  
  ## Get raster mask ----------------------------------------------------------
  GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                , abs.simulParam = abs.simulParams[1])
  ras_simulation = GLOB_MASK$ras.mask
  
  .testParam_notSameRaster.m("ras.habitat", ras.habitat, "ras_simulation", ras_simulation)
  if (.testParam_notInValues("code.habitat", colnames(mat.obs))) {
    mat.obs$code.habitat = extract(x = ras.habitat, y = mat.obs[, c("x", "y")])
    mat.obs = mat.obs[which(!is.na(mat.obs$code.habitat)), ]
    if (nrow(mat.obs) == 0) {
      stop("Wrong type of data!\n Extracted values from `ras.habitat` are NA. Please check.")
    }
  }
  
  if (!is.null(opt.ras_validation)) {
    .testParam_notSameRaster.m("opt.ras_validation", opt.ras_validation, "ras_simulation", ras_simulation)
  }
  
  # ## Keep only releves in specific area
  # if (!is.null(external.training.mask)) {
  #   inMask = extract(x = external.training.mask, y = mat.obs[, c("x", "y")])
  #   mat.obs = mat.obs[which(!is.na(inMask)), ]
  #   # cat("\n 'releve' map has been cropped to match 'external.training.mask'. \n") TODO
  #   if (nrow(mat.obs) == 0) {
  #     # stop("Code habitat vector is empty. Please verify values of your ras.habitat map")
  #     # stop("Make sure to provide habitat values") TODO
  #   }
  # }
  
  # 3. Keep only releve on interesting habitat, strata and PFG
  mat.obs = mat.obs[which(mat.obs$code.habitat %in% mat.hab$code.habitat &
                            mat.obs$strata %in% list.strata &
                            mat.obs$PFG %in% list.PFG), ]
  if (nrow(mat.obs) == 0) {
    stop("Wrong type of data!\n Values in `mat.obs` do not match required levels (code.habitat, strata, PFG). Please check.")
  }
  
  
  #############################################################################
  ### A. EXTRACT INFORMATION FROM OBSERVED DATA
  #############################################################################
  
  cat("\n ---------- OBSERVED DATA \n")
  
  if (doHabitat | doComposition) {
    
    cat("\n> Get information table on site / habitat (mat.obs)...")
    sites.obs = unique(mat.obs[, which(colnames(mat.obs) %in% c("site", "x", "y", "code.habitat"))])
    sites.obs = merge(sites.obs, mat.hab, by = "code.habitat")
    sites.obs = sites.obs[, c("site", "x", "y", "code.habitat", "habitat")]
    
    perc = sapply(unique(sites.obs$habitat), function(x) {
      ind = which(sites.obs$habitat == x)
      return(100 * length(ind) / nrow(sites.obs))
    })
    names(perc) = unique(sites.obs$habitat)
    
    if (length(which(perc <= 1)) > 0) {
      toRemove.name = names(perc)[which(perc <= 1)]
      toRemove.code = mat.hab$code.habitat[which(mat.hab$habitat %in% toRemove.name)]
      mat.obs = mat.obs[-which(mat.obs$code.habitat %in% toRemove.code), ]
      mat.hab = mat.hab[-which(mat.hab$code.habitat %in% toRemove.code),]
      cat("\n    (", paste0(toRemove.name, collapse = " / ")
          , ") represent 1% or less of the habitats in the whole area, they will be deleted for the next steps. \n")
    }
    
    cat("\n> Get information table on site / habitat (simul)...")
    sites.sim = as.data.frame(rasterToPoints(ras.habitat))
    colnames(sites.sim) = c("x", "y", "code.habitat")
    sites.sim$pixel = cellFromXY(ras_simulation, sites.sim[, c("x", "y")])
    if (!is.null(opt.ras_validation)) {
      toKeep = opt.ras_validation[cellFromXY(opt.ras_validation, sites.sim[, c("x", "y")])]
    } else {
      toKeep = ras_simulation[cellFromXY(ras_simulation, sites.sim[, c("x", "y")])]
    }
    sites.sim = sites.sim[which(toKeep == 1), ]
    if (nrow(sites.sim) == 0) {
      stop("Wrong type of data!\n Extracted values from `ras_simulation` (or `ras_validation`) are NA. Please check.")
    }
    sites.sim <- merge(sites.sim, mat.hab, by = "code.habitat")
    sites.sim = sites.sim[, c("pixel", "x", "y", "code.habitat", "habitat")]
    
    #############################################################################
    ## Reorganize mat.obs by aggregating PFG abundances
    
    OBS = .valid_organizeData(mat = mat.obs
                              , fac.agg = c("site", "code.habitat", "strata", "PFG")
                              , fac.rel = c("site", "strata")
                              , fac.cast = "site"
                              , mat.sites = sites.obs)
    
    #############################################################################
    ## Obtain quantiles of PFG abundances
    
    if (doComposition) {
      cat("\n> Get observed distribution...")
      distrib.obs = .valid_getDistrib(mat.agg = OBS$mat.agg
                                      , list.PFG = list.PFG
                                      , list.habitat = mat.hab$code.habitat
                                      , list.strata = list.strata)
      colnames(distrib.obs)[which(colnames(distrib.obs) == "quantile.val")] = "quantile.obs"
    }
    
    #############################################################################
    ## Train a Random Forest model on observed data
    
    if (doHabitat) {
      cat("\n> Split observations into training / testing...")
      set.seed(RF.seed)
      
      mat.cast = OBS$mat.cast
      mat.cast$habitat = as.factor(mat.cast$habitat)
      freq = table(mat.cast$code.habitat) / nrow(mat.cast)
      no.hab = sample(names(freq), size = RF.training * nrow(mat.cast), prob = freq, replace = TRUE)
      no.hab = table(no.hab) ## Is it possible that length(no.hab) != length(freq) ?
      
      training.site = foreach(hab = 1:length(no.hab), .combine = "c") %do%
        {
          sample(mat.cast$site[which(mat.cast$code.habitat == names(no.hab)[hab])]
                 , size = no.hab[hab], replace = FALSE)
        }
      tab.train = mat.cast[which(mat.cast$site %in% training.site), ]
      tab.test = mat.cast[-which(mat.cast$site %in% training.site), ]
      
      cat("\n Training part of the data :")
      print(table(tab.train$habitat))
      cat("\n Testing part of the data :")
      print(table(tab.test$habitat))
      
      cat("\n> Calibrate Random Forest model...")
      ## Train the RF model (with correction for unbalanced sampling)
      mtry.perf = tuneRF(x = tab.train[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                         y = tab.train$habitat,
                         strata = tab.train$habitat,
                         sampsize = nrow(tab.train),
                         ntreeTry = 500,
                         stepFactor = 2,
                         improve = 0.05,
                         doBest = FALSE,
                         plot = FALSE,
                         trace = FALSE)
      
      ## Select model (lowest n achieving minimum OOB)
      mtry.perf = as.data.frame(mtry.perf)
      mtry = mtry.perf$mtry[which.min(mtry.perf$OOBError)]
      
      ## Run selected model on testing data
      RF.model = randomForest(x = tab.train[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                              y = tab.train$habitat,
                              xtest = tab.test[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                              ytest = tab.test$habitat,
                              strata = tab.train$habitat,
                              sampsize = nrow(tab.train),
                              ntree = 500,
                              mtry = mtry,
                              norm.votes = TRUE,
                              keep.forest = TRUE)
      
      ## Analyse model performance
      RF.perf.train = .valid_getModelPerf(dataset = "train"
                                          , mod.pred = RF.model$predicted
                                          , mod.ref = tab.train$habitat)
      
      RF.perf.test = .valid_getModelPerf(dataset = "test"
                                         , mod.pred = RF.model$test$predicted
                                         , mod.ref = tab.test$habitat)
      cat("\n")
    }
  }
  
  
  #############################################################################
  ### B. EXTRACT INFORMATION FROM SIMULATED DATA
  #############################################################################
  
  cat("\n ---------- SIMULATED DATA")
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
    {
      
      cat("\n+++++++\n")
      cat("\n  Simulation name : ", name.simulation)
      cat("\n  Simulation file : ", abs.simulParam)
      cat("\n")
      
      simul = infos.simul[[abs.simulParam]]
      
      ## Get the abundance table ------------------------------------------------
      .testParam_existFile(simul$file.abund)
      mat.sim = fread(simul$file.abund, data.table = FALSE)
      if (.testParam_notInValues("strata", colnames(mat.sim))) {
        mat.sim$strata = "all"
      }
      mat.sim = mat.sim[, c("ID.pixel", "X", "Y", "strata", "PFG", year)]
      colnames(mat.sim) = c("pixel", "x", "y", "strata", "PFG", "abund")
      if (!is.null(opt.keep_strata)) {
        new.strata <- rep("all", nrow(mat.sim))
        for (i in 1:length(opt.keep_strata)) {
          ind = which(mat.sim$strata %in% opt.keep_strata[[i]])
          new.strata[ind] = names(opt.keep_strata)[i]
        }
        mat.sim$strata = new.strata
      }
      
      mat.sim = mat.sim[which(mat.sim$strata %in% list.strata &
                                mat.sim$PFG %in% list.PFG), ]
      
      ## Get the abundance table ------------------------------------------------
      if (doRichness == TRUE) {
        rich.sim = unique(mat.sim$PFG)
        rich.sim = rich.sim[which(rich.sim %in% list.PFG)]
      }
      
      #############################################################################
      ## Reorganize mat.sim by aggregating PFG abundances
      
      SIM = .valid_organizeData(mat = mat.sim
                                , fac.agg = c("pixel", "strata", "PFG")
                                , fac.rel = c("pixel", "strata")
                                , fac.cast = "pixel"
                                , mat.sites = sites.sim)
      
      #############################################################################
      
      if (doComposition) {
        
        ## Obtain quantiles of PFG abundances ---------------------------------
        cat("\n> Get simulated distribution...")
        distrib.sim = .valid_getDistrib(mat.agg = SIM$mat.agg
                                        , list.PFG = list.PFG
                                        , list.habitat = mat.hab$code.habitat
                                        , list.strata = list.strata)
        colnames(distrib.sim)[which(colnames(distrib.sim) == "quantile.val")] = "quantile.sim"
        
        ## Merge observed and simulated distributions
        distrib.ALL <- merge(distrib.obs, distrib.sim, by = c("PFG", "code.habitat", "strata", "quantile.perc"), all = TRUE)
        
        write.csv(distrib.ALL
                  , file = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_TABLE_HAB_validation_compo_distribution_"
                                  , simul$dir.save
                                  , ".csv")
                  , row.names = FALSE)
        
        message(paste0("\n The output file POST_FATE_TABLE_HAB_validation_compo_distribution_"
                       , simul$dir.save
                       , ".csv has been successfully created !\n"))
        
        ## Compute proximity --------------------------------------------------
        cat("\n> Compute proximity...")
        
        ## Distance, computed as the sum of absolute gap between obs and sim quantile
        ## (on a 0 to 1 scale, 1 meaning quantile equality)
        compute.proximity <- function(qt.obs, qt.sim) {
          return(1 - sum(abs(qt.sim - qt.obs)) / 4)
        }
        
        proximity = split(distrib.ALL, list(distrib.ALL$PFG, distrib.ALL$code.habitat, distrib.ALL$strata), drop = TRUE)
        proximity = foreach(tmp = proximity, .combine = "rbind") %do%
          {
            qt = compute.proximity(qt.obs = tmp$quantile.obs, qt.sim = tmp$quantile.sim)
            return(data.frame(PFG = unique(tmp$PFG)
                              , code.habitat = unique(tmp$code.habitat)
                              , strata = unique(tmp$strata)
                              , proximity = qt))
          }
        
        write.csv(proximity
                  , file = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_TABLE_HAB_validation_compo_proximity_"
                                  , simul$dir.save
                                  , ".csv")
                  , row.names = FALSE)
        
        message(paste0("\n The output file POST_FATE_TABLE_HAB_validation_compo_proximity_"
                       , simul$dir.save
                       , ".csv has been successfully created !\n"))
        
        # # 10. Aggregate results for the different PFG
        # aggregated.proximity = split(proximity, list(proximity$code.habitat, proximity$strata), drop = TRUE)
        # aggregated.proximity = foreach(tmp = aggregated.proximity, .combine = "rbind") %do%
        #   {
        #     return(data.frame(simul = simul$dir.save
        #                       , code.habitat = unique(tmp$code.habitat)
        #                       , strata = unique(tmp$strata)
        #                       , aggregated.proximity = mean(tmp$proximity)))
        #   }
        # performance.composition <- list(aggregated.proximity = aggregated.proximity)
      }
      
      #############################################################################
      ## Run selected Random Forest model on simulated data
      
      if (doHabitat) {
        cat("\n> Run and evaluate Random Forest model on simulated data...")
        
        mat.cast.sim = SIM$mat.cast
        mat.cast.sim$habitat <- factor(mat.cast.sim$habitat, levels = RF.model$classes)
        RF.predictors <- rownames(RF.model$importance)
        if (length(setdiff(RF.predictors, colnames(mat.cast.sim))) > 0) {
          stop(paste0("Missing data!\n Some PFG used within the random forest model are not found within the simulated dataset ("
                      , paste0(setdiff(RF.predictors, colnames(mat.cast.sim)), collapse = " / "), ")"))
        }
        
        ## Use selected RF to predict habitat onto simulated data
        RF.pred <- predict(object = RF.model, newdata = mat.cast.sim[, RF.predictors], type = "response", norm.votes = TRUE)
        
        ## Analyse model performance
        RF.perf.valid = .valid_getModelPerf(dataset = "valid"
                                            , mod.pred = RF.pred
                                            , mod.ref = factor(mat.cast.sim$habitat, RF.model$classes))
        
        ## Merge all model performances
        RF.perf = do.call(rbind, list(RF.perf.train, RF.perf.test, RF.perf.valid))
        rownames(RF.perf) = NULL
        
        write.csv(RF.perf
                  , file = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_TABLE_HAB_validation_RF_performance_"
                                  , simul$dir.save
                                  , ".csv")
                  , row.names = FALSE)
        
        message(paste0("\n The output file POST_FATE_TABLE_HAB_validation_RF_performance_"
                       , simul$dir.save
                       , ".csv has been successfully created !\n"))
        
        
        ## Predict / plot habitat for whole simulation map --------------------
        if (doHabitat.allMap) {
          cat("\n> Predict habitat on simulated data...")
          
          ## Create a correspondance table between predicted habitat / colors
          col.df = data.frame(habitat.obs = RF.model$classes,
                              failure = hcl.colors(n = length(RF.model$classes), palette = "Roma", alpha = 0.5),
                              success = hcl.colors(n = length(RF.model$classes), palette = "Roma", alpha = 1))
          col.df = melt(col.df, id.vars = c("habitat.obs"), variable.name = "fail_succ", value.name = "color")
          
          col_label = paste0(col.df$habitat.obs, " - ", col.df$fail_succ)
          names(col_label) = col.df$color
          
          ## Create the whole map prediction table
          pred.allMap = data.frame(pixel = mat.cast.sim$pixel
                                   , habitat.obs = as.character(mat.cast.sim$habitat)
                                   , habitat.sim = as.character(RF.pred))
          pred.allMap$habitat.final = pred.allMap$habitat.sim
          pred.allMap$habitat.final[which(pred.allMap$habitat.obs != pred.allMap$habitat.sim)] = "failure"
          pred.allMap$fail_succ = ifelse(pred.allMap$habitat.final == "failure", "failure", "success")
          pred.allMap = merge(pred.allMap, col.df, by.x = c("habitat.obs", "fail_succ"))
          pred.allMap = merge(pred.allMap, sites.sim, by.x = c("pixel", "habitat.obs"), by.y = c("pixel", "habitat"), all.x = TRUE)
          pred.allMap$code.habitat[which(pred.allMap$habitat.final == "failure")] = -1
          
          write.csv(pred.allMap
                    , file = paste0(name.simulation
                                    , "/RESULTS/POST_FATE_TABLE_PIXEL_validation_RF_prediction_"
                                    , simul$dir.save
                                    , ".csv")
                    , row.names = FALSE)
          
          message(paste0("\n The output file POST_FATE_TABLE_PIXEL_validation_RF_prediction_"
                         , simul$dir.save
                         , ".csv has been successfully created !\n"))
          
          ## Transform into a categorical raster map
          ras.allMap = ras_simulation
          ras.allMap[] = -1
          ras.allMap[pred.allMap$pixel] = pred.allMap$code.habitat
          ras.allMap = ratify(ras.allMap)
          map.rat = unique(pred.allMap[, c("code.habitat", "habitat.final")])
          colnames(map.rat) = c("ID", "habitat")
          levels(ras.allMap) = map.rat[order(map.rat$ID), ]
          
          new_name = paste0(name.simulation, "/RESULTS/", simul$dir.save, "/HabitatPrediction_YEAR_", year, ".tif")
          writeRaster(ras.allMap, filename = new_name, overwrite = TRUE)
          
          message(paste0("\n The output files \n"
                         , paste0(" > ", basename(new_name), " \n"
                                  , collapse = "")
                         , "have been successfully created !\n"))
          
          ## Create a ggplot
          pp = ggplot(pred.allMap, aes(x = x, y = y, fill = factor(color, levels(factor(col.df$color))))) +
            geom_raster() +
            coord_equal() +
            scale_fill_identity(guide = "legend", labels = col_label, drop = FALSE) +
            guides(fill = guide_legend(nrow = 4, byrow = FALSE)) +
            theme(plot.title = element_text(size = 8),
                  legend.text = element_text(size = 8, colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "bottom",
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank())
          
          pdf(file = paste0(name.simulation
                            , "/RESULTS/POST_FATE_GRAPHIC_D_map_habitat_"
                            , simul$dir.save, "_YEAR_", year, ".pdf")
              , width = 10, height = 8)
          plot(pp)
          dev.off()
        } 
      }
      
      #############################################################################
      
      cat("\n> Done!\n")
      
      results = list()
      if (doRichness) {
        results$rich.obs = sort(list.PFG)
        results$rich.sim = sort(rich.sim)
        results$rich.diff = setdiff(list.PFG, rich.sim)
      }
      if (doComposition) {
        results$compo.distrib = distrib.ALL
        results$compo.proximity = proximity
      }
      if (doHabitat) {
        results$hab.RF.model = RF.model
        results$hab.RF.perf = RF.perf
        if (doHabitat.allMap) {
          results$hab.tab.pred = pred.allMap
          results$hab.ras.pred = ras.allMap
          results$hab.plot = pp
        }
      }
      
      return(results)
    } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}


###################################################################################################

.valid_organizeData = function(mat, fac.agg, fac.rel, fac.cast, mat.sites)
{
  mat <- mat[, c(fac.agg, "abund")]
  
  ## Compute sum of abundance per fac.agg -------------------------------------
  txt.command = paste0(fac.agg, collapse = " + ")
  eval(parse(text = paste0('mat.agg = aggregate(abund ~ ', txt.command, ', data = mat, FUN = "sum")')))
  
  ## Compute relative abundance per fac.rel
  eval(parse(text = paste0('tmp = mat.agg %>% group_by(', paste0(fac.rel, collapse = ", "), ')')))
  mat.agg = as.data.frame(
    tmp %>% mutate(relative.metric = round(prop.table(abund), digits = 2))
  )
  
  ## Remove NA and abund column
  if (length(which(is.na(mat.agg$relative.metric)))) {
    mat.agg$relative.metric[which(is.na(mat.agg$relative.metric))] = 0
  }
  mat.agg$abund = NULL
  mat.agg = merge(mat.sites, mat.agg, by = intersect(colnames(mat.sites), colnames(mat.agg)))
  # cat("\n> Releves data have been transformed into a relative metric")
  
  ## --------------------------------------------------------------------------
  mat.cast = mat.agg
  mat.cast$PFG = as.factor(mat.cast$PFG)
  mat.cast$strata = as.factor(mat.cast$strata)
  eval(parse(text = paste0('mat.cast = reshape2::dcast(mat.cast, ', fac.cast
                           , ' ~ PFG * strata, value.var = "relative.metric", fill = 0, drop = FALSE)')))
  mat.cast = merge(mat.sites, mat.cast, by = fac.cast)
  
  ## --------------------------------------------------------------------------
  return(list(mat.agg = mat.agg, mat.cast = mat.cast))
}


.valid_getDistrib = function(mat.agg, list.PFG, list.habitat, list.strata) #, fac.agg)
{
  fac.agg = c("PFG", "code.habitat", "strata")
  txt.command = paste0('mat.agg$', fac.agg, collapse = ", ")
  eval(parse(text = paste0('distrib = split(mat.agg, list(', txt.command, '), drop = TRUE)')))
  # distrib = split(mat.agg, list(mat.agg$PFG, mat.agg$code.habitat, mat.agg$strata), drop = TRUE)
  distrib = foreach(tmp = distrib, .combine = "rbind") %do%
    {
      qt = quantile(tmp$relative.metric, probs = seq(0.25, 1, 0.25))
      return(data.frame(PFG = unique(tmp$PFG)
                        , code.habitat = unique(tmp$code.habitat)
                        , strata = unique(tmp$strata)
                        , quantile.perc = seq(0.25, 1, 0.25)
                        , quantile.val = as.vector(qt)))
    }
  
  all.distrib <- expand.grid(PFG = list.PFG                            
                             , code.habitat = list.habitat
                             , strata = list.strata
                             , quantile.perc = seq(0.25, 1, 0.25)
                             , stringsAsFactors = FALSE)
  all.distrib <- merge(all.distrib[, c(fac.agg, "quantile.perc")], distrib, by = c(fac.agg, "quantile.perc"), all.x = TRUE)
  all.distrib$quantile.val[is.na(all.distrib$quantile.val)] <- 0
  
  return(all.distrib)
}


.valid_getModelPerf = function(dataset, mod.pred, mod.ref)
{
  mat.conf = confusionMatrix(data = mod.pred, reference = mod.ref)
  mat.synth = data.frame(dataset = dataset
                         , habitat = colnames(mat.conf$table)
                         , sensitivity = mat.conf$byClass[, "Sensitivity"]
                         , specificity = mat.conf$byClass[, "Specificity"]
                         , weight = colSums(mat.conf$table) / sum(colSums(mat.conf$table)))
  #warning: prevalence is the weight of predicted habitat, not of observed habitat
  mat.synth$TSS = round(mat.synth$sensitivity + mat.synth$specificity - 1, digits = 2)
  mat.synth$TSSw = round(sum(mat.synth$weight * mat.synth$TSS), digits = 2)
  return(mat.synth)
}
