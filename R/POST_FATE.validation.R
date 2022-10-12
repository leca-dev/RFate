### HEADER ##########################################################################
##'
##' @title Compute validation data for habitat, PFG richness and composition for a \code{FATE} simulation.
##' 
##' @name POST_FATE.validation
##' 
##' @author Matthieu Combaud, Maxime Delprat, Maya Guéguen
##' 
##' @description This script is designed to compute validation data for : \cr
##' \describe{
##'   \item{Habitat}{Compares simulated and observed habitat and
##' create a map to visualize this comparison with all the \code{FATE} and
##' observed data \cr (if option selected).}
##'   \item{PFG Composition}{Computes the similarity between observed and simulated 
##'   PFG composition for a chosen set of PFG/habitat/strata combinations.}
##'   \item{PFG Richness}{Computes the PFG richness over the whole simulation area 
##' for a \code{FATE} simulation and computes the difference between observed and simulated PFG richness.}
##' }
##' 
##' @param name.simulation a \code{string} corresponding to the simulation folder name.
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation.
##' @param year an \code{integer} corresponding to the year of simulation for validation.
##' @param perStrata (\code{logical}) default \code{FALSE}. \cr 
##' If \code{TRUE}, habitat & PFG composition are computed with PFG abundance defined by strata. \cr
##' If \code{FALSE}, habitat & PFG composition are computed with PFG abundance defined for all strata.
##' @param opt.no_CPU default \code{1}. \cr An \code{integer} corresponding to the number of resources that can be used to 
##' parallelize the computation of prediction performance for habitat & richness validation.
##' 
##' @param doHabitat (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, habitat validation module is activated,
##' if \code{FALSE}, habitat validation module is disabled.
##' @param mat.obs a \code{data.frame} with at least 5 columns : \cr
##' \code{site}, \code{x}, \code{y}, \code{abund}, \code{PFG}
##' \cr (\emph{and optionally, \code{strata}, \code{code.habitat}})
##' \cr (see \href{POST_FATE.validation#details}{\code{Details}}).
##' @param ras_habitat a \code{raster} map of the habitats within the studied area, with same projection 
##' & resolution than simulation mask.
##' @param mat.hab a \code{data.frame} with 2 columns : \cr
##' \code{ID} ,\code{habitat}
##' \cr (see \href{POST_FATE.validation#details}{\code{Details}}).
##' @param doHabitat.allMap (\code{logical}) default \code{FALSE}. \cr If \code{TRUE}, the function will compute habitat prediction 
##' & performance over the whole map and will provide a prediction map.
##' @param RF.seed default \code{123}. \cr An \code{integer} corresponding to the number of seeds to set 
##' in order to generate a \code{Random Forest} model.
##' @param RF.training default \code{0.7}. \cr A \code{numeric} between 0 & 1 corresponding to the part of the data used for 
##' training a \code{Random Forest} model on \code{mat.obs} data.
##' @param opt.ras_validation (\code{optional}) default \code{NULL}. \cr A \code{raster} map (with 0 or 1 in each pixel) that specified on 
##' which pixels the performance of the habitat prediction will be compute, with same projection & resolution than simulation mask 
##' \cr If \code{NULL}, the function will take the simulation mask (which means that the performance will be compute over the whole map).
##' @param opt.keep_strata (\code{optional}) default \code{NULL}. \cr If \code{perStrata} = \code{TRUE}, 
##' a \code{list} which contains, for each stratum defined in \code{mat.obs}, the correspondence with \code{FATE} strata definition. \cr
##' If \code{perStrata} = \code{FALSE}, please specify \code{NULL} value.
##' 
##' @param doComposition (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, PFG composition validation module is activated.
##' 
##' @param doRichness (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, PFG richness validation module is activated.
##' @param opt.keep_PFG (\code{optional}) default \code{NULL}. \cr A \code{character} vector containing the name(s) 
##' of the PFG(s) that will be excluded from the composition and richness analysis.
##' 
##' @details 
##' 
##' \describe{
##'   \item{Habitat validation}{ \cr
##'   \describe{
##'     \item{Observed habitat}{is provided by the \strong{ras_habitat} map. The final set of habitats taken into account
##'     in the validation is provided by \strong{mat.hab} table which contains 
##'     all the \strong{habitats}, with their corresponding \strong{ID}.}
##'     \item{Simulated habitat}{is determined from \code{FATE} simulated relative abundances 
##'     (see \code{\link{POST_FATE.temporalEvolution}}), thanks to a \code{Random Forest} algorithm. 
##'     The model is trained on \strong{mat.obs} data. Information about \strong{PFG abundances} at each
##'     \strong{sites} with \strong{xy} coordinates are necessary. PFG abundances can be given in absolute abundance
##'     (only in sites where at least one PFG is present) or in presence-absence data.\cr
##'     Eventually, \strong{habitat ID} information can be provided, as well as \strong{strata} names.
##'     If not, habitat information will be taken from \strong{ras_habitat} map, and PFG abundance will be considered for all strata.}
##'   }
##'   To compare observations and simulations, the function computes confusion matrix between 
##'   observations and predictions and then computes the TSS for each habitat h.
##'   \deqn{TSS_{\text{ h}} = (\frac{\text{True Positives}_{\text{ h}}}{\text{True Positives}_{\text{ h}}{\text{ + False Negatives}_{\text{ h}}}} + \frac{\text{True Negatives}_{\text{ h}}}{\text{True Negatives}_{\text{ h}}{\text{ + False Positives}_{\text{ h}}}}) - \text{1}}
##'   The final metric used is the mean of TSS per habitat over all habitats, weighted by the share of each habitat in the observed 
##'   habitat distribution. The habitat validation also provides a visual comparison of observed and 
##'   simulated habitat on the whole studied area, if option selected.}
##'   
##'   \item{PFG composition validation}{ \cr 
##'   \describe{
##'     \item{Observed composition}{is computed for a set of PFG, habitat & strata
##'   (for all strata if strata definition is not activated) by computing 4 quartiles of the composition, based
##'   on \strong{mat.obs} data provided. Habitats are chosen in \code{mat.hab},
##'   PFG list is defined in the file \code{file.simulParam} and, if defined, \code{mat.obs} strata
##'   are taken into account. Optionally, PFGs contained in \code{opt.keep_PFG} can be excluded from the analysis.}
##'     \item{Simulated composition}{is computed in the same way than the observed composition, with 
##'     \code{FATE} abundances \cr (see \code{\link{POST_FATE.temporalEvolution}}).}
##'   }
##'   Then, a composition similarity between each habitat/strata combinations is provided by computing a 
##'   pseudo-distance between observed and simulated quartiles for each PFG y.
##'   \deqn{S_{\text{ habitat, strata}} = \sum S_{\text{ y}{\text{, }habitat}{\text{, }strata}}}
##'   with
##'   \deqn{S_{\text{ y}{\text{, }habitat}{\text{, }strata}} = 1 - \frac{\text{1}}{4} * \sum abs(Q_{\text{ i}{\text{, }sim}} - Q_{\text{ i}{\text{, }obs}})}
##'   with i varying from 1 to 4.
##'   }
##'   \item{PFG richness validation}{the observed PFG richness corresponds to the PFG list contained in the file \code{file.simulParam}, 
##'   the simulated PFG richness is the number of PFG for which abundance over the simulation area 
##'   is strictly superior to zero for the simulation year under scrutiny. \cr
##'   Then, observed and simulated richness are compared for all the PFGs in the simulation in order to quantify the PFG mortality.
##'   Optionally, PFGs contained in \code{opt.keep_PFG} can be excluded from the analysis.}
##' }
##' 
##' @return
##' \describe{
##'   \item{Habitat}{into the \code{name.simulation/VALIDATION/HABITAT/ directory :} \cr
##'   1 .csv file containing the prepared observed data (relative abundances & habitat information). \cr
##'   1 .rds file containing the RF model. \cr
##'   5 .csv files containing the performance analysis (confusion matrix and TSS) for the training & 
##'   testing parts of the RF model and the final habitat performance. \cr
##'   If option selected, 1 .csv file containing the habitat prediction within each pixel, 1 .csv file with 
##'   sucess or failure of the prediction and 1 .png prediction map.}
##' }
##' \describe{
##'   \item{PFG Composition}{into the \code{name.simulation/VALIDATION/PFG COMPOSITION/ directory :} \cr
##'   1 .csv file containing the proximity between observed and simulated data computed for each PFG/strata/habitat. \cr 
##'   1 .csv file containing the observed releves transformed into relative metrics. \cr 
##'   1 .csv file containing the final output with the distribution per PFG, strata and habitat.}
##' }
##' \describe{
##'   \item{PFG richness}{into the \code{name.simulation/VALIDATION/PFG RICHNESS/ directory :} \cr
##'   1 .csv file of PFG richness in a \code{FATE} simulation. \cr 
##'   1 .csv fie of the PFG extinction frequency in a \code{FATE} simulation. \cr 
##'   1 .rds file which is the abundance per PFG file.}
##' }
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
##'                              , habitat = c("coniferous.forest"
##'                                            , "deciduous.forest"
##'                                            , "natural.grassland"
##'                                            , "woody.heatland"))
##' 
##' ## Habitat & validation maps
##' ras_habitaterved = Champsaur_params$stk.mask$habitat
##' ras_simulation = Champsaur_params$stk.mask$Champsaur
##' ras_habitat = projectRaster(from = ras_habitaterved, to = ras_simulation, method = "ngb")
##' writeRaster(ras_simulation, filename = "FATE_Champsaur/DATA/MASK/MASK_Champsaur.tif")
##' 
##' ## Observed data
##' mat.obs = Champsaur_params$tab.releves
##' 
##' ## Transform observed PFG abundances into relative abundances
##' mat.obs$abund = PRE_FATE.abundBraunBlanquet(mat.obs$abund)/100
##' mat.obs = aggregate(abund ~ site + PFG + strata + x + y
##'                         , data = mat.obs, FUN = "sum")
##'                         
##' ## Create Global and Simulation parameters
##' PRE_FATE.params_globalParameters(name.simulation = "FATE_Champsaur"
##'                                 , opt.saving_abund_PFG_stratum = TRUE
##'                                 , opt.saving_abund_PFG = TRUE
##'                                 , opt.saving_abund_stratum = FALSE
##'                                 , required.no_PFG = 15
##'                                 , required.no_strata = 7
##'                                 , required.simul_duration = 2000
##'                                 , required.seeding_duration = 1000
##'                                 , required.seeding_timestep = 1
##'                                 , required.seeding_input = 100
##'                                 , required.potential_fecundity = 1
##'                                 , required.max_abund_low = 1000
##'                                 , required.max_abund_medium = 2000
##'                                 , required.max_abund_high = 3000
##'                                 , doDispersal = TRUE
##'                                 , DISPERSAL.mode = 1
##'                                 , DISPERSAL.saving = FALSE
##'                                 , doHabSuitability = TRUE
##'                                 , HABSUIT.mode = 1)
##'                                 
##' PRE_FATE.params_simulParameters(name.simulation = "FATE_Champsaur"
##'                                 , name.MASK = "MASK_Champsaur.tif")
##' 
##' param = "Simul_parameters_V1.txt"
##' simul.param = paste0("FATE_Champsaur/PARAM_SIMUL/", param)
##' 
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                      , file.simulParam = simul.param
##'                      , year = 2000
##'                      , doHabitat = TRUE
##'                      , mat.obs = mat.obs
##'                      , ras_habitat = ras_habitat
##'                      , mat.hab = mat.hab
##'                      , opt.keep_strata = opt.keep_strata
##'                      , doHabitat.allMap = TRUE
##'                      , doComposition = TRUE
##'                      , doRichness = TRUE)
##' 
##' 
##' @export
##' 
##' @importFrom stringr str_split
##' @importFrom raster raster res crop origin compareCRS extent ncell getValues levels
##' @importFrom utils read.csv
##' @importFrom foreach foreach %dopar%
##' @importFrom forcats fct_expand
##' @importFrom readr write_rds
##' @importFrom doParallel registerDoParallel
##' @importFrom dplyr select rename
##' @importFrom tidyselect all_of
##' @importFrom stats aggregate
##' @importFrom data.table fread fwrite
##' 
### END OF HEADER ###################################################################


POST_FATE.validation = function(name.simulation
                                , file.simulParam
                                , year
                                , mat.obs
                                , mat.hab
                                , ras_habitat
                                , RF.seed = 123
                                , RF.training = 0.7
                                , doHabitat = TRUE
                                , doHabitat.allMap = FALSE
                                , doComposition = TRUE
                                , doRichness = TRUE
                                , opt.ras_validation = NULL
                                , opt.keep_PFG = NULL
                                , opt.keep_strata = NULL
                                , opt.no_CPU = 1)
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
  ## CHECK parameter opt.keep_PFG
  GLOB_SIM = .getGraphics_PFG(name.simulation  = name.simulation
                              , abs.simulParam = abs.simulParams[1])
  
  list.PFG = GLOB_SIM$PFG
  if (!is.null(opt.keep_PFG)) {
    .testParam_notChar.m("opt.keep_PFG", opt.keep_PFG)
    list.PFG = list.PFG[which(list.PFG %in% opt.keep_PFG)]
    if (length(list.PFG) == 0) {
      stop("PROBLEM") ##TODO
    }
  }
  ## CHECK parameter opt.keep_strata
  list.strata = as.character(unique(mat.obs$strata))
  if (!is.null(opt.keep_strata)) {
    .testParam_notInValues.m("names(opt.keep_strata)", names(opt.keep_strata), as.character(unique(mat.obs$strata)))
    list.strata = names(opt.keep_strata)
  }
  
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.validation")
  cat("\n #------------------------------------------------------------# \n")
  
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
        file.abund = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(GLOB_DIR$dir.save)
                            , ".csv")
        mat.obs$strata = "all"
        list.strata = "all"
        warning("PROBLEM") ##TODO
      }
      if (!file.exists(file.abund)) {
        stop("PROBLEM") ##TODO
      }
      return(list(dir.save = basename(GLOB_DIR$dir.save)
                  , file.abund = file.abund))
    }
  
  ## Get raster mask --------------------------------------------------------
  GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                , abs.simulParam = abs.simulParams[1])
  ras_simulation = GLOB_MASK$ras.mask
  
  
  .testParam_notSameRaster.m("ras_habitat", ras_habitat, "ras_simulation", ras_simulation)
  if (.testParam_notInValues("code.habitat", colnames(mat.obs))) {
    mat.obs$code.habitat = extract(x = ras_habitat, y = mat.obs[, c("x", "y")])
    mat.obs = mat.obs[which(!is.na(mat.obs$code.habitat)), ]
    if (nrow(mat.obs) == 0) {
      # stop("Code habitat vector is empty. Please verify values of your ras_habitat map")
      # stop("Make sure to provide habitat values") TODO
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
  #     # stop("Code habitat vector is empty. Please verify values of your ras_habitat map")
  #     # stop("Make sure to provide habitat values") TODO
  #   }
  # }
  
  # 3. Keep only releve on interesting habitat, strata and PFG
  # mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$code.habitat %in% mat.hab$code.habitat &
  #                                   mat.PFG.agg$strata %in% list.strata &
  #                                   mat.PFG.agg$PFG %in% opt.keep_PFG), ]
  
  #############################################################################
  
  cat("\n ----------- OBSERVED DATA")
  
  if (doHabitat == TRUE | doComposition == TRUE) {
    ## GET INFOS on site / habitat for OBSERVED data ----------------------------------------------
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
      cat("\n > (", paste0(toRemove.name, collapse = " / ")
          , ") represent 1% or less of the habitats in the whole area, they will be deleted for the next steps. \n")
    }
    
    ## GET INFOS on site / habitat for SIMULATION data --------------------------------------------
    sites.sim = as.data.frame(rasterToPoints(ras_habitat))
    colnames(sites.sim) = c("x", "y", "code.habitat")
    sites.sim$pixel = cellFromXY(ras_simulation, sites.sim[, c("x", "y")])
    if (!is.null(opt.ras_validation)) {
      toKeep = opt.ras_validation[cellFromXY(opt.ras_validation, sites.sim[, c("x", "y")])]
    } else {
      toKeep = ras_simulation[cellFromXY(ras_simulation, sites.sim[, c("x", "y")])]
    }
    sites.sim = sites.sim[which(toKeep == 1), ]
    if (nrow(sites.sim)) {
      stop("PROBLEM") ## TODO
    }
    sites.sim <- merge(sites.sim, mat.hab, by = "code.habitat")
    sites.sim = sites.sim[, c("pixel", "x", "y", "code.habitat", "habitat")]
    
    ## ----------------------------------------------------------------
      
    OBS = .valid_organizeData(mat = mat.obs
                              , fac.agg = c("site", "code.habitat", "strata", "PFG")
                              , fac.rel = c("site", "strata")
                              , fac.cast = "site"
                              , mat.sites = sites.obs)
      
    if (doComposition) {
      cat("\n > Get observed distribution...")
      distrib.obs = .valid_getDistrib(mat.agg = OBS$mat.agg
                                     , list.PFG = list.PFG
                                     , list.habitat = mat.hab$code.habitat
                                     , list.strata = list.strata)
      colnames(distrib.obs)[which(colnames(distrib.obs) == "quantile.val")] = "quantile.obs"
    }
    
    if (doHabitat) {
      cat("\n ----------- TRAIN A RANDOM FOREST MODEL ON OBSERVED DATA")
      #separate the database into a training and a test part
      cat("\n > Separate the database into a training and a test part \n")
      set.seed(RF.seed)
      
      mat.cast = OBS$mat.cast
      mat.cast$habitat = as.factor(mat.cast$habitat)
      freq = table(mat.cast$code.habitat) / nrow(mat.cast)
      no.hab = sample(names(freq), size = RF.training * nrow(mat.cast), prob = freq, replace = TRUE)
      no.hab = table(no.hab)
      if (length(no.hab) != length(freq)) {
        stop("PROBLEM") ## TODO
      }
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
      
      #train the model (with correction for imbalances in sampling)
      #run optimization algo (careful : optimization over OOB...)
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
      #select mtry
      mtry.perf = as.data.frame(mtry.perf)
      mtry = mtry.perf$mtry[which.min(mtry.perf$OOBError)]  #the lowest n achieving minimum OOB
      
      #run real model
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
      
      #analyse model performance
      aggregate.TSS.training = .valid_getModelPerf(dataset = "train"
                                                   , mod.pred = RF.model$predicted
                                                   , mod.ref = tab.train$habitat)
      
      aggregate.TSS.testing = .valid_getModelPerf(dataset = "test"
                                                  , mod.pred = RF.model$test$predicted
                                                  , mod.ref = tab.test$habitat)
      cat("\n > Done ! \n")
    }
  }
  
  
  #############################################################################
  
  cat("\n ----------- SIMULATED DATA")
  
  if (opt.no_CPU > 1)
  {
    if (.getOS() != "windows")
    {
      registerDoParallel(cores = opt.no_CPU)
    } else
    {
      warning("Parallelisation with `foreach` is not available for Windows. Sorry.")
    }
  }
  results.simul = foreach (simul = infos.simul) %do%
    {
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
      
      # 3. Keep only releve on interesting habitat, strata and PFG
      # mat.sim.agg = mat.sim.agg[which(mat.sim.agg$strata %in% list.strata &
      #                                   mat.sim.agg$PFG %in% opt.keep_PFG), ]
      
      if (doRichness == TRUE) {
        rich.sim = unique(mat.sim$PFG)
        rich.sim = rich.sim[which(rich.sim %in% list.PFG)]
      }
      
      SIM = .valid_organizeData(mat = mat.sim
                                , fac.agg = c("pixel", "strata", "PFG")
                                , fac.rel = c("pixel", "strata")
                                , fac.cast = "pixel"
                                , mat.sites = sites.sim)
      
      if (doComposition == TRUE){ # Only for PFG composition validation
        
        cat("\n ------ PFG COMPOSITION VALIDATION")
        cat("\n > Get simulated distribution...")
        distrib.sim = .valid_getDistrib(mat.agg = SIM$mat.agg
                                       , list.PFG = list.PFG
                                       , list.habitat = mat.hab$code.habitat
                                       , list.strata = list.strata)
        colnames(distrib.sim)[which(colnames(distrib.sim) == "quantile.val")] = "quantile.sim"
        
        #######################################
        # 9. Compute proximity between observed and simulated data, per PFG*strata*habitat
        
        distrib.ALL <- merge(distrib.obs, distrib.sim, by = c("PFG", "code.habitat", "strata", "quantile.perc"), all = TRUE)
        
        #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
        #return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
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
        
        # 10. Aggregate results for the different PFG
        aggregated.proximity = split(proximity, list(proximity$code.habitat, proximity$strata), drop = TRUE)
        aggregated.proximity = foreach(tmp = aggregated.proximity, .combine = "rbind") %do%
          {
            return(data.frame(simul = simul$dir.save
                              , code.habitat = unique(tmp$code.habitat)
                              , strata = unique(tmp$strata)
                              , aggregated.proximity = mean(tmp$proximity)))
          }
        performance.composition <- list(aggregated.proximity = aggregated.proximity)
        cat("\n > Done ! \n")
        
      }
      
      
      if (doHabitat == TRUE){ # Only for habitat validation
        cat("\n ------ HABITAT PREDICTION")
        
        mat.cast.sim = SIM$mat.cast
        mat.cast.sim$habitat <- factor(mat.cast.sim$habitat, levels = RF.model$classes)
        #thanks to the "levels" argument, we have the same order for the habitat factor in the RF model and in the FATE outputs
        
        RF.predictors <- rownames(RF.model$importance)
        if (length(setdiff(RF.predictors, colnames(mat.cast.sim))) > 0) {
          stop("PROBLEM") ##TODO
        }
        RF.pred <- predict(object = RF.model, newdata = mat.cast.sim[, RF.predictors], type = "response", norm.votes = TRUE)

        #analyse model performance
        aggregate.TSS.validation = .valid_getModelPerf(dataset = "valid"
                                                       , mod.pred = RF.pred
                                                       , mod.ref = factor(mat.cast.sim$habitat, RF.model$classes))
          
        RF.perf = do.call(rbind, list(aggregate.TSS.training, aggregate.TSS.testing, aggregate.TSS.validation))
        
        
        ########################
        # IV. Predict habitat for the whole map if option selected (do it only for a small number of simulations)
        ############################################
        
        if (doHabitat.allMap == TRUE) {
          col.df = data.frame(habitat.obs = RF.model$classes,
                              failure = hcl.colors(n = length(RF.model$classes), palette = "Roma", alpha = 0.5),
                              success = hcl.colors(n = length(RF.model$classes), palette = "Roma", alpha = 1))
          col.df = melt(col.df, id.vars = c("habitat.obs"), variable.name = "fail_succ", value.name = "color")
          
          col_label = paste0(col.df$habitat.obs, " - ", col.df$fail_succ)
          names(col_label) = col.df$color
          
          #true/false prediction
          pred.allMap = data.frame(pixel = mat.cast.sim$pixel
                                   , habitat.obs = as.character(mat.cast.sim$habitat)
                                   , habitat.sim = as.character(RF.pred))
          pred.allMap$habitat.final = pred.allMap$habitat.sim
          pred.allMap$habitat.final[which(pred.allMap$habitat.obs != pred.allMap$habitat.sim)] = "failure"
          pred.allMap$fail_succ = ifelse(pred.allMap$habitat.final == "failure", "failure", "success")
          pred.allMap = merge(pred.allMap, col.df, by.x = c("habitat.obs", "fail_succ"))
          pred.allMap = merge(pred.allMap, sites.sim, by.x = c("pixel", "habitat.obs"), by.y = c("pixel", "habitat"), all.x = TRUE)
          pred.allMap$code.habitat[which(pred.allMap$habitat.final == "failure")] = -1
          
          ## Raster map
          prediction.map = ras_simulation
          prediction.map[] = -1
          prediction.map[pred.allMap$pixel] = pred.allMap$code.habitat
          prediction.map = ratify(prediction.map)
          map.rat = unique(pred.allMap[, c("code.habitat", "habitat.final")])
          colnames(map.rat) = c("ID", "habitat")
          levels(prediction.map) = map.rat[order(map.rat$ID), ]
          
          ## ggplot
          ggplot(pred.allMap, aes(x = x, y = y, fill = factor(color, levels(factor(col.df$color))))) +
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
        } 
        # output.validation <- c(synthesis.validation$TSS, aggregate.TSS.validation)
        # names(output.validation) <- c(synthesis.validation$habitat, "aggregated")
        results.habitat <- list(output.validation = aggregate.TSS.validation)
        if(doHabitat.allMap == TRUE){
          results.habitat$pred.allMap = pred.allMap
        }
      }

      
      results = list(simul = basename(GLOB_DIR$dir.save))
      if (doHabitat) {
        results$RF.model = RF.model
        results$habitat.performance = results.habitat$output.validation
      }
      if (doHabitat.allMap) {
        results$habitat.prediction = results.habitat$pred.allMap
      }
      if (doComposition) {
        results$performance.compo = performance.composition
      }
      if (doRichness) {
        results$dying.PFG.list = setdiff(list.PFG, rich.sim)
      }
      return(results)
    } # End of loop on simulations
  cat("\n ----------- END OF LOOP ON SIMULATIONS \n")
  
  
  #############################################################################
  
  if(doRichness == TRUE){ # PFG Richness validation
    
    output.path = paste0(name.simulation, "/VALIDATION/PFG_RICHNESS")
    dying.PFG.list = list()
    for(i in 1:length(all_of(simulations))){
      dying.PFG.list[[i]] = results.simul[[i]]$dying.PFG.list
    }
    
    cat("\n ----------- RICHNESS COMPUTATION \n")
    
    # names the results
    names(dying.PFG.list) = simulations
    
    # get table with PFG richness
    PFG.richness.df = data.frame(simulation = names(dying.PFG.list), richness = length(list.PFG) - unlist(lapply(dying.PFG.list, FUN = "length")))
    
    # get vector with one occurence per PFG*simulation with dying of the PFG, as factor with completed levels in order to have table with all PFG, including those which never die
    dyingPFG.vector = as.factor(unlist(dying.PFG.list))
    dyingPFG.vector = fct_expand(dyingPFG.vector, list.PFG)
    dying.distribution = round(table(dyingPFG.vector)/length(simulations), digits = 2)
    dying.distribution = as.data.frame(dying.distribution)
    
    # output
    output = list(PFG.richness.df, dying.distribution , dying.PFG.list)
    names(output) = c("PFG.richness.df", "dying.distribution", "dying.PFG.list")
    
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
    
    fwrite(PFG.richness.df, paste0(output.path, "/performance.richness.csv"), row.names = F)
    fwrite(dying.distribution, paste0(output.path, "/PFG.extinction.frequency.csv"), row.names = F)
    write_rds(dying.PFG.list, file = paste0(output.path, "/dying.PFG.list.rds"), compress = "none")
    
    cat("\n > PFG richness results saved \n")
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # RESULTS : ")
  cat("\n #------------------------------------------------------------# \n")
  
  if(doRichness == TRUE){
    cat("\n ---------- PFG RICHNESS : \n")
    rich = as.matrix(output[[1]])
    rownames(rich) = seq(1, length(abs.simulParams), 1)
    rich[1:length(simulations),1] = simulations
    cat(paste0("\n Richness at year ", year, " : \n"))
    print(rich)
  } else{ 
    cat("\n ---------- PFG RICHNESS VALIDATION DISABLED \n")
  }
  
  if(doComposition == TRUE){
    cat("\n ---------- PFG COMPOSITION : \n")
    print(results.compo[simulations])
  } else{
    cat("\n ---------- PFG COMPOSITION VALIDATION DISABLED \n")
  }
  
  if(doHabitat == TRUE & doHabitat.allMap == TRUE){
    
    hab.pred = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/hab.pred.csv")))
    failure = as.numeric((table(hab.pred$fail_succ)[1]/sum(table(hab.pred$fail_succ)))*100)
    success = as.numeric((table(hab.pred$fail_succ)[2]/sum(table(hab.pred$fail_succ)))*100)
    
    testing = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_testing.csv")))
    training = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_training.csv")))
    hab.perf = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/performance.habitat.csv")))
    hab.perf = as.data.frame(t(hab.perf))
    colnames(hab.perf) = hab.perf["simulation",]
    performances = testing[,c("habitat","TSS")]
    colnames(performances) = c("habitat", "TSS_testing_part")
    performances$TSS_training_part = training$TSS
    performances = cbind(performances, hab.perf[1:length(new.mat.hab[,1]),])
    colnames(performances) = c("habitat", "TSS_testing_part", "TSS_training_part", simulations)
    rownames(performances) = seq(1, length(mat.hab[,1]), 1)
    
    cat("\n ---------- HABITAT : \n")
    cat(paste0("\n", round(failure, digits = 2), "% of habitats are not correctly predicted by the simulations \n"))
    cat(paste0("\n", round(success, digits = 2), "% of habitats are correctly predicted by the simulations \n"))
    cat("\n Habitat performance : \n")
    print(performances)
    return(prediction.map)
    
  } else if (doHabitat == TRUE & doHabitat.allMap == FALSE){
    
    testing = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_testing.csv")))
    training = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_training.csv")))
    hab.perf = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/performance.habitat.csv")))
    hab.perf = as.data.frame(t(hab.perf))
    colnames(hab.perf) = hab.perf["simulation",]
    performances = testing[,c("habitat","TSS")]
    colnames(performances) = c("habitat", "TSS_testing_part")
    performances$TSS_training_part = training$TSS
    performances = cbind(performances, hab.perf[1:length(new.mat.hab[,1]),])
    colnames(performances) = c("habitat", "TSS_testing_part", "TSS_training_part", simulations)
    rownames(performances) = seq(1, length(mat.hab[,1]), 1)
    
    cat("\n ---------- HABITAT : \n")
    cat("\n Habitat performance : \n")
    return(performances)
    
  } else{
    cat("\n ---------- HABITAT VALIDATION DISABLED \n")
  }
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
  # cat("\n > Releves data have been transformed into a relative metric")
  
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
