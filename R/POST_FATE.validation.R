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
                              , abs.simulParam = abs.simulParam[1])
  
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
      return(list(dir.save = basename(GLOB_DIR$dir.save)
                  , file.abund = paste0(name.simulation
                                        , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                                        , ifelse(length(list.strata) == 1 && list.strata == "all", "", "perStrata_")
                                        , basename(GLOB_DIR$dir.save)
                                        , ".csv")))
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
  
  #############################################################################
  
  
  if (doHabitat == TRUE | doComposition == TRUE) {
    
    #################################################################
    # I.2 Train a RF model on observed data (habitat validation only)
    #################################################################
    
    # 3. Keep only releve on interesting habitat, strata and PFG
    ##################################################################"
    mat.PFG.agg <- as.data.frame(mat.obs[, which(colnames(mat.obs) %in% c("site", "code.habitat", "strata", "PFG", "abund"))])
    mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$code.habitat %in% mat.hab$code.habitat &
                                      mat.PFG.agg$strata %in% list.strata &
                                      mat.PFG.agg$PFG %in% opt.keep_PFG), ]

    ## Compute sum of abundance per PFG (and per strata and per code.habitat if provided)
    mat.PFG.agg = aggregate(abund ~ site + PFG + strata + code.habitat, data = mat.PFG.agg, FUN = "sum")

    ## Compute relative abundance per site and strata (might be all if not provided)
    mat.PFG.agg = as.data.frame(
      mat.PFG.agg %>% group_by(site, strata) %>%
        mutate(relative.metric = round(prop.table(abund), digits = 2))
    )

    ## Remove NA and abund column
    if (length(which(is.na(mat.PFG.agg$relative.metric)))) {
      mat.PFG.agg$relative.metric[which(is.na(mat.PFG.agg$relative.metric))] = 0
    }
    mat.PFG.agg$abund = NULL
    cat("\n > Releves data have been transformed into a relative metric")

    # write.csv(mat.PFG.agg, paste0(output.path,"/HABITAT/obs.releves.prepared.csv"),row.names = FALSE)
    
    if (doComposition) {
      ## GET OBSERVED DISTRIBUTION
      # output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
      cat("\n > Get observed distribution...")
      
      distrib = split(mat.PFG.agg, list(mat.PFG.agg$PFG, mat.PFG.agg$code.habitat, mat.PFG.agg$strata), drop = TRUE)
      distrib = foreach(tmp = distrib, .combine = "rbind") %do%
        {
          qt = quantile(tmp$relative.metric, probs = seq(0, 1, 0.25))
          return(data.frame(PFG = unique(tmp$PFG)
                            , code.habitat = unique(tmp$code.habitat)
                            , strata = unique(tmp$strata)
                            , quantile.perc = seq(0, 1, 0.25)
                            , quantile.obs = as.vector(qt)))
        }
      
      # 7. Add the missing PFG*habitat*strata
      #final distribution is the distribution once the missing combination have been added. For these combination, all quantiles are set to 0
      
      all.distrib <- expand.grid(PFG = list.PFG,                              
                                 code.habitat = studied.habitat$code.habitat,
                                 strata = list.strata, 
                                 quantile.perc = seq(0, 1, 0.25),
                                 stringsAsFactors = FALSE)
      all.distrib <- merge(all.distrib, distrib, by = c("PFG", "code.habitat", "strata", "quantile.perc"), all.x = TRUE)
      all.distrib$quantile.obs[is.na(all.distrib$quantile.obs)] <- 0
      all.distrib = all.distrib[order(all.distrib$code.habitat, all.distrib$strata, all.distrib$PFG, all.distrib$quantile.perc), ]
      
      write.csv(all.distrib, paste0(output.path, "/all.distrib.csv"), row.names = F)
      obs.distri = all.distrib
      
      # obs.distri = get_observed_distribution(mat.obs = mat.PFG.agg
      #                                        , ras_habitat.compo = ras_habitat
      #                                        , mat.hab = mat.hab
      #                                        , list.PFG = list.PFG
      #                                        , list.strata = list.strata
      #                                        , output.path = output.path.compo)
    }
    
    
    #############
    
    if (doHabitat) {
      
      cat("\n ----------- TRAIN A RANDOM FOREST MODEL ON OBSERVED DATA")
      
      
      # 2. Cast the df
      ################
      
      ## Get site and habitat informations
      infos = unique(mat.obs[, which(colnames(mat.obs) %in% c("site", "x", "y", "code.habitat"))])
      infos = merge(infos, mat.hab, by = "code.habitat")
      infos = infos[, c("site", "x", "y", "code.habitat", "habitat")]
      
      #transfo into factor to be sure to create all the combination when doing "dcast"
      tab2 = mat.PFG.agg
      tab2$PFG = as.factor(tab2$PFG)
      tab2$strata = as.factor(tab2$strata)
      tab2 = reshape2::dcast(tab2, site ~ PFG + strata, value.var = "relative.metric", fill = 0, drop = FALSE)
      tab3 = merge(infos, tab2, by = "site")
      
      # write.csv(mat.PFG.agg, paste0(output.path,"/HABITAT/obs.releves.prepared.csv"),row.names = FALSE)
      
      
      ## TRAIN A RF ON OBSERVED DATA
      RF.param = list(share.training = RF.training, ntree = 500)
      RF.train = train_RF_habitat(mat.obs = tab3
                                  , ras_habitat.RF = ras_habitat
                                  , mat.hab = mat.hab
                                  , RF.param = RF.param
                                  , output.path = output.path
                                  , seed = RF.seed)
      RF.model = RF.train$RF
      new.mat.hab = RF.train$habitat
      cat("\n > Done ! \n")
    } else {
      new.mat.hab = mat.hab
    }
    
    #######################################
    # II. Prepare database for FATE habitat
    #######################################
    
    # habitat df for the whole simulation area
    hab.whole.df = as.data.frame(rasterToPoints(ras_habitat))
    colnames(hab.whole.df) = c("x", "y", "code.habitat")
    # hab.whole.df$pixel = cellFromXY(ras_habitat, hab.whole.df[, c("x", "y")])
    hab.whole.df$pixel = cellFromXY(ras_simulation, hab.whole.df[, c("x", "y")])
    if (!is.null(opt.ras_validation)) {
      toKeep = opt.ras_validation[cellFromXY(opt.ras_validation, hab.whole.df[, c("x", "y")])]
    } else {
      toKeep = ras_simulation[cellFromXY(ras_simulation, hab.whole.df[, c("x", "y")])]
    }
    hab.whole.df = hab.whole.df[which(toKeep == 1), ]
    if (nrow(hab.whole.df)) {
      stop("PROBLEM") ## TODO
    }
    hab.whole.df <- merge(hab.whole.df, new.mat.hab, by = "code.habitat")
    
    cat("\n > Habitat considered in the prediction exercise : ", paste0(unique(hab.whole.df$habitat), collapse = " / "), "\n")
  }
  
  
  #############################################################################
  
  cat("\n ----------- PROCESSING LOOP ON SIMULATIONS")
  
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
      colnames(mat.sim) = c("pixel", "x", "y", "strata", "PFG", "abs")
      if (!is.null(opt.keep_strata)) {
        new.strata <- rep("all", nrow(mat.sim))
        for (i in 1:length(opt.keep_strata)) {
          ind = which(mat.sim$strata %in% opt.keep_strata[[i]])
          new.strata[ind] = names(opt.keep_strata)[i]
        }
        mat.sim$strata = new.strata
      }
      
      if (doRichness == TRUE) {
        PFG.richness.simulated = unique(mat.sim$PFG)
        PFG.richness.simulated = PFG.richness.simulated[which(PFG.richness.simulated %in% list.PFG)]
      }
      
      
      
      if (doHabitat == TRUE){ # Only for habitat validation
        cat("\n ------ HABITAT PREDICTION")
        
        # 3. Keep only releve on interesting habitat, strata and PFG
        ##################################################################"
        mat.sim.agg <- as.data.frame(mat.sim[, which(colnames(mat.sim) %in% c("pixel", "strata", "PFG", "abs"))])
        mat.sim.agg = mat.sim.agg[which(mat.sim.agg$strata %in% list.strata &
                                          mat.sim.agg$PFG %in% opt.keep_PFG), ]
        
        ## Compute sum of abundance per PFG (and per strata and per code.habitat if provided)
        mat.sim.agg = aggregate(abs ~ pixel + PFG + strata, data = mat.sim.agg, FUN = "sum")
        
        ## Compute relative abundance per site and strata (might be all if not provided)
        mat.sim.agg = as.data.frame(
          mat.sim.agg %>% group_by(pixel, strata) %>% 
            mutate(relative.metric = round(prop.table(abs), digits = 2))
        )
        
        ## Remove NA and abs column
        if (length(which(is.na(mat.sim.agg$relative.metric)))) {
          mat.sim.agg$relative.metric[which(is.na(mat.sim.agg$relative.metric))] = 0
        }
        mat.sim.agg$abs = NULL
        cat("\n > Simulated data have been transformed into a relative metric")
        
        #transfo into factor to be sure to create all the combination when doing "dcast"
        #correct the levels (to have all PFG and all strata) to make the dcast transfo easier 
        #(all PFG*strata combination will be automatically created thanks to the factor structure, even if no line corresponds to it)
        tab2 = mat.sim.agg
        tab2$PFG <- as.factor(tab2$PFG)
        tab2$strata <- as.factor(tab2$strata)
        tab3 <- reshape2::dcast(tab2, pixel ~ PFG * strata, value.var = "relative.metric", fill = 0, drop = FALSE)
        
        # ## USE THE RF MODEL TO VALIDATE FATE OUTPUT
        
        #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
        data.FATE.PFG.habitat <- merge(tab3, hab.whole.df, by = "pixel") #at this stage we have all the pixels in the simulation area
        data.FATE.PFG.habitat$habitat <- factor(data.FATE.PFG.habitat$habitat, levels = RF.model$classes) #thanks to the "levels" argument, we have the same order for the habitat factor in the RF model and in the FATE outputs
        
        ############################
        # II. Prediction of habitat with the RF algorithm
        #################################
        
        data.validation <- data.FATE.PFG.habitat #[which(data.FATE.PFG.habitat$for.validation == 1), ]
        x.validation <- dplyr::select(data.validation,all_of(RF.predictors))
        y.validation <- data.validation$habitat
        y.validation.predicted <- predict(object = RF.model, newdata = x.validation, type = "response", norm.votes = TRUE)
        
        ##############################
        # III. Analysis of the results
        ################################
        #analyse model performance
        conf.valid = confusionMatrix(data = y.validation.predicted, reference = factor(y.validation, sort(unique(c(levels(y.validation), levels(y.validation.predicted))))))
        
        synthesis = foreach(ii = c("valid"), .combine = "rbind") %do%
          {
            tab = get(paste0("conf.", ii))
            synth = data.frame(dataset = ii
                               , habitat = colnames(tab$table)
                               , sensitivity = tab$byClass[, 1]
                               , specificity = tab$byClass[, 2]
                               , weight = colSums(tab$table) / sum(colSums(tab$table)))
            #warning: prevalence is the weight of predicted habitat, not of observed habitat
            synth$TSS = round(synth$sensitivity + synth$specificity - 1, digits = 2)
            return(synth)
          }
        aggregate.TSS.validation = round(sum(synthesis$weight[which(synthesis$dataset == "valid")] * 
                                               synthesis$TSS[which(synthesis$dataset == "valid")]), digits = 2)
        
        ########################
        # IV. Predict habitat for the whole map if option selected (do it only for a small number of simulations)
        ############################################
        
        if (predict.all.map == TRUE) {
          y.all.map.predicted = predict(object = RF.model, newdata =  data.FATE.PFG.habitat[, RF.predictors], type = "response", norm.votes = TRUE)
          y.all.map.predicted = as.data.frame(y.all.map.predicted)
          y.all.map.predicted$pixel = data.FATE.PFG.habitat$pixel
          y.all.map.predicted$habitat = data.FATE.PFG.habitat$habitat
          colnames(y.all.map.predicted) = c(sim, "pixel", "habitat")
        } else {
          y.all.map.predicted <- NULL
        }
        
        #prepare outputs
        output.validation <- c(synthesis.validation$TSS, aggregate.TSS.validation)
        names(output.validation) <- c(synthesis.validation$habitat, "aggregated")
        results.habitat <- list(output.validation = output.validation)
        
        if(predict.all.map == TRUE){
          results.habitat$y.all.map.predicted = y.all.map.predicted
        }
      }
      
      if (doComposition == TRUE){ # Only for PFG composition validation
        
        cat("\n ------ PFG COMPOSITION VALIDATION")
        
        
        ## DO PFG COMPOSITION VALIDATION
        cat("\n > Comparison between observed and simulated distribution... \n")
        mat.sim.agg <- merge(mat.sim, hab.whole.df, by = c("pixel", "x", "y")) #at this stage we have all the pixels in the simulation area
        # mat.sim.agg <- as.data.frame(mat.sim[, which(colnames(mat.sim) %in% c("pixel", "strata", "PFG", "abs"))])
        mat.sim.agg = mat.sim.agg[which(mat.sim.agg$strata %in% list.strata &
                                          mat.sim.agg$PFG %in% opt.keep_PFG), ]
        
        ## Compute relative abundance per site and strata (might be all if not provided)
        mat.sim.agg = as.data.frame(
          mat.sim.agg %>% group_by(pixel, strata) %>% 
            mutate(relative.metric = round(prop.table(abs), digits = 2))
        )
        
        ## Remove NA and abs column
        if (length(which(is.na(mat.sim.agg$relative.metric)))) {
          mat.sim.agg$relative.metric[which(is.na(mat.sim.agg$relative.metric))] = 0
        }
        mat.sim.agg$abs = NULL
        cat("\n > Simulated data have been transformed into a relative metric")
        
        cat("\n > Get simulated distribution...")
        
        distrib = split(mat.sim.agg, list(mat.sim.agg$PFG, mat.sim.agg$code.habitat, mat.sim.agg$strata), drop = TRUE)
        distrib = foreach(tmp = distrib, .combine = "rbind") %do%
          {
            qt = quantile(tmp$relative.metric, probs = seq(0, 1, 0.25))
            return(data.frame(PFG = unique(tmp$PFG)
                              , code.habitat = unique(tmp$code.habitat)
                              , strata = unique(tmp$strata)
                              , quantile.perc = seq(0, 1, 0.25)
                              , quantile.sim = as.vector(qt)))
          }
        
        all.distrib <- expand.grid(PFG = list.PFG,                              
                                   code.habitat = studied.habitat$code.habitat,
                                   strata = list.strata, 
                                   quantile.perc = seq(0, 1, 0.25),
                                   stringsAsFactors = FALSE)
        all.distrib <- merge(all.distrib, distrib, by = c("PFG", "code.habitat", "strata", "quantile.perc"), all.x = TRUE)
        all.distrib$quantile.obs[is.na(all.distrib$quantile.obs)] <- 0
        all.distrib = all.distrib[order(all.distrib$code.habitat, all.distrib$strata, all.distrib$PFG, all.distrib$quantile.perc), ]
        
        write.csv(all.distrib, paste0(output.path, "/all.distrib.csv"), row.names = F)
        sim.distri = all.distrib
        
        
        # # "if" to check that observed and simulated databases are in the same order
        # if(
        #   !(
        #     all(simulated.distribution$PFG == observed.distribution$PFG)&
        #     all(simulated.distribution$habitat == observed.distribution$habitat)&
        #     all(simulated.distribution$strata == observed.distribution$strata)&
        #     all(simulated.distribution$rank == observed.distribution$rank)
        #   )
        # ){
        #   stop("Problem in observed vs simulated database (problem in the PFG*strata*habitat considered or in the database order)")
        # }
        
        # 8. Merge observed and simulated data
        #######################################
        
        ALL.distrib <- merge(obs.distri, sim.distri, by = c("PFG", "code.habitat", "strata", "quantile.perc"))
        # cbind(simulated.distribution, observed.quantile = observed.distribution$observed.quantile) #quicker than a merge, but we can do it only because we have worked on the order of the DT
        
        # 9. Compute proximity between observed and simulated data, per PFG*strata*habitat
        ###################################################################################
        
        #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
        #for a given PFG*habitat*strata, return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
        compute.proximity <- function(sim.qt, obs.qt) {
          return(1 - sum(abs(sim.qt - obs.qt)) / 4)
        }
        
        #we get rid off rank==0 because there is good chance that it is nearly always equal to zero both in observed and simulated data, and that would provide a favorable bias in the results
        
        ALL.distrib = ALL.distrib[which(ALL.distrib$quantile.perc > 0), ]
        # simulated.distribution <- filter(simulated.distribution, rank != 0)
        
        proximity = split(ALL.distrib, list(ALL.distrib$PFG, ALL.distrib$code.habitat, ALL.distrib$strata), drop = TRUE)
        proximity = foreach(tmp = proximity, .combine = "rbind") %do%
          {
            qt = compute.proximity(sim.qt = tmp$quantile.sim, obs.qt = tmp$quantile.obs)
            return(data.frame(PFG = unique(tmp$PFG)
                              , code.habitat = unique(tmp$code.habitat)
                              , strata = unique(tmp$strata)
                              , proximity = qt))
          }
        proximity = proximity[order(proximity$code.habitat, proximity$strata, proximity$PFG), ]
        
        
        # 10. Aggregate results for the different PFG
        ##############################################
        
        ## TODO
        # aggregated.proximity <- proximity[,mean(proximity), by = c("habitat", "strata")]
        # aggregated.proximity <- rename(aggregated.proximity, "aggregated.proximity" = "V1")
        # aggregated.proximity$aggregated.proximity <- round(aggregated.proximity$aggregated.proximity, digits = 2)
        # aggregated.proximity$simul <- sim
        
        performance.composition <- list(aggregated.proximity = aggregated.proximity)
        
        cat("\n > Done ! \n")
        
      }
      
      results = list(simul = basename(GLOB_DIR$dir.save))
      if (doHabitat) {
        results$RF.model = RF.model
        results$habitat.performance = results.habitat$output.validation
      }
      if (doHabitat.allMap) {
        results$habitat.prediction = results.habitat$y.all.map.predicted
      }
      if (doComposition) {
        results$performance.compo = performance.composition
      }
      if (doRichness) {
        results$dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated)
      }
      return(results)
    } # End of loop on simulations
  cat("\n ----------- END OF LOOP ON SIMULATIONS \n")
  
  
  #############################################################################
  
  if (doHabitat == TRUE) { # If habitat validation activated, the function uses the results to build and save a final map of habitat prediction
    output.path = paste0(name.simulation, "/VALIDATION")
    
    # deal with the results regarding model performance
    habitat.performance = foreach(res = results.simul, .combine = "rbind") %do% {
      tmp = t(res$habitat.performance)
      colnames(tmp) <- c(RF.model$classes, "weighted")
      tmp$simulation <- res$simul
      return(tmp)
    }
    fwrite(habitat.performance, paste0(output.path, "/HABITAT/performance.habitat.csv"), row.names = FALSE)
    cat("\n > Habitat performance saved")
    
    if (doHabitat.allMap) {
      # deal with the results regarding habitat prediction over the whole map
      all.map.prediction = foreach(res = results.simul, .combine = "rbind") %do% {
        tmp = res$habitat.prediction
        tmp$simulation <- res$simul
        return(tmp)
      }
      # all.map.prediction = as.data.frame(lapply(results.simul, "[[", 1))
      all.map.prediction = all.map.prediction[, c(simulations, "pixel", "habitat")]
      all.map.prediction = rename(all.map.prediction, "true.habitat" = "habitat")
      fwrite(all.map.prediction, paste0(output.path,"/HABITAT/habitat.prediction.csv"), row.names = FALSE)
      cat("\n > Habitat prediction saved")
      
      ## AGGREGATE HABITAT PREDICTION AND PLOT PREDICTED HABITAT
      # Provide a color df
      col.df = data.frame(
        habitat = RF.model$classes,
        failure = rainbow(length(RF.model$classes), alpha = 0.5),
        success = rainbow(length(RF.model$classes), alpha = 1))
      
      prediction.map = plot_predicted_habitat(predicted.habitat = all.map.prediction
                                              , col.df = col.df
                                              , ras_simulation = ras_simulation
                                              , output.path = output.path
                                              , sim.version = simulations)
      
      cat("\n > Predicted habitat plot saved")
    }
  }
  
  if(doComposition == TRUE){ # If PFG composition validation activated, the function uses the results to save a table with proximity of PFG composition for each PFG and habitat*strata define by the user
    
    output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
    
    # results.compo = foreach(res = results.simul, .combine = "rbind") %do% {
    #   tmp = t(res$performance.compo)
    #   # colnames(tmp) <- c(RF.model$classes, "weighted")
    #   # tmp$simulation <- res$simul
    #   return(tmp)
    # }
    # fwrite(results.compo, paste0(output.path.compo, "/performance.composition.csv"), row.names = FALSE)
    # cat("\n > Performance composition file saved \n")
    
    
    results.compo = sapply(results.simul, "[[", "performance.compo")
    results <- sapply(results.compo, function(X){X$aggregated.proximity})
    rownames(results) <- paste0(results.compo[[1]]$habitat, "_", results.compo[[1]]$strata)
    # colnames(results) <- abs.simulParams
    colnames(results) <- simulations
    results.compo <- t(results)
    results.compo <- as.data.frame(results)
    results.compo$simulation <- rownames(results)
    
    #save and return
    # fwrite(results.compo, paste0(output.path.compo, "/performance.composition.csv"), row.names = FALSE)
    # cat("\n > Performance composition file saved \n")
    
  }
  
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
    failure = as.numeric((table(hab.pred$prediction.code)[1]/sum(table(hab.pred$prediction.code)))*100)
    success = as.numeric((table(hab.pred$prediction.code)[2]/sum(table(hab.pred$prediction.code)))*100)
    
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
