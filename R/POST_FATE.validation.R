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
##' @param releves.PFG a \code{data.frame} with at least 5 columns : \cr
##' \code{site}, \code{x}, \code{y}, \code{abund}, \code{PFG}
##' \cr (\emph{and optionally, \code{strata}, \code{code.habitat}})
##' \cr (see \href{POST_FATE.validation#details}{\code{Details}}).
##' @param hab.obs a \code{raster} map of the habitats within the studied area, with same projection 
##' & resolution than simulation mask.
##' @param studied.habitat a \code{data.frame} with 2 columns : \cr
##' \code{ID} ,\code{habitat}
##' \cr (see \href{POST_FATE.validation#details}{\code{Details}}).
##' @param predict.all.map (\code{logical}) default \code{FALSE}. \cr If \code{TRUE}, the function will compute habitat prediction 
##' & performance over the whole map and will provide a prediction map.
##' @param RF.seed default \code{123}. \cr An \code{integer} corresponding to the number of seeds to set 
##' in order to generate a \code{Random Forest} model.
##' @param RF.training default \code{0.7}. \cr A \code{double} between 0 & 1 corresponding to the part of the data used for 
##' training a \code{Random Forest} model on \code{releves.PFG} data.
##' @param validation.mask (\code{optional}) default \code{NULL}. \cr A \code{raster} map (with 0 or 1 in each pixel) that specified on 
##' which pixels the performance of the habitat prediction will be compute, with same projection & resolution than simulation mask 
##' \cr If \code{NULL}, the function will take the simulation mask (which means that the performance will be compute over the whole map).
##' @param list.strata.simulations (\code{optional}) default \code{NULL}. \cr If \code{perStrata} = \code{TRUE}, 
##' a \code{list} which contains, for each stratum defined in \code{releves.PFG}, the correspondence with \code{FATE} strata definition. \cr
##' If \code{perStrata} = \code{FALSE}, please specify \code{NULL} value.
##' 
##' @param doComposition (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, PFG composition validation module is activated.
##' 
##' @param doRichness (\code{logical}) default \code{TRUE}. \cr If \code{TRUE}, PFG richness validation module is activated.
##' @param exclude.PFG (\code{optional}) default \code{NULL}. \cr A \code{character} vector containing the name(s) 
##' of the PFG(s) that will be excluded from the composition and richness analysis.
##' 
##' @details 
##' 
##' \describe{
##'   \item{Habitat validation}{ \cr
##'   \describe{
##'     \item{Observed habitat}{is provided by the \strong{hab.obs} map. The final set of habitats taken into account
##'     in the validation is provided by \strong{studied.habitat} table which contains 
##'     all the \strong{habitats}, with their corresponding \strong{ID}.}
##'     \item{Simulated habitat}{is determined from \code{FATE} simulated relative abundances 
##'     (see \code{\link{POST_FATE.temporalEvolution}}), thanks to a \code{Random Forest} algorithm. 
##'     The model is trained on \strong{releves.PFG} data. Information about \strong{PFG abundances} at each
##'     \strong{sites} with \strong{xy} coordinates are necessary. PFG abundances can be given in absolute abundance
##'     (only in sites where at least one PFG is present) or in presence-absence data.\cr
##'     Eventually, \strong{habitat ID} information can be provided, as well as \strong{strata} names.
##'     If not, habitat information will be taken from \strong{hab.obs} map, and PFG abundance will be considered for all strata.}
##'   }
##'   To compare observations and simulations, the function computes confusion matrix between 
##'   observations and predictions and then computes the TSS for each habitat h.
##'   \deqn{TSS_{\text{ h}} = (\frac{\text{number of prediction}_{\text{ h}}}{\text{number of observation}_{\text{ h}}} + \frac{\text{number of non-prediction}_{\text{ h}}}{\text{number of non-observation}_{\text{ h}}}) - \text{1}}
##'   The final metric used is the mean of TSS per habitat over all habitats, weighted by the share of each habitat in the observed 
##'   habitat distribution. The habitat validation also provides a visual comparison of observed and 
##'   simulated habitat on the whole studied area, if option selected.}
##'   
##'   \item{PFG composition validation}{ \cr 
##'   \describe{
##'     \item{Observed composition}{is computed for a set of PFG, habitat & strata
##'   (for all strata if strata definition is not activated) by computing 4 quartiles of the composition, based
##'   on \strong{releves.PFG} data provided. Habitats are chosen in \code{studied.habitat},
##'   PFG list is defined in the file \code{file.simulParam} and, if defined, \code{releves.PFG} strata
##'   are taken into account. Optionally, PFGs contained in \code{exclude.PFG} can be excluded from the analysis.}
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
##'   Optionally, PFGs contained in \code{exclude.PFG} can be excluded from the analysis.}
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
##' studied.habitat = data.frame(ID = c(6, 5, 7, 8)
##'                              , habitat = c("coniferous.forest"
##'                                            , "deciduous.forest"
##'                                            , "natural.grassland"
##'                                            , "woody.heatland"))
##' 
##' ## Habitat & validation maps
##' hab.observed = Champsaur_params$stk.mask$habitat
##' simulation.map = Champsaur_params$stk.mask$Champsaur
##' hab.obs = projectRaster(from = hab.observed, to = simulation.map, method = "ngb")
##' writeRaster(simulation.map, filename = "FATE_Champsaur/DATA/MASK/MASK_Champsaur.tif")
##' 
##' ## Observed data
##' releves.PFG = Champsaur_params$tab.releves
##' 
##' ## Transform observed PFG abundances into relative abundances
##' releves.PFG$abund = PRE_FATE.abundBraunBlanquet(releves.PFG$abund)/100
##' releves.PFG = aggregate(abund ~ site + PFG + strata + x + y
##'                         , data = releves.PFG, FUN = "sum")
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
##'                      , releves.PFG = releves.PFG
##'                      , hab.obs = hab.obs
##'                      , studied.habitat = studied.habitat
##'                      , list.strata.simulations = list.strata.simulations
##'                      , predict.all.map = TRUE
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
                                , perStrata = FALSE
                                , opt.no_CPU = 1
                                , doHabitat = TRUE
                                , releves.PFG
                                , hab.obs
                                , studied.habitat
                                , predict.all.map = FALSE
                                , RF.seed = 123
                                , RF.training = 0.7
                                , validation.mask = NULL
                                , list.strata.simulations = NULL
                                , doComposition = TRUE
                                , doRichness = TRUE
                                , exclude.PFG = NULL)
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
  ## CHECK parameter releves.PFG
  if (.testParam_notDf(releves.PFG))
  {
    .stopMessage_beDataframe("releves.PFG")
  } else
  {
    if (nrow(releves.PFG) == 0 || !(ncol(releves.PFG) %in% c(5, 6, 7)))
    {
      .stopMessage_numRowCol("releves.PFG", c("site", "x", "y", "PFG", "abund", "(strata)", "(code.habitat)"))
    } else
    {
      notCorrect = switch(as.character(ncol(releves.PFG))
                          , "5" = .testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund"))
                          , "6" = (.testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund", "strata")) &&
                                     .testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund", "code.habitat")))
                          , "7" = .testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund", "strata", "code.habitat"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("releves.PFG", c("site", "x", "y", "PFG", "abund", "(strata)", "(code.habitat)"))
      }
      .testParam_notNum.m("releves.PFG$site", releves.PFG$site)
      if (.testParam_notNum(releves.PFG$abund) & min(releves.PFG$abund) == 0) # absolute abundance
      {
        stop("Wrong type of data!\n absolute abundance data must be numeric and not contain 0")
      } else if (.testParam_notNum(releves.PFG$abund) & .testParam_notBetween(releves.PFG$abund, 0, 1)) # presence-absence
      {
        stop("Wrong type of data!\n presence-absence data must be numeric and between '0' & '1'")
      }
      if (sum(colnames(releves.PFG) == "strata") == 1)
      {
        if(.testParam_notNum(releves.PFG$strata) & .testParam_notChar(releves.PFG$strata))
        {
          stop("Wrong type of data!\n 'releves.PFG$strata' must contain numeric or character values")
        }
      }
      if (sum(colnames(releves.PFG) == "code.habitat") == 1)
      {
        .testParam_notNum.m("releves.PFG$code.habitat", releves.PFG$code.habitat)
      }
    }
  }
  if ((perStrata == TRUE && ncol(releves.PFG) == 7) ||
      (perStrata == FALSE && ncol(releves.PFG) == 6)) {
    hab.obs.RF = hab.obs.compo = NULL
    if (ncol(releves.PFG) == 6 && .testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund", "code.habitat"))) {
      .stopMessage_columnNames("releves.PFG", c("site", "x", "y", "PFG", "abund", "code.habitat"))
    }
  } else if ((perStrata == TRUE && ncol(releves.PFG) == 6) ||
             (perStrata == FALSE && ncol(releves.PFG) == 5)) {
    hab.obs.RF = hab.obs.compo = hab.obs
    if (ncol(releves.PFG) == 6 && .testParam_notColnames(releves.PFG, c("site", "x", "y", "PFG", "abund", "strata"))) {
      .stopMessage_columnNames("releves.PFG", c("site", "x", "y", "PFG", "abund", "strata"))
    }
  }
  
  ## CHECK perStrata parameters
  list.strata.releves = NULL
  list.strata = "all"
  if (perStrata) {
    list.strata.releves = as.character(unique(releves.PFG$strata))
    
    ## CHECK parameter list.strata.simulations
    # check if strata definition used in the RF model is the same as the one used to analyze FATE output
    list.strata = names(list.strata.simulations)
    .testParam_notInValues.m("names(list.strata.simulations)", names(list.strata.simulations), list.strata.releves)
  }
  ## CHECK doHabitat parameters
  if (doHabitat)
  {
    ## CHECK parameter studied.habitat
    if (.testParam_notDf(studied.habitat))
    {
      .stopMessage_beDataframe("studied.habitat")
    } else
    {
      if (nrow(studied.habitat) == 0 || ncol(studied.habitat) != 2)
      {
        .stopMessage_numRowCol("studied.habitat", c("ID", "habitat"))
      }
      studied.habitat$habitat = as.character(studied.habitat$habitat)
      .testParam_notNum.m("studied.habitat$ID", studied.habitat$ID)
      .testParam_notChar.m("studied.habitat$habitat", studied.habitat$habitat)
    }
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.validation")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  
  ## Get raster mask --------------------------------------------------------
  GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                , abs.simulParam = abs.simulParams[1])
  simulation.map = GLOB_MASK$ras.mask
  
  if (doHabitat == TRUE | doComposition == TRUE | doRichness == TRUE){ # Habitat or composition or both validation
    
    if(doHabitat == TRUE | doComposition == TRUE){
      
      cat("\n ----------- PRELIMINARY CHECKS")
      
      #######################
      # I. Preliminary checks
      #######################
      
      # Check hab.obs map
      .testParam_notSameRaster.m("simulation.map", simulation.map, "hab.obs", hab.obs)
      habitat.FATE.map = hab.obs
      
      # Check validation mask
      if(!is.null(validation.mask)){
        .testParam_notSameRaster.m("simulation.map", simulation.map, "validation.mask", validation.mask)
      }
      cat("\n > Done !")
      
      #################################################################
      # I.2 Train a RF model on observed data (habitat validation only)
      #################################################################
      
      if(doHabitat == TRUE){
        
        cat("\n ----------- TRAIN A RANDOM FOREST MODEL ON OBSERVED DATA")
        
        output.path = paste0(name.simulation, "/VALIDATION")
        
        ## TRAIN A RF ON OBSERVED DATA
        
        releves.PFG$coverage = releves.PFG$abund
        RF.param = list(share.training = RF.training, ntree = 500)
        RF.train = train_RF_habitat(releves.PFG = releves.PFG
                                    , hab.obs.RF = hab.obs.RF
                                    , studied.habitat = studied.habitat
                                    , RF.param = RF.param
                                    , output.path = output.path
                                    , perStrata = perStrata
                                    , seed = RF.seed)
        
        RF.model = RF.train$RF
        new.studied.habitat = RF.train$habitat
        
        cat("\n > Done ! \n")
        
      }
      
      #######################################
      # II. Prepare database for FATE habitat
      #######################################
      
      # habitat df for the whole simulation area
      if(is.null(validation.mask)){
        habitat.whole.area.df <- data.frame(pixel = seq(1, ncell(habitat.FATE.map), 1)
                                            , code.habitat = getValues(habitat.FATE.map)
                                            , for.validation = getValues(simulation.map))
      }else {
        habitat.whole.area.df <- data.frame(pixel = seq(1, ncell(habitat.FATE.map), 1)
                                            , code.habitat = getValues(habitat.FATE.map)
                                            , for.validation = getValues(validation.mask))
      }
      habitat.whole.area.df <- habitat.whole.area.df[which(getValues(simulation.map) == 1), ] # index of the pixels in the simulation area
      habitat.whole.area.df <- habitat.whole.area.df[which(!is.na(habitat.whole.area.df$for.validation)), ]
      if (doHabitat == TRUE){
        habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(new.studied.habitat,c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
        habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% new.studied.habitat$habitat), ]
      } else if (doHabitat == FALSE){
        habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(studied.habitat,c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
        habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% studied.habitat$habitat), ]
      } else {
        stop("Habitat definition in studied.habitat is not correct")
      }
      if(doComposition == TRUE){
        habitat.whole.area.df = habitat.whole.area.df[which(habitat.whole.area.df$for.validation == 1),]
      }
      
      cat("\n > Habitat considered in the prediction exercise : ", c(unique(habitat.whole.area.df$habitat)), "\n", sep = "\t")
      
      cat("\n ----------- PROCESSING LOOP ON SIMULATIONS")
      
    }
    
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
    
    
    results.simul = foreach (abs.simulParam = abs.simulParams) %do%
      {
        
        cat("\n+++++++\n")
        cat("\n  Simulation name : ", name.simulation)
        cat("\n  Simulation file : ", abs.simulParam)
        cat("\n")
        
        ## Get results directories ------------------------------------------------
        GLOB_DIR = .getGraphics_results(name.simulation  = name.simulation
                                        , abs.simulParam = abs.simulParam)
        
        ## Get number of PFGs -----------------------------------------------------
        ## Get PFG names ----------------------------------------------------------
        GLOB_SIM = .getGraphics_PFG(name.simulation  = name.simulation
                                    , abs.simulParam = abs.simulParam)
        
        list.PFG = GLOB_SIM$PFG
        if(!is.null(exclude.PFG)){
          list.PFG = setdiff(list.PFG, exclude.PFG)
        }
        
        ## Get raster mask --------------------------------------------------------
        GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                      , abs.simulParam = abs.simulParam)
        
        sim = basename(GLOB_DIR$dir.save)
        cat("\n >", sim, " :")
        cat("\n > Data preparation \n")
        # get simulated abundance per pixel*strata*PFG for pixels in the simulation area
        if (perStrata == FALSE) {
          ## Get the abundance table ------------------------------------------------
          file.abundance = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                                  , basename(GLOB_DIR$dir.save)
                                  , ".csv")
          .testParam_existFile(file.abundance)
          simu_PFG = fread(file.abundance)
          simu_PFG = as.data.frame(simu_PFG, stringsAsFactors = FALSE)
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", year)]
          colnames(simu_PFG) = c("PFG", "pixel", "abs")
          simu_PFG$strata <- "all"
          
        } else if (perStrata == TRUE) {
          ## Get the abundance table ------------------------------------------------
          file.abundance = paste0(name.simulation
                                  , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_"
                                  , basename(GLOB_DIR$dir.save)
                                  , ".csv")
          .testParam_existFile(file.abundance)
          simu_PFG = fread(file.abundance)
          simu_PFG = as.data.frame(simu_PFG, stringsAsFactors = FALSE)
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", "strata", year)]
          colnames(simu_PFG) = c("PFG", "pixel", "strata", "abs")
          new.strata <- rep(NA, nrow(simu_PFG))
          for (i in 1:length(list.strata.simulations)) {
            ind = which(simu_PFG$strata %in% list.strata.simulations[[i]])
            new.strata[ind] = names(list.strata.simulations)[i]
          }
          simu_PFG$strata = new.strata
        }
        
        if(doComposition == TRUE | doHabitat == TRUE){
          simu_PFG <- aggregate(abs ~ pixel + strata + PFG, data = simu_PFG, FUN = "sum")
        }
        
        if(doRichness == TRUE){
          PFG.richness.simulated = NULL
          for(pfg in list.PFG){
            sub = subset(simu_PFG, simu_PFG$PFG == pfg)
            if(!all(is.na(sub[,"abs"]))){
              PFG = pfg
              PFG.richness.simulated = c(PFG.richness.simulated, PFG)
            }
          }
        }
        
        if (doHabitat == TRUE){ # Only for habitat validation
          cat("\n ------ HABITAT PREDICTION")
          
          ## USE THE RF MODEL TO VALIDATE FATE OUTPUT
          predict.all.map = predict.all.map
          
          results.habitat = do_habitat_validation(output.path = output.path
                                                  , RF.model = RF.model
                                                  , predict.all.map = predict.all.map
                                                  , sim = sim
                                                  , simu_PFG = simu_PFG
                                                  , habitat.whole.area.df = habitat.whole.area.df
                                                  , list.strata = list.strata
                                                  , perStrata = perStrata)
          
          cat("\n > Done ! \n")
        }
        
        if (doComposition == TRUE){ # Only for PFG composition validation
          
          cat("\n ------ PFG COMPOSITION VALIDATION")
          output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
          
          ## GET OBSERVED DISTRIBUTION
          
          cat("\n > Get observed distribution...")
          
          if (doHabitat == TRUE){
            obs.distri = get_observed_distribution(releves.PFG = releves.PFG
                                                   , hab.obs.compo = hab.obs.compo
                                                   , studied.habitat = new.studied.habitat
                                                   , list.PFG = list.PFG
                                                   , list.strata = list.strata
                                                   , perStrata = perStrata
                                                   , output.path = output.path.compo)
          } else {
            obs.distri = get_observed_distribution(releves.PFG = releves.PFG
                                                   , hab.obs.compo = hab.obs.compo
                                                   , studied.habitat = studied.habitat
                                                   , list.PFG = list.PFG
                                                   , list.strata = list.strata
                                                   , perStrata = perStrata
                                                   , output.path = output.path.compo)
          }
          
          ## DO PFG COMPOSITION VALIDATION
          
          cat("\n > Comparison between observed and simulated distribution... \n")
          
          if (doHabitat == TRUE){
            performance.composition = do_PFG_composition_validation(sim = sim
                                                                    , list.PFG = list.PFG
                                                                    , studied.habitat = new.studied.habitat
                                                                    , list.strata = list.strata
                                                                    , observed.distribution = obs.distri
                                                                    , simu_PFG = simu_PFG
                                                                    , habitat.whole.area.df = habitat.whole.area.df)
          } else {
            performance.composition = do_PFG_composition_validation(sim = sim
                                                                    , list.PFG = list.PFG
                                                                    , studied.habitat = studied.habitat
                                                                    , list.strata = list.strata
                                                                    , observed.distribution = obs.distri
                                                                    , simu_PFG = simu_PFG
                                                                    , habitat.whole.area.df = habitat.whole.area.df)
          }
          
          cat("\n > Done ! \n")
          
        }
        
        if(doHabitat == TRUE & doComposition == TRUE & predict.all.map == TRUE & doRichness == FALSE){
          results = list(habitat.prediction = results.habitat$y.all.map.predicted, habitat.performance = results.habitat$output.validation, RF.model = RF.model, performance.compo = performance.composition)
        }
        if(doHabitat == TRUE & doComposition == TRUE & predict.all.map == FALSE & doRichness == FALSE){
          results = list(habitat.performance = results.habitat$output.validation, RF.model = RF.model, performance.compo = performance.composition)
        }
        if(doHabitat == TRUE & doComposition == FALSE & predict.all.map == TRUE & doRichness == FALSE){
          results = list(habitat.prediction = results.habitat$y.all.map.predicted, habitat.performance = results.habitat$output.validation, RF.model = RF.model)
        }
        if(doHabitat == TRUE & doComposition == FALSE & predict.all.map == FALSE & doRichness == FALSE){
          results = list(habitat.performance = results.habitat$output.validation, RF.model = RF.model)
        }
        if(doHabitat == FALSE & doComposition == TRUE & doRichness == FALSE){
          results = list(performance.compo = performance.composition)
        }
        if(doHabitat == TRUE & doComposition == TRUE & predict.all.map == TRUE & doRichness == TRUE){
          results = list(habitat.prediction = results.habitat$y.all.map.predicted, habitat.performance = results.habitat$output.validation, RF.model = RF.model, performance.compo = performance.composition, dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        }
        if(doHabitat == TRUE & doComposition == TRUE & predict.all.map == FALSE & doRichness == TRUE){
          results = list(habitat.performance = results.habitat$output.validation, RF.model = RF.model, performance.compo = performance.composition, dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        }
        if(doHabitat == TRUE & doComposition == FALSE & predict.all.map == TRUE & doRichness == TRUE){
          results = list(habitat.prediction = results.habitat$y.all.map.predicted, habitat.performance = results.habitat$output.validation, RF.model = RF.model, dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        }
        if(doHabitat == TRUE & doComposition == FALSE & predict.all.map == FALSE & doRichness == TRUE){
          results = list(habitat.performance = results.habitat$output.validation, RF.model = RF.model, dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        }
        if(doHabitat == FALSE & doComposition == TRUE & doRichness == TRUE){
          results = list(performance.compo = performance.composition, dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        }
        if(doHabitat == FALSE & doComposition == FALSE & doRichness == TRUE){
          results = list(dying.PFG.list = setdiff(list.PFG, PFG.richness.simulated))
        } # Based on choice of the user, foreach loop returns different results
        
        return(results)
        # results.simul = results
        
      } # End of loop on simulations
    cat("\n ----------- END OF LOOP ON SIMULATIONS \n")
    
    simulations = NULL
    for(simul in abs.simulParams){
      GLOB_DIR = .getGraphics_results(name.simulation  = name.simulation
                                      , abs.simulParam = simul)
      s = basename(GLOB_DIR$dir.save)
      simulations = c(simulations, s)
    }
    
    if(doHabitat == TRUE){ # If habitat validation activated, the function uses the results to build and save a final map of habitat prediction
      
      if(predict.all.map == TRUE){
        
        # deal with the results regarding model performance
        output.path = paste0(name.simulation, "/VALIDATION")
        RF.model = results.simul[[1]]$RF.model
        habitat.performance <- as.data.frame(matrix(unlist(lapply(results.simul,"[[", 2)), ncol = length(RF.model$classes) + 1, byrow = TRUE))
        colnames(habitat.performance) <- c(RF.model$classes, "weighted")
        # habitat.performance$simulation <- abs.simulParams
        habitat.performance$simulation <- simulations
        
        # save
        fwrite(habitat.performance, paste0(output.path, "/HABITAT/performance.habitat.csv"), row.names = FALSE)
        cat("\n > Habitat performance saved")
        
        # deal with the results regarding habitat prediction over the whole map
        all.map.prediction = as.data.frame(lapply(results.simul, "[[", 1))
        all.map.prediction = all.map.prediction[, c(simulations, "pixel", "habitat")]
        all.map.prediction = rename(all.map.prediction, "true.habitat" = "habitat")
        # save
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
                                                , simulation.map = simulation.map
                                                , output.path = output.path
                                                , sim.version = simulations)
        
        cat("\n > Predicted habitat plot saved")
        
      }else if(predict.all.map == FALSE){
        
        # deal with the results regarding model performance
        output.path = paste0(name.simulation, "/VALIDATION")
        RF.model = results.simul[[1]]$RF.model
        habitat.performance <- as.data.frame(matrix(unlist(lapply(results.simul,"[[", 1)), ncol = length(RF.model$classes) + 1, byrow = TRUE))
        colnames(habitat.performance) <- c(RF.model$classes, "weighted")
        # habitat.performance$simulation <- abs.simulParams
        habitat.performance$simulation <- simulations
        # save
        fwrite(habitat.performance, paste0(output.path, "/HABITAT/performance.habitat.csv"), row.names = FALSE)
        cat("\n > Habitat performance saved")
        
      }
    }
    
    if(doComposition == TRUE){ # If PFG composition validation activated, the function uses the results to save a table with proximity of PFG composition for each PFG and habitat*strata define by the user
      
      output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
      
      results.compo = sapply(results.simul, "[[", "performance.compo")
      results <- sapply(results.compo, function(X){X$aggregated.proximity})
      rownames(results) <- paste0(results.compo[[1]]$habitat, "_", results.compo[[1]]$strata)
      # colnames(results) <- abs.simulParams
      colnames(results) <- simulations
      results.compo <- t(results)
      results.compo <- as.data.frame(results)
      results.compo$simulation <- rownames(results)
      
      #save and return
      fwrite(results.compo, paste0(output.path.compo, "/performance.composition.csv"), row.names = FALSE)
      cat("\n > Performance composition file saved \n")
      
    }
  } # End of (doHabitat | doComposition) condition
  
  if(doRichness == TRUE){ # PFG Richness validation
    
    output.path = paste0(name.simulation, "/VALIDATION/PFG_RICHNESS")
    perStrata = perStrata
    
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
  
  if(doHabitat == TRUE & predict.all.map == TRUE){
    
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
    performances = cbind(performances, hab.perf[1:length(new.studied.habitat[,1]),])
    colnames(performances) = c("habitat", "TSS_testing_part", "TSS_training_part", simulations)
    rownames(performances) = seq(1, length(studied.habitat[,1]), 1)
    
    cat("\n ---------- HABITAT : \n")
    cat(paste0("\n", round(failure, digits = 2), "% of habitats are not correctly predicted by the simulations \n"))
    cat(paste0("\n", round(success, digits = 2), "% of habitats are correctly predicted by the simulations \n"))
    cat("\n Habitat performance : \n")
    print(performances)
    return(prediction.map)
    
  } else if (doHabitat == TRUE & predict.all.map == FALSE){
    
    testing = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_testing.csv")))
    training = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/RF_perf.per.hab_training.csv")))
    hab.perf = as.data.frame(fread(paste0(name.simulation, "/VALIDATION/HABITAT/performance.habitat.csv")))
    hab.perf = as.data.frame(t(hab.perf))
    colnames(hab.perf) = hab.perf["simulation",]
    performances = testing[,c("habitat","TSS")]
    colnames(performances) = c("habitat", "TSS_testing_part")
    performances$TSS_training_part = training$TSS
    performances = cbind(performances, hab.perf[1:length(new.studied.habitat[,1]),])
    colnames(performances) = c("habitat", "TSS_testing_part", "TSS_training_part", simulations)
    rownames(performances) = seq(1, length(studied.habitat[,1]), 1)
    
    cat("\n ---------- HABITAT : \n")
    cat("\n Habitat performance : \n")
    return(performances)
    
  } else{
    cat("\n ---------- HABITAT VALIDATION DISABLED \n")
  }
}
