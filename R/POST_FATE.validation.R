### HEADER ##########################################################################
##'
##' @title Computes validation data for habitat, PFG richness and composition for a \code{FATE} simulation.
##' 
##' @name POST_FATE.validation
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to compute validation data for : 
##' \cr \code{Habitat} : compares habitat simulations and observations and
##' create a map to visualize this comparison with all the \code{FATE} and
##' observed data.
##' \cr \code{PFG Composition} : produced a computation of observed distribution 
##' of relative abundance in the simulation area and a computation of distance between
##' observed and simulated distribution.
##' \cr \code{PFG Richness} : computes the PFG richness over the whole simulation area 
##' for a \code{FATE} simulation and computes the difference between observed and simulated PFG richness.
##' 
##' @param name.simulation simulation folder name.
##' @param sim.version a character vector with the name(s) of the simulation(s) to validate.
##' @param year year of simulation for validation.
##' @param perStrata \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG abundance is defined by strata. 
##' If \code{FALSE}, PFG abundance defined for all strata (habitat & PFG composition & PFG richness validation).
##' @param opt.no_CPU default \code{1}. \cr The number of resources that can be used to 
##' parallelize the computation of prediction performance for habitat & richness validation.
##' 
##' @param doHabitat \code{Logical}. Default \code{TRUE}. If \code{TRUE}, habitat validation module is activated,
##' if \code{FALSE}, habitat validation module is disabled.
##' @param releves.PFG a data frame with abundance (column named abund) at each site
##' and for each PFG and strata, if necessary, & sites coordinates (habitat & PFG composition validation).
##' @param hab.obs a raster map of the extended studied map in the simulation, with same projection 
##' & resolution than simulation mask (habitat & PFG composition validation).
##' @param validation.mask a raster mask that specified which pixels need validation, with same projection 
##' & resolution than simulation mask (habitat & PFG composition validation).
##' @param studied.habitat a 2 columns data frame which contains the habitats (2nd column) and the ID (1st column) 
##' for each of them which will be taken into account for the validation (habitat & PFG composition validation).
##' @param list.strata.simulations If \code{perStrata} = \code{TRUE}, 
##' a character vector which contain \code{FATE} strata definition and correspondence with observed strata definition. 
##' If \code{perStrata} = \code{FALSE}, please specify \code{NULL} value.
##' 
##' @param doComposition \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG composition validation module is activated,
##' if \code{FALSE}, PFG composition validation module is disabled.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation (PFG composition validation).
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation (PFG composition validation).
##' @param strata.considered_PFG.compo If \code{perStrata} = \code{FALSE}, a character vector with value "A" 
##' (selection of one or several specific strata disabled). If \code{perStrata} = \code{TRUE}, a character 
##' vector with at least one of the observed strata (PFG composition validation).
##' 
##' @param doRichness \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG richness validation module is activated,
##' if \code{FALSE}, PFG richness validation module is disabled.
##' @param list.PFG a character vector which contain all the PFGs taken account in
##' the simulation and observed in the simulation area (PFG richness validation).
##' @param exclude.PFG default \code{NULL}. A character vector containing the names 
##' of the PFG you want to exclude from the analysis (PFG richness validation).
##' 
##' @details 
##' 
##' \describe{
##'   \item{Habitat validation}{The observed habitat is derived from a map of the area or, if defined, 
##' from \code{studied.habitat}, the simulated habitat is derived from \code{FATE} simulated relative 
##' abundance, based on a random forest algorithm trained on observed releves data (see \code{\link{train_RF_habitat}}). \cr
##' To compare observations and simulations, the function computes confusion matrix between 
##' observations and predictions and then compute the TSS for each habitat h 
##' (number of prediction of habitat h/number of observation of habitat h + number of non-prediction 
##' of habitat h/number of non-observation of habitat h). \cr The final metrics this script use is the 
##' mean of TSS per habitat over all habitats, weighted by the share of each habitat in the observed 
##' habitat distribution. The habitat validation also provides a visual comparison of observed and 
##' simulated habitat on the whole studied area (see \code{\link{do_habitat_validation}} &
##' \code{\link{plot_predicted_habitat}}).} \cr
##'   \item{PFG composition validation}{This code firstly run the \code{get_observed_distribution} 
##' function in order to have a \code{obs.distri} file which contain the observed distribution 
##' per PFG, strata and habitat. This file is also an argument for the \code{do_PFG_composition_validation} 
##' function run next. This second sub function provides the computation of distance between observed 
##' and simulated distribution.}
##'   \item{PFG richness validation}{Firstly, the function updates the \code{list.PFG} with \code{exclude.PFG} vector.
##' Then, the script takes the abundance per PFG (and per strata if option selected) file from the 
##' results of the \code{FATE} simulation and computes the difference between the \code{list.PFG} 
##' and all the PFG which are presents in the abundance file, in order to obtain the PFG richness 
##' for a simulation. The function also determine if an observed PFG is missing in the results of the 
##' simulation at a specific year.}
##' }
##' 
##' @return 
##' 
##' Output files : 
##' \describe{
##'   \item{\file{VALIDATION/HABITAT}}{containing the prepared CBNA data,
##'   RF model, the performance analyzes (confusion matrix and TSS) for the training and 
##' testing parts of the RF model, the habitat performance file, the habitat prediction file with 
##' observed and simulated habitat for each pixel of the whole map and the final prediction plot.}
##' }
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION}}{1 .csv file which contain the proximity 
##'   between observed and simulated data computed for each PFG/strata/habitat. \cr 1 .csv file which 
##'   contain the observed releves transformed into relative metrics. \cr 1 .csv file which contain 
##'   the final output with the distribution per PFG, strata and habitat.}
##' }
##' \describe{
##'   \item{\file{VALIDATION/PFG_RICHNESS}}{1 .csv file of PFG richness in a \code{FATE} simulation.
##'   \cr 1 .csv fie of the PFG extinction frequency in a \code{FATE} simulation. \cr 1 .rds file which is 
##'   the abundance per PFG file.
##' }
##' 
##' @examples 
##' 
##' library(raster)
##' library(sf)
##' 
##' ## Define a vector to choose habitats taken into account
##' studied.habitat = data.frame(ID = c(6, 5, 7, 8), habitat = c("coniferous.forest", "deciduous.forest", "natural.grassland", "woody.heatland"))
##' ## Habitat & validation maps
##' hab.observed = raster("FATE_Champsaur/DATA_OBS/simplified.cesbio.map.grd")
##' validation.mask = raster("FATE_Champsaur/DATA_OBS/certain.habitat.100m.restricted.grd")
##' simulation.map = raster("FATE_Champsaur/DATA/MASK/MASK_Champsaur.tif")
##' hab.obs = projectRaster(from = hab.observed, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
##' validation.mask = projectRaster(from = validation.mask, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
##' ## Observed data
##' releves.sites = as.data.frame(st_read("FATE_Champsaur/DATA_OBS/releves.sites.shp"))
##' releves.PFG = as.data.frame(read.csv("FATE_Champsaur/DATA_OBS/releves.PFG.abundance.csv"))
##' releves.PFG = merge(releves.PFG, releves.sites[,c("site","geometry")], by = "site")
##' coor = SpatialPoints(st_coordinates(releves.PFG$geometry), proj4string = crs(hab.observed))
##' coor = spTransform(coor, CRSobj = crs(hab.obs))
##' releves.PFG$geometry = NULL
##' releves.PFG[,c("x","y")] = coor@coords
##' colnames(releves.PFG) = c("site", "abund", "PFG", "strata", "x", "y")
##' ## List of FATE strata & correspondence with observed strata (if perStrata = TRUE, if not = NULL)
##' list.strata.simulations = list(S = c(1,2,3), M = c(4), B = c(5,6,7))
##' ## List of PFG taken into account in a FATE simulation
##' list.PFG = as.factor(c("C1", "C2", "C3", "C4", "H1", "H2", "H3", "H4", "H5", "H6", "P1", "P2", "P3", "P4", "P5"))
##' ## Habitat, strata and PFG considered in PFG compo validation
##' habitat.considered = c("coniferous.forest", "deciduous.forest", "natural.grassland", "woody.heatland")
##' strata.considered_PFG.compo = c("S", "M", "B")
##' PFG.considered_PFG.compo = as.factor(c("H1", "H2", "H3", "H4", "H5", "H6", "P1", "P2", "P3", "P4", "P5"))
##' 
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                      , sim.version = c("SIMUL_V4.1", "SIMUL_V8.1")
##'                      , year = 2000
##'                      , perStrata = TRUE
##'                      , opt.no_CPU = 1
##'                      , doHabitat = TRUE
##'                      , releves.PFG = releves.PFG
##'                      , hab.obs = hab.obs
##'                      , validation.mask = validation.mask
##'                      , studied.habitat = studied.habitat
##'                      , list.strata.simulations = list.strata.simulations
##'                      , doComposition = TRUE
##'                      , PFG.considered_PFG.compo = PFG.considered_PFG.compo
##'                      , habitat.considered_PFG.compo = habitat.considered
##'                      , strata.considered_PFG.compo = strata.considered_PFG.compo
##'                      , doRichness = TRUE
##'                      , list.PFG = list.PFG
##'                      , exclude.PFG = NULL)
##' 
##' @export
##' 
##' @importFrom stringr str_split
##' @importFrom raster raster res crop origin compareCRS extent ncell getValues levels
##' @importFrom utils read.csv write.csv
##' @importFrom foreach foreach %dopar%
##' @importFrom forcats fct_expand
##' @importFrom readr write_rds
##' @importFrom doParallel registerDoParallel
##' @importFrom dplyr select rename
##' @importFrom tidyselect all_of
##' @importFrom stats aggregate
##' 
### END OF HEADER ###################################################################


POST_FATE.validation = function(name.simulation
                                , sim.version
                                , year
                                , perStrata = TRUE
                                , opt.no_CPU = 1
                                , doHabitat = TRUE
                                , releves.PFG
                                , hab.obs
                                , validation.mask = NULL
                                , studied.habitat
                                , list.strata.simulations
                                , doComposition = TRUE
                                , PFG.considered_PFG.compo
                                , habitat.considered_PFG.compo
                                , strata.considered_PFG.compo
                                , doRichness = TRUE
                                , list.PFG
                                , exclude.PFG = NULL){
  
  if (doHabitat == TRUE | doComposition == TRUE){ # Habitat or composition or both validation
    
    if(doHabitat == TRUE & doComposition == TRUE){
      cat("\n\n #------------------------------------------------------------#")
      cat("\n # HABITAT & PFG COMPOSITION VALIDATION")
      cat("\n #------------------------------------------------------------# \n")
    }else if(doHabitat == TRUE & doComposition == FALSE){
      cat("\n\n #------------------------------------------------------------#")
      cat("\n # HABITAT VALIDATION")
      cat("\n #------------------------------------------------------------# \n")
    }else if(doHabitat ==  FALSE & doComposition == TRUE){
      cat("\n\n #------------------------------------------------------------#")
      cat("\n # PFG COMPOSITION VALIDATION")
      cat("\n #------------------------------------------------------------# \n")
    }
    
    cat("\n ----------- CHECKS & DATA PREPARATION")
    
    #######################
    # 0. Global parameters
    #######################
    
    # General
    year = year # choice in the year for validation
    perStrata = perStrata
    opt.no_CPU = opt.no_CPU
    
    # Observed releves data
    releves.PFG = releves.PFG
    # releves.sites = releves.sites
    if(perStrata==TRUE){
      list.strata.releves = as.character(unique(releves.PFG$strata))
      list.strata.simulations = list.strata.simulations
    }else {
      list.strata.releves = NULL
      list.strata.simulations = NULL
    }
    
    # Habitat map
    hab.obs = hab.obs
    
    # Simulation mask
    name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                     flag = "MASK",
                     flag.split = "^--.*--$",
                     is.num = FALSE) # isolate the access path to the simulation mask for any FATE simulation
    simulation.map = raster(paste0(name))
    
    # Validation mask (if provided)
    if(!is.null(validation.mask)){
      validation.mask = validation.mask
    }
    
    #######################
    # I. Preliminary checks
    #######################
    
    # Check hab.obs map
    if(!compareCRS(simulation.map, hab.obs) | !all(res(hab.obs)==res(simulation.map))){
      stop(paste0("Projection & resolution of hab.obs map does not match with simulation mask. Please reproject hab.obs map with projection & resolution of ", names(simulation.map)))
    }else if(extent(simulation.map) != extent(hab.obs)){
      habitat.FATE.map = crop(hab.obs, simulation.map)
    }else {
      habitat.FATE.map = hab.obs
    }
    if(!all(origin(simulation.map) == origin(habitat.FATE.map))){
      cat("\n setting origin habitat.FATE.map to match simulation.map \n")
      raster::origin(habitat.FATE.map) <- raster::origin(simulation.map)
    }
    
    # Check validation mask
    if(!is.null(validation.mask)){
      if(!compareCRS(simulation.map, validation.mask) | !all(res(validation.mask)==res(simulation.map))){
        stop(paste0("Projection & resolution of validation mask does not match with simulation mask. Please reproject validation mask with projection & resolution of ", names(simulation.map)))
      }else if(extent(validation.mask) != extent(simulation.map)){
        validation.mask = crop(validation.mask, simulation.map)
      }else {
        validation.mask = vlidation.mask
      }
      if(!all(origin(simulation.map) == origin(validation.mask))){
        cat("\n setting origin validation mask to match simulation.map \n")
        raster::origin(validation.mask) <- raster::origin(simulation.map)
      }
    }
    
    # Check studied habitat
    if(is.null(studied.habitat)){
      stop("studied.habitat vector is null, please specify at least one habitat which will be taken into account in the validation")
    } else if(is.data.frame(studied.habitat)){
      studied.habitat = studied.habitat # if a data frame with habitat names and codes, the function will study only the habitats in the vector
    } else{
      stop("studied.habitat is not a data frame")
    }
    
    # check if strata definition used in the RF model is the same as the one used to analyze FATE output
    if (perStrata == TRUE) {
      if (all(intersect(names(list.strata.simulations), list.strata.releves) == names(list.strata.simulations))) {
        list.strata = names(list.strata.simulations)
        cat("\n strata definition OK \n")
      } else {
        stop("wrong strata definition")
      }
    } else if (perStrata == FALSE) {
      list.strata <- "all"
    } else {
      stop("check 'perStrata' parameter and/or the names of strata in list.strata.releves & list.strata.simulation")
    }
    
    #################################################################
    # I.2 Train a RF model on observed data (habitat validation only)
    #################################################################
    
    if(doHabitat == TRUE){
      
      cat("\n ----------- TRAIN A RANDOM FOREST MODEL ON OBSERVED DATA")
      
      output.path = paste0(name.simulation, "/VALIDATION")
      
      ## TRAIN A RF ON OBSERVED DATA
      
      RF.param = list(share.training = 0.7, ntree = 500)
      
      RF.model = train_RF_habitat(releves.PFG = releves.PFG
                                  , hab.obs = hab.obs
                                  , external.training.mask = NULL
                                  , studied.habitat = studied.habitat
                                  , RF.param = RF.param
                                  , output.path = output.path
                                  , perStrata = perStrata
                                  , sim.version = sim.version)
      
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
    if (!is.null(studied.habitat) & nrow(studied.habitat) > 0 & ncol(studied.habitat) == 2){
      habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(studied.habitat,c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
      habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% studied.habitat$habitat), ]
    } else {
      stop("Habitat definition in studied.habitat is not correct")
    }
    if(doComposition == TRUE){
      habitat.whole.area.df = habitat.whole.area.df[which(habitat.whole.area.df$for.validation == 1),]
    }
    
    cat("\n Habitat considered in the prediction exercise: ", c(unique(habitat.whole.area.df$habitat)), "\n", sep = "\t")
    
    cat("\n ----------- PROCESSING SIMULATIONS")
    
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
    results.simul = foreach(i = 1:length(all_of(sim.version))) %dopar% # loop on simulations
      {
      
        sim <- sim.version[i]
        cat("\n", sim, "\n")
        
        # get simulated abundance per pixel*strata*PFG for pixels in the simulation area
        if (perStrata == FALSE) {
          if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim, ".csv")))
          {
            simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim, ".csv"))
            simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))] # keep only the PFG, ID.pixel and abundance at any year columns
            # careful : the number of abundance data files to save is to defined in POST_FATE.temporal.evolution function
            colnames(simu_PFG) = c("PFG", "pixel", "abs")
            simu_PFG$strata <- "A"
          }else
          {
            stop("Simulated abundance file does not exist")
          }
          
        } else if (perStrata == TRUE) {
          if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim, ".csv")))
          {
            simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim, ".csv"))
            simu_PFG = simu_PFG[, c("PFG", "ID.pixel", "strata", paste0("X", year))]
            colnames(simu_PFG) = c("PFG", "pixel", "strata", "abs")
            new.strata <- rep(NA, nrow(simu_PFG))
            for (i in 1:length(list.strata.simulations)) {
              ind = which(simu_PFG$strata %in% list.strata.simulations[[i]])
              new.strata[ind] = names(list.strata.simulations)[i]
            }
            simu_PFG$strata = new.strata
          }else
          {
            stop("Simulated abundance file does not exist")
          }
        }
        
        simu_PFG <- aggregate(abs ~ pixel + strata + PFG, data = simu_PFG, FUN = "sum")
        
        if (doHabitat == TRUE){ # Only for habitat validation
          
          cat("\n ----------- HABITAT PREDICTION")
          
          ## USE THE RF MODEL TO VALIDATE FATE OUTPUT
          
          predict.all.map = TRUE
          
          results.habitat = do_habitat_validation(output.path = output.path
                                                   , RF.model = RF.model
                                                   , predict.all.map = predict.all.map
                                                   , sim = sim
                                                   , simu_PFG = simu_PFG
                                                   , habitat.whole.area.df = habitat.whole.area.df
                                                   , list.strata = list.strata
                                                   , perStrata = perStrata)
          
        }
        
        if (doComposition == TRUE){ # Only for PFG composition validation
          
          cat("\n ----------- PFG COMPOSITION VALIDATION")
          
          output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
          
          ## GET OBSERVED DISTRIBUTION
          
          cat("\n Get observed distribution \n")
          
          obs.distri = get_observed_distribution(releves.PFG = releves.PFG
                                                 , hab.obs = hab.obs
                                                 , studied.habitat = studied.habitat
                                                 , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                                 , strata.considered_PFG.compo = strata.considered_PFG.compo
                                                 , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                                 , perStrata = perStrata
                                                 , output.path = output.path.compo)
          
          ## DO PFG COMPOSITION VALIDATION
          
          cat("\n Comparison between observed and simulated distribution \n")
          
          performance.composition = do_PFG_composition_validation(sim = sim
                                                                  , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                                                  , strata.considered_PFG.compo = strata.considered_PFG.compo
                                                                  , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                                                  , observed.distribution = obs.distri
                                                                  , simu_PFG = simu_PFG
                                                                  , habitat.whole.area.df = habitat.whole.area.df)
          
        }
        
        if(doHabitat == TRUE & doComposition == TRUE){
          results = list(habitat.prediction = results.habitat$y.all.map.predicted, habitat.performance = results.habitat$output.validation, RF.model = RF.model, performance.compo = performance.composition)
          return(results)
        }
        if(doHabitat == TRUE & doComposition == FALSE){
          results = list(habitat.prediction = results.habitat$all.map.prediction, habitat.performance = output.performance.habitat, RF.model = RF.model)
          return(results)
        }
        if(doHabitat == FALSE & doComposition == TRUE){
          results = list(performance.compo = performance.composition)
          return(results)
        } # Based on choice of the user, foreach loop returns different results
        
      } # End of loop on simulations
    cat("\n End of loop on simulations")
    
    if(doHabitat == TRUE){ # If habitat validation activated, the function uses the results to build and save a final map of habitat prediction
      
      # deal with the results regarding model performance
      output.path = paste0(name.simulation, "/VALIDATION")
      RF.model = results.simul[[1]]$RF.model
      habitat.performance <- as.data.frame(matrix(unlist(lapply(results.simul,"[[", 2)), ncol = length(RF.model$classes) + 1, byrow = TRUE))
      colnames(habitat.performance) <- c(RF.model$classes, "weighted")
      habitat.performance$simulation <- sim.version
      # save
      write.csv(habitat.performance, paste0(output.path, "/HABITAT/performance.habitat.csv"), row.names = FALSE)
      
      # deal with the results regarding habitat prediction over the whole map
      all.map.prediction = as.data.frame(lapply(results.simul, "[[", 1))
      all.map.prediction = all.map.prediction[,c(sim.version, "pixel", "habitat")]
      all.map.prediction = rename(all.map.prediction, "true.habitat" = "habitat")
      # save
      write.csv(all.map.prediction, paste0(output.path,"/HABITAT/habitat.prediction.csv"), row.names = FALSE)
      cat("\n Habitat results saved \n")
      
      ## AGGREGATE HABITAT PREDICTION AND PLOT PREDICTED HABITAT
      
      # Provide a color df
      col.df = data.frame(
        habitat = RF.model$classes,
        failure = terrain.colors(length(RF.model$classes), alpha = 0.5),
        success = terrain.colors(length(RF.model$classes), alpha = 1))
      
      prediction.map = plot_predicted_habitat(predicted.habitat = all.map.prediction
                                              , col.df = col.df
                                              , simulation.map = simulation.map
                                              , output.path = output.path
                                              , sim.version = sim.version)
      
      cat("\n Predicted habitat plot saved \n")
    }
    
    if(doComposition == TRUE){ # If PFG composition validation activated, the function uses the results to save a table with proximity of PFG composition for each PFG and habitat*strata define by the user
      
      output.path.compo = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION")
      
      results.compo = sapply(results.simul, "[[", "performance.compo")
      results <- sapply(results.compo, function(X){X$aggregated.proximity})
      rownames(results) <- paste0(results.compo[[1]]$habitat, "_", results.compo[[1]]$strata)
      colnames(results) <- sim.version
      results.compo <- t(results)
      results.compo <- as.data.frame(results)
      results.compo$simulation <- rownames(results)
      
      #save and return
      write.csv(results.compo, paste0(output.path.compo, "/performance.composition.csv"), row.names = FALSE)
      cat("\n Performance composition file saved \n")
      
    }
  } # End of (doHabitat | doComposition) condition
  
  if(doRichness == TRUE){ # PFG Richness validation
    
    cat("\n\n #------------------------------------------------------------#")
    cat("\n # PFG RICHNESS VALIDATION")
    cat("\n #------------------------------------------------------------# \n")
    
    output.path = paste0(name.simulation, "/VALIDATION/PFG_RICHNESS")
    perStrata = perStrata
    
    #list of PFG of interest
    list.PFG = setdiff(list.PFG,exclude.PFG)
    
    cat("\n Data preparation \n")
    
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
    dying.PFG.list <- foreach(i = 1:length(all_of(sim.version))) %dopar% { # Loop on simulations
      
      sim <- sim.version[i]
      
      if(perStrata == FALSE){
        
        if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim, ".csv"))){
          
          simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim, ".csv"))
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))]
          colnames(simu_PFG) = c("PFG", "pixel", "abs")
          
        }
        
      } else if(perStrata == TRUE){
        
        if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim, ".csv"))){
          
          simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim, ".csv"))
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", "strata", paste0("X", year))]
          colnames(simu_PFG) = c("PFG", "pixel", "strata", "abs")
          
        }
      }
      
      return(setdiff(list.PFG,unique(simu_PFG$PFG)))
      
    } # End of loop
    
    cat("\n Richness computation \n")
    
    # names the results
    names(dying.PFG.list) = sim.version
    
    # get table with PFG richness
    PFG.richness.df = data.frame(simulation = names(dying.PFG.list), richness = length(list.PFG) - unlist(lapply(dying.PFG.list, FUN = "length")))
    
    # get vector with one occurence per PFG*simulation with dying of the PFG, as factor with completed levels in order to have table with all PFG, including those which never die
    dyingPFG.vector = as.factor(unlist(dying.PFG.list))
    dyingPFG.vector = fct_expand(dyingPFG.vector, list.PFG)
    dying.distribution = round(table(dyingPFG.vector)/length(sim.version), digits = 2)
    
    # output
    output = list(PFG.richness.df, dying.distribution , dying.PFG.list)
    names(output) = c("PFG.richness.df", "dying.distribution", "dying.PFG.list")
    
    dir.create(output.path, recursive = TRUE, showWarnings = FALSE)
    
    write.csv(PFG.richness.df, paste0(output.path, "/performance.richness.csv"), row.names = F)
    write.csv(dying.distribution, paste0(output.path, "/PFG.extinction.frequency.csv"), row.names = F)
    write_rds(dying.PFG.list, file = paste0(output.path, "/dying.PFG.list.rds"), compress = "none")
    
    cat("\n PFG richness results saved \n")
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # RESULTS : ")
  cat("\n #------------------------------------------------------------# \n")
  
  if(doRichness == TRUE){
    
    cat("\n ---------- PFG RICHNESS : \n")
    cat(paste0("\n Richness at year ", year, " : ", output[[1]], "\n"))
    
  } else{ 
    
    cat("\n ---------- PFG RICHNESS VALIDATION DISABLED \n")
    
  }
  
  if(doHabitat == TRUE){
    
    hab.pred = read.csv(paste0(name.simulation, "/VALIDATION/HABITAT/hab.pred.csv"))
    failure = as.numeric((table(hab.pred$prediction.code)[1]/sum(table(hab.pred$prediction.code)))*100)
    success = as.numeric((table(hab.pred$prediction.code)[2]/sum(table(hab.pred$prediction.code)))*100)
    
    cat("\n ---------- HABITAT : \n")
    cat(paste0("\n", round(failure, digits = 2), "% of habitats are not correctly predicted by the simulations \n"))
    cat(paste0("\n", round(success, digits = 2), "% of habitats are correctly predicted by the simulations \n"))
    plot(prediction.map)
    
  } else{
    
    cat("\n ---------- HABITAT VALIDATION DISABLED \n")
    
  }
  
  if(doComposition == TRUE){
    
    cat("\n ---------- PFG COMPOSITION : \n")
    return(results.compo[,sim.version])
    
  } else{
    
    cat("\n ---------- PFG COMPOSITION VALIDATION DISABLED \n")
    
  }
  
}
