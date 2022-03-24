### HEADER #####################################################################
##'
##' @title Compute distance between observed and simulated distribution
##'
##' @name do.PFG.composition.validation
##'
##' @author Matthieu Combaud, Maxime Delprat
##'
##' @description This script is designed to compare the difference between the
##' PFG distribution in observed and simulated data. For a set of PFG, strata and
##' habitats chosen, the function compute distance between observed and simulated
##' distribution for a precise \code{FATE} simulation.
##' 
##' @param name.simulation simulation folder name.
##' @param sim.version name of the simulation we want to validate (it works with
##' only one \code{sim.version}).
##' @param hab.obs a raster map of the extended studied map in the simulation, with same projection 
##' & resolution than simulation mask.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation.
##' @param strata.considered_PFG.compo a character vector of the list of precise 
##' strata considered in the validation.
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation.
##' @param observed.distribution PFG observed distribution table provides by \code{get.observed.distribution} function.
##' @param perStrata.compo \code{Logical}. All strata together (FALSE) or per strata (TRUE).
##' @param validation.mask a raster mask that specified 
##' which pixels need validation, with same projection & resolution than simulation mask.
##' @param year year of simulation to validate.
##' @param list.strata.simulations a character vector which contain \code{FATE} 
##' strata definition and correspondence with observed strata definition.
##' @param list.strata.releves a character vector which contain the observed strata 
##' definition, extracted from observed PFG releves.
##' @param habitat.FATE.map a raster map of the observed habitat in the
##' studied area with same projection & resolution than validation mask and simulation mask.
##' @param studied.habitat default \code{NULL}. If \code{NULL}, the function will
##' take into account of habitats define in the \code{hab.obs} map. Otherwise, please specify 
##' in a 2 columns data frame the habitats (2nd column) and the ID (1st column) for each of them which will be taken 
##' into account for the validation.
##' 
##' @details 
##' 
##' After preliminary checks, this code extract observed habitat from the \code{hab.obs}
##' map and, then, merge it with the simulated PFG abundance file (with or without strata definition) 
##' from results of the \code{FATE} simulation selected with \code{sim.version}. 
##' After filtration of the required PFG, strata and habitats, the function transforms 
##' the data into relative metrics and, then, compute distribution per PFG, strata
##' and habitat (if necessary). Finally, the code computes proximity between observed 
##' and simulated data, per PFG, strata and habitat.
##' 
##' @return 
##' 
##' 1 file is created in
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION/sim.version} :
##'   A .csv file which contain the proximity between observed and simulated data computed
##'   for each PFG/strata/habitat.
##'   
##' @export
##' 
##' @importFrom dplyr rename filter group_by mutate %>% select
##' @importFrom raster raster projectRaster res crs crop extent origin compareRaster 
##' getValues ncell compareCRS levels
##' @importFrom stats aggregate
##' @importFrom utils read.csv write.csv
##' @importFrom data.table setDT
##' @importFrom tidyselect all_of
##'
### END OF HEADER ##############################################################


do.PFG.composition.validation<-function(name.simulation, sim.version, hab.obs, PFG.considered_PFG.compo
                                        , strata.considered_PFG.compo, habitat.considered_PFG.compo, observed.distribution
                                        , perStrata, validation.mask, year, list.strata.simulations, list.strata.releves
                                        , habitat.FATE.map, studied.habitat){
  
  cat("\n ---------- PFG COMPOSITION VALIDATION \n")
  
  output.path = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION/", sim.version)
  
  #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
  compute.proximity<-function(simulated.quantile,observed.quantile){
    #for a given PFG*habitat*strata, return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
    return(1-sum(abs(simulated.quantile-observed.quantile))/4)
  }
  
  ############################
  # 1. Preliminary checks
  ############################
  
  #check if strata definition used in the RF model is the same as the one used to analyze FATE output
  if (perStrata == TRUE) {
    if (all(intersect(names(list.strata.simulations), list.strata.releves) == names(list.strata.simulations))) {
      list.strata = names(list.strata.simulations)
      print("strata definition OK")
    } else {
      stop("wrong strata definition")
    }
  } else if (perStrata == FALSE) {
    list.strata <- "all"
  } else {
    stop("check 'perStrata' parameter and/or the names of strata in list.strata.releves & list.strata.simulation")
  }

  #initial consistency between habitat.FATE.map and validation.mask (do it before the adjustement of habitat.FATE.map)
  if(!compareCRS(habitat.FATE.map,validation.mask) | !all(res(habitat.FATE.map) == res(validation.mask))){
    stop("please provide rasters with same crs and resolution for habitat.FATE.map and validation.mask")
  }
  
  #########################################
  # 2. Get observed habitat
  #########################################
  
  habitat.whole.area.df <- data.frame(pixel = seq(1, ncell(habitat.FATE.map), 1)
                                      , code.habitat = getValues(habitat.FATE.map)
                                      , for.validation = getValues(validation.mask))
  habitat.whole.area.df <- habitat.whole.area.df[which(getValues(simulation.map) == 1), ] #index of the pixels in the simulation area
  habitat.whole.area.df <- habitat.whole.area.df[which(!is.na(habitat.whole.area.df$for.validation)), ] 
  if (!is.null(studied.habitat) & nrow(studied.habitat) > 0 & ncol(studied.habitat) == 2){
    habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(studied.habitat,c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
    habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% RF.model$classes), ]
  } else if (names(raster::levels(hab.obs)[[1]]) == c("ID", "habitat", "colour") & nrow(raster::levels(hab.obs)[[1]]) > 0 & is.null(studied.habitat)){
    habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(levels(hab.obs)[[1]],c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
    habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% RF.model$classes), ]
  }
  
  print(cat("Habitat considered in the prediction exercise: ", c(unique(habitat.whole.area.df$habitat)), "\n", sep = "\t"))
  
  print("Habitat in the simulation area:")
  table(habitat.whole.area.df$habitat, useNA = "always")
  
  print("Habitat in the subpart of the simulation area used for validation:")
  table(habitat.whole.area.df$habitat[habitat.whole.area.df$for.validation == 1], useNA = "always")
  
  
  ##############################
  # 3. Loop on simulations
  ##############################
  
  print("processing simulations")
  
  results.simul<-list()
  for(i in 1:length(all_of(sim.version))) {
    
    # 3.1. Data preparation
    #########################
    
    #get simulated abundance per pixel*strata*PFG for pixels in the simulation area
    if(perStrata == FALSE){
      
      if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))){
        
        simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
        simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))]
        colnames(simu_PFG) = c("PFG", "pixel", "abs")
        simu_PFG$strata <- "A"
        
      }else {
        
        stop("Simulated abundance file does not exist")
      }
      
    }else if(perStrata == TRUE){
      
      if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv"))){
        
        simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv"))
        simu_PFG = simu_PFG[,c("PFG","ID.pixel", "strata", paste0("X", year))]
        colnames(simu_PFG) = c("PFG", "pixel", "strata", "abs")
        new.strata <- rep(NA, nrow(simu_PFG))
        for (i in 1:length(list.strata.simulations)) {
          ind = which(simu_PFG$strata %in% list.strata.simulations[[i]])
          new.strata[ind] = names(list.strata.simulations)[i]
        }
        simu_PFG$strata = new.strata
        
      }else {
        
        stop("Simulated abundance file does not exist")
      }
    }
    
    #aggregate all the rows with same pixel, (new) strata and PFG (necessary since possibly several line with the same pixel+strata+PFG after strata grouping)
    simu_PFG <- aggregate(abs ~ pixel + strata + PFG, data = simu_PFG, FUN = "sum") #sum and not mean because for a given CBNA strata some PFG are present in 2 FATE strata (let's say 1 unit in each) and other are present in 3 FATE strata (let's say one unit in each), so taking the mean would suppress the info that the second PFG is more present!

    # 3.2. Merge with habitat
    ###########################
    
    #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
    simu_PFG <- merge(simu_PFG, habitat.whole.area.df, by = "pixel") #at this stage we have all the pixels in the simulation area
    
    # 3.3. Filter the required PFG, strata and habitat
    ###################################################
    
    simu_PFG<-filter(
      simu_PFG,
      is.element(PFG,PFG.considered_PFG.compo)&
        is.element(strata,strata.considered_PFG.compo)&
        is.element(habitat,habitat.considered_PFG.compo)
    )
    
    # 3.4.Transform into a relative metrics (here relative.metric is relative coverage)
    #####################################################################################
    
    #important to do it only here, because if we filter some PFG, it changes the value of the relative metric (no impact of filtering for habitat or for strata since we do it per strata, and habitat is constant across a given pixel)
    #careful: if several strata/habitat are selected, the computation is made for each strata separately
    simu_PFG <- as.data.frame(simu_PFG %>% group_by(pixel, strata) %>% mutate(relative.metric = round(prop.table(abs), digits = 2)))
    simu_PFG$relative.metric[is.na(simu_PFG$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
    simu_PFG$abs <- NULL
    
    # 3.5. Compute distribution per PFG, and if require per strata/habitat (else all strata/habitat will be considered together)
    ##############################################################################################################################
    
    
    #prepare the df storing quantile values
    simulated.distribution <- expand.grid(
      PFG = PFG.considered_PFG.compo,                              
      habitat = habitat.considered_PFG.compo,
      strata = strata.considered_PFG.compo
    )
    
    null.quantile <- data.frame(rank = seq(0, 4, 1)) #to have 5 rows per PFG*strata*habitat
    simulated.distribution <- merge(simulated.distribution, null.quantile, all = TRUE)
    
    if(dim(simu_PFG)[1] > 0){
      
      distribution <- setDT(simu_PFG)[, quantile(relative.metric), by = c("PFG", "habitat", "strata")]
      distribution <- rename(distribution, "quantile" = "V1")
      distribution <- data.frame(distribution, rank = seq(0, 4, 1)) #add the rank number
      
      simulated.distribution <- merge(simulated.distribution, distribution, by = c("PFG", "habitat", "strata", "rank"), all.x = TRUE) # add the simulated quantiles, "all.x=T" to keep the unobserved combination (with quantile=NA then)
      
      simulated.distribution$quantile[is.na(simulated.distribution$quantile)] <- 0 # "NA" in the previous line means that the corresponding combination PFG*strata*habitat is not present, so as a null relative abundance !
      
    }else{
      simulated.distribution$quantile <- 0
    }
    
    simulated.distribution$habitat <- as.character(simulated.distribution$habitat) #else may generate problem in ordering the database
    simulated.distribution$strata <- as.character(simulated.distribution$strata) #else may generate problem in ordering the database
    simulated.distribution$PFG <- as.character(simulated.distribution$PFG) #else may generate problem in ordering the database
    simulated.distribution$rank <- as.numeric(simulated.distribution$rank) #else may generate problem in ordering the database
    
    
    # 3.6. Order the table to be able to have output in the right format
    #####################################################################
    simulated.distribution <- setDT(simulated.distribution)
    simulated.distribution <- simulated.distribution[order(habitat, strata, PFG, rank)]
    
    
    # 3.7. Rename
    ##############
    simulated.distribution <- rename(simulated.distribution, "simulated.quantile" = "quantile")
    
    
    # 3.8 Rename and reorder the observed database
    ###############################################
    
    observed.distribution$habitat <- as.character(observed.distribution$habitat) #else may generate problem in ordering the database
    observed.distribution$strata <- as.character(observed.distribution$strata) #else may generate problem in ordering the database
    observed.distribution$PFG <- as.character(observed.distribution$PFG) #else may generate problem in ordering the database
    observed.distribution$rank <- as.numeric(observed.distribution$rank) #else may generate problem in ordering the database
    
    observed.distribution <- setDT(observed.distribution)
    observed.distribution <- observed.distribution[order(habitat, strata, PFG, rank)]
    
    # "if" to check that observed and simulated databases are in the same order
    if(
      !(
        all(simulated.distribution$PFG == observed.distribution$PFG)&
        all(simulated.distribution$habitat == observed.distribution$habitat)&
        all(simulated.distribution$strata == observed.distribution$strata)&
        all(simulated.distribution$rank == observed.distribution$rank)
      )
    ){
      stop("Problem in observed vs simulated database (problem in the PFG*strata*habitat considered or in the database order)")
    }
    
    # 3.9. Merge observed and simulated data
    #########################################
    
    simulated.distribution <- cbind(simulated.distribution, observed.quantile = observed.distribution$observed.quantile) #quicker than a merge, but we can do it only because we have worked on the order of the DT
    
    # 3.10 Compute proximity between observed and simulated data, per PFG*strata*habitat
    #####################################################################################
    
    #we get rid off rank==0 because there is good chance that it is nearly always equal to zero both in observed and simulated data, and that would provide a favorable bias in the results
    
    simulated.distribution <- filter(simulated.distribution, rank != 0)
    
    proximity <- simulated.distribution[,compute.proximity(simulated.quantile = simulated.quantile, observed.quantile = observed.quantile), by = c("PFG", "habitat", "strata")]
    proximity <- rename(proximity, "proximity" = "V1")
    proximity <- proximity[order(habitat, strata, PFG)] #to have output in the same order for all simulations
    
    
    # 3.11. Aggregate results for the different PFG
    ################################################
    
    aggregated.proximity <- proximity[,mean(proximity), by = c("habitat", "strata")]
    aggregated.proximity <- rename(aggregated.proximity, "aggregated.proximity" = "V1")
    aggregated.proximity$aggregated.proximity <- round(aggregated.proximity$aggregated.proximity, digits = 2)
    aggregated.proximity$simul <- sim.version
    
    # return(aggregated.proximity)
    
    #line added because the foreach method does not work
    results.simul[[i]] <- aggregated.proximity
    
  }
  
  # 4. Put in the output format
  ##############################
  
  results <- sapply(results.simul, function(X){X$aggregated.proximity})
  rownames(results) <- paste0(results.simul[[1]]$habitat, "_", results.simul[[1]]$strata)
  colnames(results) <- sim.version
  results <- t(results)
  results <- as.data.frame(results)
  results$simulation <- rownames(results)
  
  #save and return
  write.csv(results, paste0(output.path, "/performance.composition.csv"), row.names = FALSE)
  
  return(results)
}

