### HEADER #####################################################################
##'
##' @title Compute distance between observed and simulated distribution
##'
##' @name do_PFG_composition_validation
##'
##' @author Matthieu Combaud, Maxime Delprat
##'
##' @description This script is designed to compare the difference between the
##' PFG distribution in observed and simulated data. For a set of PFG, strata and
##' habitats chosen, the function computes distance between observed and simulated
##' distribution for a precise \code{FATE} simulation.
##' 
##' @param sim name of the single simulation to validate.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation.
##' @param strata.considered_PFG.compo a character vector of the list of precise 
##' strata considered in the validation.
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation.
##' @param observed.distribution PFG observed distribution table provides by \code{get.observed.distribution} function.
##' @param simu_PFG a \code{data frame} with simulated abundance for each PFG and strata 
##' (if option selected) and pixel ID, extracted from a \code{FATE} simulation (see \code{\link{POST_FATE.temporalEvolution}}).
##' @param habitat.whole.area.df a \code{data frame} which contain habitat names and code for each pixel that needs validation.
##' 
##' @details 
##' 
##' Firstly, this code merges \code{habitat.whole.area.df} data frame with the simulated PFG abundance 
##' \code{simu_PFG} data frame (with or without strata definition). 
##' After filtration of the required PFG, strata and habitats, the function transforms 
##' the data into relative metrics and, then, compute distribution per PFG, strata
##' and habitat (if necessary). Finally, the code computes proximity between observed 
##' and simulated data, per PFG, strata and habitat.
##' 
##' @return 
##' 
##' 1 file is created in
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION} :
##'   A .csv file which contain the proximity between observed and simulated data computed
##'   for each PFG/strata/habitat.
##'   
##' @export
##' 
##' @importFrom dplyr rename filter group_by mutate %>% select
##' @importFrom raster raster projectRaster res crs crop extent origin compareRaster 
##' getValues ncell compareCRS levels
##' @importFrom stats aggregate
##' @importFrom data.table setDT
##' @importFrom tidyselect all_of
##'
### END OF HEADER ##############################################################

do_PFG_composition_validation <- function(sim, PFG.considered_PFG.compo
                                          , strata.considered_PFG.compo, habitat.considered_PFG.compo
                                          , observed.distribution, simu_PFG, habitat.whole.area.df){
  
  
  #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
  compute.proximity <- function(simulated.quantile,observed.quantile){
    #for a given PFG*habitat*strata, return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
    return(1-sum(abs(simulated.quantile-observed.quantile))/4)
  }
  
  # 1. Merge with habitat
  ########################
  
  #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
  simu_PFG <- merge(simu_PFG, habitat.whole.area.df, by = "pixel") #at this stage we have all the pixels in the simulation area
  
  # 2. Filter the required PFG, strata and habitat
  #################################################
  
  simu_PFG<-filter(
    simu_PFG,
    is.element(PFG,PFG.considered_PFG.compo)&
      is.element(strata,strata.considered_PFG.compo)&
      is.element(habitat,habitat.considered_PFG.compo)
  )
  
  # 3. Transform into a relative metrics (here relative.metric is relative coverage)
  ###################################################################################
  
  #important to do it only here, because if we filter some PFG, it changes the value of the relative metric (no impact of filtering for habitat or for strata since we do it per strata, and habitat is constant across a given pixel)
  #careful: if several strata/habitat are selected, the computation is made for each strata separately
  simu_PFG <- as.data.frame(simu_PFG %>% group_by(pixel, strata) %>% mutate(relative.metric = round(prop.table(abs), digits = 2)))
  simu_PFG$relative.metric[is.na(simu_PFG$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  simu_PFG$abs <- NULL
  
  # 4. Compute distribution per PFG, and if require per strata/habitat
  #####################################################################
  
  
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
  
  # 5. Order the table to be able to have output in the right format
  ###################################################################
  
  simulated.distribution <- setDT(simulated.distribution)
  simulated.distribution <- simulated.distribution[order(habitat, strata, PFG, rank)]
  
  # 6. Rename
  ############
  
  simulated.distribution <- rename(simulated.distribution, "simulated.quantile" = "quantile")
  
  # 7. Rename and reorder the observed database
  #############################################
  
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
  
  # 8. Merge observed and simulated data
  #######################################
  
  simulated.distribution <- cbind(simulated.distribution, observed.quantile = observed.distribution$observed.quantile) #quicker than a merge, but we can do it only because we have worked on the order of the DT
  
  # 9. Compute proximity between observed and simulated data, per PFG*strata*habitat
  ###################################################################################
  
  #we get rid off rank==0 because there is good chance that it is nearly always equal to zero both in observed and simulated data, and that would provide a favorable bias in the results
  
  simulated.distribution <- filter(simulated.distribution, rank != 0)
  
  proximity <- simulated.distribution[,compute.proximity(simulated.quantile = simulated.quantile, observed.quantile = observed.quantile), by = c("PFG", "habitat", "strata")]
  proximity <- rename(proximity, "proximity" = "V1")
  proximity <- proximity[order(habitat, strata, PFG)] #to have output in the same order for all simulations
  
  
  # 10. Aggregate results for the different PFG
  ##############################################
  
  aggregated.proximity <- proximity[,mean(proximity), by = c("habitat", "strata")]
  aggregated.proximity <- rename(aggregated.proximity, "aggregated.proximity" = "V1")
  aggregated.proximity$aggregated.proximity <- round(aggregated.proximity$aggregated.proximity, digits = 2)
  aggregated.proximity$simul <- sim
  
  results.PFG.compo <- list(aggregated.proximity = aggregated.proximity)
  names(results.PFG.compo) <- "aggregated.proximity"

  return(results.PFG.compo)
  
}

