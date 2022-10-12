#############################################################################################
##' @importFrom dplyr rename filter group_by mutate %>% select
##' @importFrom raster raster projectRaster res crs crop extent origin compareRaster 
##' getValues ncell compareCRS levels
##' @importFrom stats aggregate
##' @importFrom data.table setDT
##' @importFrom tidyselect all_of
#############################################################################################

do_PFG_composition_validation <- function(sim, list.PFG
                                          , studied.habitat, list.strata
                                          , observed.distribution, mat.sim, hab.whole.df){
  
  
  #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
  compute.proximity <- function(simulated.quantile,observed.quantile){
    #for a given PFG*habitat*strata, return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
    return(1-sum(abs(simulated.quantile-observed.quantile))/4)
  }
  
  # 1. Merge with habitat
  ########################
  
  #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
  # mat.sim <- merge(mat.sim, hab.whole.df, by = "pixel") #at this stage we have all the pixels in the simulation area
  
  # 2. Filter the required PFG, strata and habitat
  #################################################
  
  # mat.sim<-filter(
  #   mat.sim,
  #   is.element(PFG,list.PFG)&
  #     is.element(strata,list.strata)
  # )
  
  # 3. Transform into a relative metrics (here relative.metric is relative coverage)
  ###################################################################################
  
  #important to do it only here, because if we filter some PFG, it changes the value of the relative metric (no impact of filtering for habitat or for strata since we do it per strata, and habitat is constant across a given pixel)
  #careful: if several strata/habitat are selected, the computation is made for each strata separately
  # mat.sim <- as.data.frame(mat.sim %>% group_by(pixel, strata) %>% mutate(relative.metric = round(prop.table(abs), digits = 2)))
  # mat.sim$relative.metric[is.na(mat.sim$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  # mat.sim$abs <- NULL
  
  # 4. Compute distribution per PFG, and if require per strata/habitat
  #####################################################################
  
  

  
  
  
  #prepare the df storing quantile values
  # simulated.distribution <- expand.grid(
  #   PFG = list.PFG,
  #   habitat = studied.habitat$habitat,
  #   strata = list.strata
  # )
  # 
  # null.quantile <- data.frame(rank = seq(0, 4, 1)) #to have 5 rows per PFG*strata*habitat
  # simulated.distribution <- merge(simulated.distribution, null.quantile, all = TRUE)
  
  # if(dim(mat.sim)[1] > 0){
  #   
  #   # distribution <- setDT(mat.sim)[, quantile(relative.metric), by = c("PFG", "habitat", "strata")]
  #   # distribution <- rename(distribution, "quantile" = "V1")
  #   # distribution <- data.frame(distribution, rank = seq(0, 4, 1)) #add the rank number
  #   
  #   simulated.distribution <- merge(simulated.distribution, distribution, by = c("PFG", "habitat", "strata", "rank"), all.x = TRUE) # add the simulated quantiles, "all.x=T" to keep the unobserved combination (with quantile=NA then)
  #   
  #   simulated.distribution$quantile[is.na(simulated.distribution$quantile)] <- 0 # "NA" in the previous line means that the corresponding combination PFG*strata*habitat is not present, so as a null relative abundance !
  #   
  # }else{
  #   simulated.distribution$quantile <- 0
  # }
  
  # simulated.distribution$habitat <- as.character(simulated.distribution$habitat) #else may generate problem in ordering the database
  # simulated.distribution$strata <- as.character(simulated.distribution$strata) #else may generate problem in ordering the database
  # simulated.distribution$PFG <- as.character(simulated.distribution$PFG) #else may generate problem in ordering the database
  # simulated.distribution$rank <- as.numeric(simulated.distribution$rank) #else may generate problem in ordering the database
  # 
  # # 5. Order the table to be able to have output in the right format
  # ###################################################################
  # 
  # simulated.distribution <- setDT(simulated.distribution)
  # simulated.distribution <- simulated.distribution[order(habitat, strata, PFG, rank)]
  
  # 6. Rename
  ############
  
  # simulated.distribution <- rename(simulated.distribution, "simulated.quantile" = "quantile")
  
  # 7. Rename and reorder the observed database
  #############################################
  
  # observed.distribution$habitat <- as.character(observed.distribution$habitat) #else may generate problem in ordering the database
  # observed.distribution$strata <- as.character(observed.distribution$strata) #else may generate problem in ordering the database
  # observed.distribution$PFG <- as.character(observed.distribution$PFG) #else may generate problem in ordering the database
  # observed.distribution$rank <- as.numeric(observed.distribution$rank) #else may generate problem in ordering the database
  # 
  # observed.distribution <- setDT(observed.distribution)
  # observed.distribution <- observed.distribution[order(habitat, strata, PFG, rank)]
  
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

