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
##' @param obs.path the function needs observed data, please create a folder for them in your 
##' simulation folder and then indicate in this parameter the access path to this new folder.
##' @param sim.version name of the simulation we want to validate (it works with
##' only one \code{sim.version}).
##' @param hab.obs file which contain the extended studied map in the simulation.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation.
##' @param strata.considered_PFG.compo a character vector of the list of precise 
##' strata considered in the validation.
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation.
##' @param observed.distribution PFG observed distribution table.
##' @param perStrata.compo Logical. All strata together (FALSE) or per strata (TRUE).
##' @param validation.mask file which contain a raster mask that specified 
##' which pixels need validation.
##' @param year year of simulation to validate.
##' 
##' @details 
##' 
##' After preliminary checks, this code extract observed habitat from the \code{hab.obs}
##' map and, then, merge it with the simulated PFG abundance file from results of a \code{FATE}
##' simulation. After filtration of the required PFG, strata and habitats, the function
##' transform the data into relative metrics and, then, compute distribution per PFG, strata
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
##' @importFrom raster raster projectRaster res crs crop extent origin compareRaster
##' getValues ncell aggregate compareCRS
##' @importFrom utils read.csv write.csv
##' @importFrom dplyr rename filter group_by mutate %>% select
##' @importFrom data.table setDT
##'
### END OF HEADER ##############################################################


do.PFG.composition.validation<-function(name.simulation, obs.path, sim.version, hab.obs, PFG.considered_PFG.compo, strata.considered_PFG.compo, habitat.considered_PFG.compo, observed.distribution, perStrata.compo, validation.mask, year){
  
  output.path = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION/", sim.version)
  name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                   flag = "MASK",
                   flag.split = "^--.*--$",
                   is.num = FALSE) #isolate the access path to the simulation mask for any FATE simulation
  simulation.map = raster(paste0(name))
  hab.obs.modif = projectRaster(from = hab.obs, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
  habitat.FATE.map = crop(hab.obs.modif, simulation.map) #reprojection and croping of the extended habitat map in order to have a reduced observed habitat map
  
  #Auxiliary function to compute proximity (on a 0 to 1 scale, 1 means quantile equality)
  compute.proximity<-function(simulated.quantile,observed.quantile){
    #for a given PFG*habitat*strata, return a "distance", computed as the sum of the absolute gap between observed and simulated quantile
    return(1-sum(abs(simulated.quantile-observed.quantile))/4)
  }
  
  ############################
  # 1. Preliminary checks
  ############################
  
  #check if strata definition used in the RF model is the same as the one used to analyze FATE output
  if(perStrata.compo==F){
    list.strata<-"all"
  }else{
    stop("check 'perStrata' parameter and/or the names of strata in param$list.strata.releves & param$list.strata.simul")
  }
  
  #consistency between habitat.FATE.map and simulation.map
  if(!compareCRS(simulation.map,habitat.FATE.map)){
    print("reprojecting habitat.FATE.map to match simulation.map crs")
    habitat.FATE.map<-projectRaster(habitat.FATE.map,crs=crs(simulation.map))
  }
  if(!all(res(habitat.FATE.map)==res(simulation.map))){
    stop("provide habitat.FATE.map with same resolution as simulation.map")
  }
  if(extent(simulation.map)!=extent(habitat.FATE.map)){
    print("cropping habitat.FATE.map to match simulation.map")
    habitat.FATE.map<-crop(x=habitat.FATE.map,y=simulation.map)
  }
  if(!all(origin(simulation.map)==origin(habitat.FATE.map))){
    print("setting origin habitat.FATE.map to match simulation.map")
    origin(habitat.FATE.map)<-origin(simulation.map)
  }
  if(!compareRaster(simulation.map,habitat.FATE.map)){ #this is crucial to be able to identify pixel by their index and not their coordinates
    stop("habitat.FATE.map could not be coerced to match simulation.map")
  }else{
    print("simulation.map & habitat.FATE.map are (now) consistent")
  }
  
  #adjust validation.mask accordingly
  if(!all(res(habitat.FATE.map)==res(validation.mask))){
    validation.mask<-projectRaster(from=validation.mask,to=habitat.FATE.map,method = "ngb")
  }
  if(extent(validation.mask)!=extent(habitat.FATE.map)){
    validation.mask<-crop(x=validation.mask,y=habitat.FATE.map)
  }
  if(!compareRaster(validation.mask,habitat.FATE.map)){
    stop("error in correcting validation.mask to match habitat.FATE.map")
  }else{
    print("validation.mask is (now) consistent with (modified) habitat.FATE.map")
  }
  
  
  #########################################
  # 2. Get observed habitat
  #########################################
  
  #index of the pixels in the simulation area
  in.region.pixels<-which(getValues(simulation.map)==1)
  
  #habitat df for the whole simulation area
  habitat.whole.area.df<-data.frame(pixel=seq(from=1,to=ncell(habitat.FATE.map),by=1),code.habitat=getValues(habitat.FATE.map),for.validation=getValues(validation.mask))
  habitat.whole.area.df<-filter(habitat.whole.area.df,is.element(pixel,in.region.pixels)&for.validation==1)
  habitat.whole.area.df<-merge(habitat.whole.area.df,dplyr::select(levels(hab.obs)[[1]],c(ID,habitat)),by.x="code.habitat",by.y="ID")
  
  print("Habitat in the simulation area:")
  table(habitat.whole.area.df$habitat,useNA="always")
  
  
  ##############################
  # 3. Loop on simulations
  ##############################
  
  print("processing simulations")
  
  results.simul<-list()
  for(i in 1:length(sim.version)) {
    
    # 3.1. Data preparation
    #########################
    
    #get simulated abundance per pixel*strata*PFG for pixels in the simulation area
    simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
    simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))]
    colnames(simu_PFG) = c("PFG", "pixel", "abs")
    
    #aggregate per strata group with the correspondence provided in input
    simu_PFG$new.strata<-NA
    
    #attribute the "new.strata" value to group FATE strata used in the simulations into strata comparable with CBNA ones (all strata together or per strata)
    if(perStrata.compo==F){
      simu_PFG$new.strata<-"A"
    }
    
    simu_PFG<-dplyr::rename(simu_PFG,"strata"="new.strata")
    
    #agggregate all the rows with same pixel, (new) strata and PFG (necessary since possibly several line with the same pixel+strata+PFG after strata grouping)
    simu_PFG<-aggregate(abs~pixel+strata+PFG,data=simu_PFG,FUN="sum") #sum and not mean because for a given CBNA strata some PFG are present in 2 FATE strata (let's say 1 unit in each) and other are present in 3 FATE strata (let's say one unit in each), so taking the mean would suppress the info that the second PFG is more present!

    # 3.2. Merge with habitat
    ###########################
    
    #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
    simu_PFG<-merge(simu_PFG,habitat.whole.area.df,by="pixel") #at this stage we have all the pixels in the simulation area
    
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
    simu_PFG<-as.data.frame(simu_PFG %>% group_by(pixel,strata) %>% mutate(relative.metric= round(prop.table(abs),digits = 2)))
    simu_PFG$relative.metric[is.na(simu_PFG$relative.metric)]<-0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
    simu_PFG$abs<-NULL
    
    # 3.5. Compute distribution per PFG, and if require per strata/habitat (else all strata/habitat will be considered together)
    ##############################################################################################################################
    
    
    #prepare the df storing quantile values
    simulated.distribution<-expand.grid(
      PFG=PFG.considered_PFG.compo,                              
      habitat=habitat.considered_PFG.compo,
      strata=strata.considered_PFG.compo
    )
    
    null.quantile<-data.frame(rank=seq(0,4,1)) #to have 5 rows per PFG*strata*habitat
    simulated.distribution<-merge(simulated.distribution,null.quantile,all=T)
    
    if(dim(simu_PFG)[1]>0){
      
      distribution<-setDT(simu_PFG)[, quantile(relative.metric), by=c("PFG","habitat","strata")]
      distribution<-rename(distribution,"quantile"="V1")
      distribution<-data.frame(distribution,rank=seq(0,4,1)) #add the rank number
      
      simulated.distribution<-merge(simulated.distribution,distribution,by=c("PFG","habitat","strata","rank"),all.x=T) # add the simulated quantiles, "all.x=T" to keep the unobserved combination (with quantile=NA then)
      
      simulated.distribution$quantile[is.na(simulated.distribution$quantile)]<-0 # "NA" in the previous line means that the corresponding combination PFG*strata*habitat is not present, so as a null relative abundance !
      
    }else{
      simulated.distribution$quantile<-0
    }
    
    simulated.distribution$habitat<-as.character(simulated.distribution$habitat) #else may generate problem in ordering the database
    simulated.distribution$strata<-as.character(simulated.distribution$strata) #else may generate problem in ordering the database
    simulated.distribution$PFG<-as.character(simulated.distribution$PFG) #else may generate problem in ordering the database
    simulated.distribution$rank<-as.numeric(simulated.distribution$rank) #else may generate problem in ordering the database
    
    
    # 3.6. Order the table to be able to have output in the right format
    #####################################################################
    simulated.distribution<-setDT(simulated.distribution)
    simulated.distribution<-simulated.distribution[order(habitat,strata,PFG,rank)]
    
    
    # 3.7. Rename
    ##############
    simulated.distribution<-rename(simulated.distribution,"simulated.quantile"="quantile")
    
    
    # 3.8 Rename and reorder the observed database
    ###############################################
    
    observed.distribution$habitat<-as.character(observed.distribution$habitat) #else may generate problem in ordering the database
    observed.distribution$strata<-as.character(observed.distribution$strata) #else may generate problem in ordering the database
    observed.distribution$PFG<-as.character(observed.distribution$PFG) #else may generate problem in ordering the database
    observed.distribution$rank<-as.numeric(observed.distribution$rank) #else may generate problem in ordering the database
    
    observed.distribution<-setDT(observed.distribution)
    observed.distribution<-observed.distribution[order(habitat,strata,PFG,rank)]
    
    # "if" to check that observed and simulated databases are in the same order
    if(
      !(
        all(simulated.distribution$PFG==observed.distribution$PFG)&
        all(simulated.distribution$habitat==observed.distribution$habitat)&
        all(simulated.distribution$strata==observed.distribution$strata)&
        all(simulated.distribution$rank==observed.distribution$rank)
      )
    ){
      stop("Problem in observed vs simulated database (problem in the PFG*strata*habitat considered or in the database order)")
    }
    
    # 3.9. Merge observed and simulated data
    #########################################
    
    simulated.distribution<-cbind(simulated.distribution,observed.quantile=observed.distribution$observed.quantile) #quicker than a merge, but we can do it only because we have worked on the order of the DT
    
    # 3.10 Compute proximity between observed and simulated data, per PFG*strata*habitat
    #####################################################################################
    
    #we get rid off rank==0 because there is good chance that it is nearly always equal to zero both in observed and simulated data, and that would provide a favorable bias in the results
    
    simulated.distribution<-filter(simulated.distribution,rank!=0)
    
    proximity<-simulated.distribution[,compute.proximity(simulated.quantile=simulated.quantile,observed.quantile=observed.quantile),by=c("PFG","habitat","strata")]
    
    
    proximity<-rename(proximity,"proximity"="V1")
    
    proximity<-proximity[order(habitat,strata,PFG)] #to have output in the same order for all simulations
    
    
    # 3.11. Aggregate results for the different PFG
    ################################################
    
    aggregated.proximity<-proximity[,mean(proximity),by=c("habitat","strata")]
    aggregated.proximity<-rename(aggregated.proximity,"aggregated.proximity"="V1")
    aggregated.proximity$aggregated.proximity<-round(aggregated.proximity$aggregated.proximity,digits=2)
    aggregated.proximity$simul<-sim.version
    
    # return(aggregated.proximity)
    
    #line added because the foreach method does not work
    results.simul[[i]]<-aggregated.proximity
    
  }
  
  # 4. Put in the output format
  ##############################
  
  results<-sapply(results.simul,function(X){X$aggregated.proximity})
  rownames(results)<-paste0(results.simul[[1]]$habitat,"_",results.simul[[1]]$strata)
  colnames(results)<-sim.version
  results<-t(results)
  results<-as.data.frame(results)
  results$simulation<-rownames(results)
  
  #save and return
  write.csv(results,paste0(output.path,"/performance.composition.csv"),row.names = F)
  
  return(results)
}

