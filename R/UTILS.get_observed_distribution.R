### HEADER #####################################################################
##'
##' @title Compute distribution of relative abundance over observed relevés
##'
##' @name get.observed.distribution
##'
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to compute distribution, per PFG/strata/habitat,
##' of relative abundance, from observed data.
##' 
##' @param name.simulation simulation folder name.
##' @param obs.path the function needs observed data, please create a folder for them in your 
##' simulation folder and then indicate in this parameter the access path to this new folder.
##' @param releves.PFG file which contain the observed Braund-Blanquet abundance at each site
##' and each PFG and strata.
##' @param releves.sites file which contain coordinates and a description of
##' the habitat associated with the dominant species of each site in the studied map.
##' @param hab.obs raster map of the extended studied area in the simulation.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation.
##' @param strata.considered_PFG.compo a character vector of the list of precise 
##' strata considered in the validation.
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation.
##' @param perStrata \code{Logical}. All strata together (FALSE) or per strata (TRUE).
##' @param sim.version name of the simulation we want to validate (it works with
##' only one \code{sim.version}).
##' 
##' @details
##' 
##' The function takes the \code{releves.PFG} and \code{releves.sites} files and 
##' aggregate coverage per PFG. Then, the code get habitat information from also
##' the \code{hab.obs} map, keep only interesting habitat, strata and PFG, and
##' transform the data into relative metrics. Finally, the script computes distribution
##' per PFG, and if require per strata/habitat (else all strata/habitat will be considered together).
##' 
##' @return 
##' 
##' 2 files are created in
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION/sim.version} :
##'   1 .csv file which contain the observed relevés transformed into relative metrics.
##'   1 .csv file which contain the final output with the distribution per PFG, strata and habitat.
##'   
##' @export
##' 
##' @importFrom dplyr select filter group_by mutate %>% rename
##' @importFrom raster compareCRS res crs levels
##' @importFrom stats aggregate
##' @importFrom sf st_transform st_crop
##' @importFrom utils write.csv
##' @importFrom data.table setDT
##' 
### END OF HEADER ##############################################################


get.observed.distribution<-function(name.simulation
                                    , obs.path
                                    , releves.PFG
                                    , releves.sites
                                    , hab.obs
                                    , PFG.considered_PFG.compo
                                    , strata.considered_PFG.compo
                                    , habitat.considered_PFG.compo
                                    , perStrata
                                    , sim.version){
  
  cat("\n ---------- GET OBSERVED DISTRIBUTION \n")
  
  composition.mask = NULL
  output.path = paste0(name.simulation, "/VALIDATION/PFG_COMPOSITION/", sim.version)
  dir.create(file.path(output.path), recursive = TRUE, showWarnings = FALSE)
  
  #1. Aggregate coverage per PFG
  #########################################
  
  #identify sites with wrong BB values (ie values that cannot be converted by the PRE_FATE.abundBraunBlanquet function)
  releves.PFG<-filter(releves.PFG,is.element(BB,c(NA, "NA", 0, "+", "r", 1:5)))
  
  #transformation into coverage percentage
  releves.PFG$coverage<-PRE_FATE.abundBraunBlanquet(releves.PFG$BB)/100 #as a proportion, not a percentage
  
  if(perStrata==T){
    aggregated.releves.PFG<-aggregate(coverage~site+PFG+strata,data=releves.PFG,FUN="sum")
  }else if(perStrata==F){
    aggregated.releves.PFG<-aggregate(coverage~site+PFG,data=releves.PFG,FUN="sum")
    aggregated.releves.PFG$strata<-"A" #"A" is for "all". Important to have a single-letter code here (useful to check consistency between relevés strata and model strata)
  }
  
  
  #2. Get habitat information
  ###################################
  
  #get sites coordinates
  aggregated.releves.PFG<-merge(dplyr::select(releves.sites,c(site)),aggregated.releves.PFG,by="site")
  
  #get habitat code and name
  if(compareCRS(aggregated.releves.PFG,hab.obs)){
    aggregated.releves.PFG$code.habitat<-raster::extract(x=hab.obs,y=aggregated.releves.PFG)
  }else{
    aggregated.releves.PFG<-st_transform(x=aggregated.releves.PFG,crs=crs(hab.obs))
    aggregated.releves.PFG$code.habitat<-raster::extract(x=hab.obs,y=aggregated.releves.PFG)
  }
  
  #correspondance habitat code/habitat name
  table.habitat.releve<-levels(hab.obs)[[1]]
  
  aggregated.releves.PFG<-merge(aggregated.releves.PFG,dplyr::select(table.habitat.releve,c(ID,habitat)),by.x="code.habitat",by.y="ID") 
  
  #(optional) keep only releves data in a specific area
  if(!is.null(composition.mask)){
    
    if(compareCRS(aggregated.releves.PFG,composition.mask)==F){ #as this stage it is not a problem to transform crs(aggregated.releves.PFG) since we have no more merge to do (we have already extracted habitat info from the map)
      aggregated.releves.PFG<-st_transform(x=aggregated.releves.PFG,crs=crs(composition.mask))
    }
    
    aggregated.releves.PFG<-st_crop(x=aggregated.releves.PFG,y=composition.mask)
    print("'releve' map has been cropped to match 'external.training.mask'.")
  }
  
  
  # 3. Keep only releve on interesting habitat, strata and PFG
  ##################################################################"
  
  aggregated.releves.PFG<-as.data.frame(aggregated.releves.PFG)
  aggregated.releves.PFG<-dplyr::select(aggregated.releves.PFG,c(site,PFG,strata,coverage,habitat))
  
  aggregated.releves.PFG<-filter(
    aggregated.releves.PFG,
    is.element(PFG,PFG.considered_PFG.compo)&
      is.element(strata,strata.considered_PFG.compo)&
      is.element(habitat,habitat.considered_PFG.compo)
  )
  
  
  #4.Transform into a relative metrics (here relative.metric is relative coverage)
  ###################################################################################
  
  #important to do it only here, because if we filter some PFG, it changes the value of the relative metric
  #careful: if several strata are selected, the computation is made for each strata separately
  aggregated.releves.PFG<-as.data.frame(aggregated.releves.PFG %>% group_by(site,strata) %>% mutate(relative.metric= round(prop.table(coverage),digits = 2)))
  aggregated.releves.PFG$relative.metric[is.na(aggregated.releves.PFG$relative.metric)]<-0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  aggregated.releves.PFG$coverage<-NULL
  
  print("releve data have been transformed into a relative metric")
  
  
  # 5. Save data
  #####################
  write.csv(aggregated.releves.PFG,paste0(output.path,"/CBNA.releves.prepared.csv"),row.names = F)
  
  
  # 6. Compute distribution per PFG, and if require per strata/habitat (else all strata/habitat will be considered together)
  ####################################
  
  distribution<-setDT(aggregated.releves.PFG)[, quantile(relative.metric), by=c("PFG","habitat","strata")]
  distribution<-rename(distribution,"quantile"="V1")
  distribution<-data.frame(distribution,rank=seq(0,4,1)) #to be able to sort on quantile
  
  # 7. Add the missing PFG*habitat*strata
  #final distribution is the distribution once the missing combination have been added. For these combination, all quantiles are set to 0
  
  observed.distribution<-expand.grid(
    PFG=PFG.considered_PFG.compo,                              
    habitat=habitat.considered_PFG.compo,
    strata=strata.considered_PFG.compo
  )
  
  null.quantile<-data.frame(rank=seq(0,4,1)) #to have 5 rows per PFG*strata*habitat
  observed.distribution<-merge(observed.distribution,null.quantile,all=T)
  
  observed.distribution<-merge(observed.distribution,distribution,by=c("PFG","habitat","strata","rank"),all.x=T) # "all.x=T" to keep the unobserved combination
  
  observed.distribution$quantile[is.na(observed.distribution$quantile)]<-0
  
  # 8. Order the table to be able to have output in the right format
  observed.distribution<-setDT(observed.distribution)
  observed.distribution<-observed.distribution[order(habitat,strata,PFG,rank)]
  
  observed.distribution<-rename(observed.distribution,"observed.quantile"="quantile")
  
  
  # 9. Save results
  ##########################################
  write.csv(observed.distribution,paste0(output.path,"/observed.distribution.csv"),row.names = F)
  
  # 8. Return
  ####################
  
  return(observed.distribution)
  
}
