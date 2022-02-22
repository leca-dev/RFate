### HEADER #####################################################################
##'
##' @title Compare observed and simulated habitat of a \code{FATE} simulation
##' at the last simulation year.
##' 
##' @name do.habitat.validation
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description To compare observations and simulations, this function compute 
##' confusion matrix between observation and prediction and then compute the TSS 
##' for each habitat.
##' 
##' @param output.path access path to the for the folder where output files
##' will be created.
##' @param RF.model random forest model trained on CBNA data (train.RF.habitat
##' function)
##' @param habitat.FATE.map a raster map of the observed habitat in the
##' studied area.
##' @param validation.mask a raster mask that specified which pixels need validation.
##' @param simulation.map a raster map of the whole studied area use to check
##' the consistency between simulation map and the observed habitat map.
##' @param predict.all.map a TRUE/FALSE vector. If TRUE, the script will predict 
##' habitat for the whole map.
##' @param sim.version name of the simulation we want to validate.
##' @param name.simulation simulation folder name.
##' @param perStrata a TRUE/FALSE vector. If TRUE, the PFG abundance is defined
##' by strata in each pixel. If FALSE, PFG abundance is defined for all strata.
##' @param hab.obs a raster map of the observed habitat in the
##' extended studied area.
##' 
##' @details
##' 
##' After several preliminary checks, the function is going to prepare the observations
##' database by extracting the observed habitat from a raster map. Then, for each
##' simulations (sim.version), the script take the evolution abundance for each PFG
##' and all strata file and predict the habitat for the whole map (if option selected) 
##' thanks to the RF model.Finally, the function compute habitat performance based on
##' TSS for each habitat.
##' 
##' @return
##' 
##' Habitat performance file
##' If option selected, the function returns an habitat prediction file with 
##' observed and simulated habitat for each pixel of the whole map.
##' 
### END OF HEADER ##############################################################


do.habitat.validation<-function(output.path, RF.model, habitat.FATE.map, validation.mask, simulation.map, predict.all.map, sim.version, name.simulation, perStrata, hab.obs) {
  
  #notes
  # we prepare the relevé data in this function, but in fact we could provide them directly if we adjust the code
  
  ###########################
  #I. Preliminary checks
  ###########################
  
  #check if strata definition used in the RF model is the same as the one used to analyze FATE output
  if(perStrata==F){
    list.strata<-"all"
  }else{
    stop("check 'perStrata' parameter and/or the names of strata in param$list.strata.releves & param$list.strata.simul")
  }
  
  #initial consistency between habitat.FATE.map and validation.mask (do it before the adjustement of habitat.FATE.map)
  if(!compareCRS(habitat.FATE.map,validation.mask) | !all(res(habitat.FATE.map)==res(validation.mask))){
    stop("please provide rasters with same crs and resolution for habitat.FATE.map and validation.mask")
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
  
  #check consistency for PFG & strata classes between FATE output vs the RF model
  
  RF.predictors<-rownames(RF.model$importance)
  RF.PFG<-unique(str_sub(RF.predictors,1,2))
  
  FATE.PFG<-str_sub(list.files(paste0(name.simulation,"/DATA/PFGS/SUCC")),6,7)
  
  if(length(setdiff(FATE.PFG,RF.PFG))>0|length(setdiff(RF.PFG,FATE.PFG))>0){
    stop("The PFG used to train the RF algorithm are not the same as the PFG used to run FATE.")
  }
  
  
  #########################################################################################
  #II. Prepare database for FATE habitat
  #########################################################################################
  
  #index of the pixels in the simulation area
  in.region.pixels<-which(getValues(simulation.map)==1)
  
  #habitat df for the whole simulation area
  habitat.whole.area.df<-data.frame(pixel=seq(from=1,to=ncell(habitat.FATE.map),by=1),code.habitat=getValues(habitat.FATE.map),for.validation=getValues(validation.mask))
  habitat.whole.area.df<-habitat.whole.area.df[in.region.pixels,]
  habitat.whole.area.df<-subset(habitat.whole.area.df, for.validation!="NA")
  habitat.whole.area.df<-merge(habitat.whole.area.df,dplyr::select(levels(hab.obs)[[1]],c(ID,habitat)),by.x="code.habitat",by.y="ID")
  habitat.whole.area.df<-filter(habitat.whole.area.df,is.element(habitat,RF.model$classes))
  
  print(cat("Habitat considered in the prediction exercise: ",c(unique(habitat.whole.area.df$habitat)),"\n",sep="\t"))
  
  print("Habitat in the simulation area:")
  table(habitat.whole.area.df$habitat,useNA="always")
  
  print("Habitat in the subpart of the simulation area used for validation:")
  table(habitat.whole.area.df$habitat[habitat.whole.area.df$for.validation==1],useNA="always")
  
  ##############################
  # III. Loop on simulations
  #########################
  
  print("processing simulations")
  
  registerDoParallel(detectCores()-2)
  results.simul <- foreach(i=1:length(sim.version),.packages = c("dplyr","forcats","reshape2","randomForest","vcd","caret")) %dopar%{
    
    ########################"
    # III.1. Data preparation
    #########################
    
    #get simulated abundance per pixel*strata*PFG for pixels in the simulation area
    simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
    simu_PFG = simu_PFG[,-c(3:44)]
    colnames(simu_PFG) = c("PFG", "pixel", "abs")
    
    #aggregate per strata group with the correspondance provided in input
    simu_PFG$new.strata<-NA
    
    #attribute the "new.strata" value to group FATE strata used in the simulations into strata comparable with CBNA ones (all strata together or per strata)
    if(perStrata==F){
      simu_PFG$new.strata<-"A"
    }
    
    simu_PFG<-dplyr::rename(simu_PFG,"strata"="new.strata")
    
    #aggregate all the rows with same pixel, (new) strata and PFG (necessary since possibly several line with the same pixel+strata+PFG after strata grouping)
    simu_PFG<-aggregate(abs~pixel+strata+PFG,data=simu_PFG,FUN="sum")
    
    #transform absolute abundance into relative abundance (no pb if all combination PFG*strata are not present, since then the value is 0!)
    simu_PFG<-simu_PFG %>% group_by(pixel,strata) %>% mutate(relative.abundance= round(prop.table(abs),digits=2)) #those are proportions, not percentages
    simu_PFG$relative.abundance[is.na(simu_PFG$relative.abundance)]<-0 #NA because abs==0 for some PFG, so put 0 instead of NA (necessary to avoid risk of confusion with NA in pixels because out of the map)
    simu_PFG<-as.data.frame(simu_PFG)
    
    #drop the absolute abundance
    simu_PFG$abs<-NULL
    
    #set a factor structure
    simu_PFG$PFG<-as.factor(simu_PFG$PFG)
    simu_PFG$strata<-as.factor(simu_PFG$strata)
    
    #correct the levels (to have all PFG and all strata) to make the dcast transfo easier (all PFG*strata combination will be automatically created thanks to the factor structure, even if no line corresponds to it)
    simu_PFG$PFG<-fct_expand(simu_PFG$PFG,RF.PFG)
    simu_PFG$strata<-fct_expand(simu_PFG$strata,list.strata)
    
    #cast
    simu_PFG<-dcast(simu_PFG,pixel~PFG*strata,value.var=c("relative.abundance"),fill=0,drop=F)
    
    #merge PFG info and habitat + transform habitat into factor
    
    #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
    data.FATE.PFG.habitat<-merge(simu_PFG,habitat.whole.area.df,by="pixel") #at this stage we have all the pixels in the simulation area
    data.FATE.PFG.habitat$habitat<-factor(data.FATE.PFG.habitat$habitat,levels=RF.model$classes) #thanks to the "levels" argument, we have the same order for the habitat factor in the RF model and in the FATE outputs
    
    ############################
    # III.2. Prediction of habitat with the RF algorithm
    #################################
    
    data.validation<-filter(data.FATE.PFG.habitat,for.validation==1)
    x.validation<-select(data.validation,all_of(RF.predictors))
    y.validation<-data.validation$habitat
    
    y.validation.predicted<-predict(object=RF.model,newdata=x.validation,type="response",norm.votes=T)
    
    ##############################
    # III.3. Analysis of the results
    ################################
    
    confusion.validation<-confusionMatrix(data=y.validation.predicted,reference=fct_expand(y.validation,levels(y.validation.predicted)))
    
    synthesis.validation<-data.frame(habitat=colnames(confusion.validation$table),sensitivity=confusion.validation$byClass[,1],specificity=confusion.validation$byClass[,2],weight=colSums(confusion.validation$table)/sum(colSums(confusion.validation$table)))
    synthesis.validation<-synthesis.validation%>%mutate(TSS=round(sensitivity+specificity-1,digits=2))
    
    aggregate.TSS.validation<-round(sum(synthesis.validation$weight*synthesis.validation$TSS,na.rm=T),digits=2)
    
    ########################
    # III.4. Predict habitat for the whole map if option selected (do it only for a small number of simulations)
    ############################################
    
    if(predict.all.map==T){
      
      y.all.map.predicted = predict(object=RF.model,newdata=select(data.FATE.PFG.habitat,all_of(RF.predictors)),type="response",norm.votes=T)
      y.all.map.predicted = as.data.frame(y.all.map.predicted)
      y.all.map.predicted$pixel = data.FATE.PFG.habitat$pixel
      colnames(y.all.map.predicted) = c(sim.version, "pixel")
      
    }else{
      y.all.map.predicted<-NULL
    }
    
    #prepare outputs
    
    output.validation<-c(synthesis.validation$TSS,aggregate.TSS.validation)
    names(output.validation)<-c(synthesis.validation$habitat,"aggregated")
    
    output<-list(output.validation,y.all.map.predicted)
    names(output)<-c("output.validation","y.all.map.predicted")
    
    return(output)
  }
  #end of the loop on simulations
  
  #deal with the results regarding model performance
  habitat.performance<-as.data.frame(matrix(unlist(lapply(results.simul,"[[",1)),ncol=length(RF.model$classes)+1,byrow=T))
  names(habitat.performance)<-c(RF.model$classes,"weighted")
  habitat.performance$simulation<-sim.version
  
  #save
  write.csv(habitat.performance,paste0(output.path,"/HABITAT/", sim.version, "/performance.habitat.csv"),row.names=F)
  
  print("habitat performance saved")
  
  #deal with the results regarding habitat prediction over the whole map
  all.map.prediction = results.simul[[1]]$y.all.map.predicted
  all.map.prediction = merge(all.map.prediction, select(habitat.whole.area.df, c(pixel,habitat)), by = "pixel")
  all.map.prediction = rename(all.map.prediction,"true.habitat"="habitat")
  
  #save
  write.csv(all.map.prediction,paste0(output.path,"/HABITAT/", sim.version, "/habitat.prediction.csv"), row.names=F)
  
  #return results
  return(all.map.prediction)
  
}

