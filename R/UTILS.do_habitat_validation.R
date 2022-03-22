### HEADER #####################################################################
##'
##' @title Compare observed and simulated habitat of a \code{FATE} simulation
##' at the last simulation year.
##' 
##' @name do.habitat.validation
##' 
##' @author Matthieu Combaud & Maxime Delprat
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
##' @param predict.all.map \code{Logical}. If TRUE, the script will predict 
##' habitat for the whole map.
##' @param sim.version name of the simulation to validate.
##' @param name.simulation simulation folder name.
##' @param perStrata \code{Logical}. If TRUE, the PFG abundance is defined
##' by strata in each pixel. If FALSE, PFG abundance is defined for all strata.
##' @param hab.obs a raster map of the observed habitat in the
##' extended studied area.
##' @param year simulation year selected for validation.
##' @param list.strata.releves a character vector which contain the observed strata 
##' definition, extracted from observed PFG releves.
##' @param list.strata.simulations a character vector which contain \code{FATE} 
##' strata definition and correspondence with observed strata definition.
##' 
##' @details
##' 
##' After several preliminary checks, the function is going to prepare the observations
##' database by extracting the observed habitat from a raster map. Then, for the
##' simulation \code{sim.version}, the script take the evolution abundance for each PFG
##' and all strata (or for each PFG & each strata if option selected) file and predict 
##' the habitat for the whole map (if option selected) thanks to the RF model. 
##' Finally, the function computes habitat performance based on TSS for each habitat.
##' 
##' @return
##' 
##' Habitat performance file. \cr
##' If option selected, the function also returns an habitat prediction file with 
##' observed and simulated habitat for each pixel of the whole map.
##' 
##' @export
##' 
##' @importFrom dplyr filter rename group_by %>% mutate rename select
##' @importFrom raster compareCRS res projectRaster extent crop origin compareRaster 
##' getValues predict levels
##' @importFrom stats aggregate
##' @importFrom stringr str_sub
##' @importFrom foreach foreach %dopar%
##' @importFrom reshape2 dcast
##' @importFrom caret confusionMatrix
##' @importFrom utils write.csv
##' @importFrom doParallel registerDoParallel
##' @importFrom parallel detectCores
##' @importFrom tidyselect all_of
##' 
### END OF HEADER ##############################################################


do.habitat.validation<-function(output.path, RF.model, habitat.FATE.map, validation.mask
                                , simulation.map, predict.all.map, sim.version, name.simulation
                                , perStrata, hab.obs, year, list.strata.releves, list.strata.simulations)
{
  
  cat("\n ---------- FATE OUTPUT ANALYSIS \n")
  
  #notes
  # we prepare the relevé data in this function, but in fact we could provide them directly if we adjust the code
  
  ###########################
  #I. Preliminary checks
  ###########################
  
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
  
  #consistency between habitat.FATE.map and simulation.map
  ## MUST BE DONE before
  # if(!compareCRS(simulation.map,habitat.FATE.map)){
  #   print("reprojecting habitat.FATE.map to match simulation.map crs")
  #   habitat.FATE.map<-projectRaster(habitat.FATE.map,crs=crs(simulation.map))
  # }
  if(!all(res(habitat.FATE.map)==res(simulation.map))){
    stop("provide habitat.FATE.map with same resolution as simulation.map")
  }
  if(extent(simulation.map) != extent(habitat.FATE.map)){
    print("cropping habitat.FATE.map to match simulation.map")
    habitat.FATE.map = crop(x = habitat.FATE.map, y = simulation.map)
  }
  ## MUST BE DONE before
  # if(!all(origin(simulation.map)==origin(habitat.FATE.map))){
  #   print("setting origin habitat.FATE.map to match simulation.map")
  #   raster::origin(habitat.FATE.map) <- raster::origin(simulation.map)
  # }
  if(!compareRaster(simulation.map,habitat.FATE.map)){ #this is crucial to be able to identify pixel by their index and not their coordinates
    stop("habitat.FATE.map could not be coerced to match simulation.map")
  }else{
    print("simulation.map & habitat.FATE.map are (now) consistent")
  }
  
  #adjust validation.mask accordingly
  ## MUST BE DONE before ?
  # if(!all(res(habitat.FATE.map)==res(validation.mask))){
  #   validation.mask<-projectRaster(from=validation.mask,to=habitat.FATE.map,method = "ngb")
  # }
  if(extent(validation.mask)!=extent(habitat.FATE.map)){
    validation.mask<-crop(x=validation.mask,y=habitat.FATE.map)
  }
  if(!compareRaster(validation.mask, habitat.FATE.map)){
    stop("error in correcting validation.mask to match habitat.FATE.map")
  }else{
    print("validation.mask is (now) consistent with (modified) habitat.FATE.map") ## TODO : change message
  }
  
  #check consistency for PFG & strata classes between FATE output vs the RF model

  RF.predictors <- rownames(RF.model$importance)
  RF.PFG <- unique(str_sub(RF.predictors, 1, 2))
  
  FATE.PFG<-str_sub(list.files(paste0(name.simulation,"/DATA/PFGS/SUCC")),6,7) ## TODO : careful, will not match necessarily all PFG names
  
  if(length(setdiff(FATE.PFG,RF.PFG)) > 0 | length(setdiff(RF.PFG,FATE.PFG)) > 0){
    stop("The PFG used to train the RF algorithm are not the same as the PFG used to run FATE.")
  }
  
  
  #########################################################################################
  #II. Prepare database for FATE habitat
  #########################################################################################
  
  #habitat df for the whole simulation area
  habitat.whole.area.df <- data.frame(pixel = seq(1, ncell(habitat.FATE.map), 1)
                                      , code.habitat = getValues(habitat.FATE.map)
                                      , for.validation = getValues(validation.mask))
  habitat.whole.area.df <- habitat.whole.area.df[which(getValues(simulation.map) == 1), ] #index of the pixels in the simulation area
  habitat.whole.area.df <- habitat.whole.area.df[which(!is.na(habitat.whole.area.df$for.validation)), ] 
  habitat.whole.area.df <- merge(habitat.whole.area.df, dplyr::select(levels(hab.obs)[[1]],c(ID,habitat)), by.x = "code.habitat", by.y = "ID")
  habitat.whole.area.df <- habitat.whole.area.df[which(habitat.whole.area.df$habitat %in% RF.model$classes), ]
  
  print(cat("Habitat considered in the prediction exercise: ", c(unique(habitat.whole.area.df$habitat)), "\n", sep = "\t"))
  
  print("Habitat in the simulation area:")
  table(habitat.whole.area.df$habitat, useNA = "always")
  
  print("Habitat in the subpart of the simulation area used for validation:")
  table(habitat.whole.area.df$habitat[habitat.whole.area.df$for.validation == 1], useNA = "always")
  
  ##############################
  # III. Loop on simulations
  ##############################
  
  print("processing simulations")
  
  
  # registerDoParallel(detectCores()-2) ## TODO : put as optional (like in zip/unzip function)
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
  results.simul <- foreach(i = 1:length(all_of(sim.version))) %dopar% 
    {
      
      ########################"
      # III.1. Data preparation
      #########################
      
      #get simulated abundance per pixel*strata*PFG for pixels in the simulation area
      if (perStrata == FALSE) {
        ## TODO : add test if file exists
        if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv")))
        {
          simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))] #keep only the PFG, ID.pixel and abundance at any year columns
          #careful : the number of abundance data files to save is to defined in POST_FATE.temporal.evolution function
          colnames(simu_PFG) = c("PFG", "pixel", "abs")
          simu_PFG$strata <- "A"
        }else
        {
          stop("Simulated abundance file does not exist")
        }
        
      } else if (perStrata == TRUE) {
        ## TODO : add test if file exists
        if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv")))
        {
          simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv"))
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
      
      ## SIMILAR to what was done in train_RF_habitat for the releves
      #aggregate all the rows with same pixel, (new) strata and PFG (necessary since possibly several line with the same pixel+strata+PFG after strata grouping)
      simu_PFG <- aggregate(abs ~ pixel + strata + PFG, data = simu_PFG, FUN = "sum")
      
      #transform absolute abundance into relative abundance (no pb if all combination PFG*strata are not present, since then the value is 0!)
      simu_PFG <- simu_PFG %>% group_by(pixel,strata) %>% mutate(relative.abundance = round(prop.table(abs), digits = 2)) #those are proportions, not percentages
      simu_PFG$relative.abundance[is.na(simu_PFG$relative.abundance)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (necessary to avoid risk of confusion with NA in pixels because out of the map)
      simu_PFG <- as.data.frame(simu_PFG)
      
      #drop the absolute abundance
      simu_PFG$abs<-NULL
      
      #correct the levels (to have all PFG and all strata) to make the dcast transfo easier (all PFG*strata combination will be automatically created thanks to the factor structure, even if no line corresponds to it)
      simu_PFG$PFG<-as.factor(simu_PFG$PFG)
      simu_PFG$PFG <- factor(simu_PFG$PFG, sort(unique(c(levels(simu_PFG$PFG), RF.PFG))))
      simu_PFG$strata<-as.factor(simu_PFG$strata)
      simu_PFG$PFG <- factor(simu_PFG$PFG, sort(unique(c(levels(simu_PFG$strata), list.strata))))
      
      #cast
      simu_PFG<-reshape2::dcast(simu_PFG, pixel ~ PFG * strata, value.var = c("relative.abundance"), fill = 0, drop = FALSE)
      
      #merge PFG info and habitat + transform habitat into factor
      
      #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
      data.FATE.PFG.habitat <- merge(simu_PFG, habitat.whole.area.df, by = "pixel") #at this stage we have all the pixels in the simulation area
      data.FATE.PFG.habitat$habitat <- factor(data.FATE.PFG.habitat$habitat, levels = RF.model$classes) #thanks to the "levels" argument, we have the same order for the habitat factor in the RF model and in the FATE outputs
      
      ############################
      # III.2. Prediction of habitat with the RF algorithm
      #################################
      
      data.validation <- data.FATE.PFG.habitat[which(data.FATE.PFG.habitat$for.validation == 1), ]
      x.validation <- dplyr::select(data.validation,all_of(RF.predictors)) ## TODO : change for classic colnames selection but with error message if not fullfilling all names ?
      y.validation <- data.validation$habitat
      
      y.validation.predicted <- predict(object = RF.model, newdata = x.validation, type = "response", norm.votes = TRUE)
      
      ##############################
      # III.3. Analysis of the results
      ################################
      
      confusion.validation <- confusionMatrix(data = y.validation.predicted
                                              , reference = factor(y.validation, sort(unique(c(levels(y.validation), levels(y.validation.predicted))))))
      
      synthesis.validation <- data.frame(habitat = colnames(confusion.validation$table)
                                         , sensitivity = confusion.validation$byClass[, 1]
                                         , specificity = confusion.validation$byClass[, 2]
                                         , weight = colSums(confusion.validation$table) / sum(colSums(confusion.validation$table)))
      synthesis.validation <- synthesis.validation %>% mutate(TSS = round(sensitivity + specificity - 1, digits = 2))
      
      aggregate.TSS.validation <- round(sum(synthesis.validation$weight * synthesis.validation$TSS, na.rm = TRUE), digits = 2)
      
      ########################
      # III.4. Predict habitat for the whole map if option selected (do it only for a small number of simulations)
      ############################################
      
      if (predict.all.map == TRUE) {
        y.all.map.predicted = predict(object = RF.model, newdata =  dplyr::select(data.FATE.PFG.habitat, all_of(RF.predictors)), type = "response", norm.votes = TRUE)
        y.all.map.predicted = as.data.frame(y.all.map.predicted)
        y.all.map.predicted$pixel = data.FATE.PFG.habitat$pixel
        colnames(y.all.map.predicted) = c(sim.version, "pixel")
      } else {
        y.all.map.predicted <- NULL
      }
      
      #prepare outputs
      output.validation <- c(synthesis.validation$TSS, aggregate.TSS.validation)
      names(output.validation) <- c(synthesis.validation$habitat, "aggregated")
      
      return(list(output.validation = output.validation
                  , y.all.map.predicted = y.all.map.predicted))
    }
  #end of the loop on simulations
  
  #deal with the results regarding model performance
  habitat.performance <- as.data.frame(matrix(unlist(lapply(results.simul, "[[", 1)), ncol = length(RF.model$classes) + 1, byrow = TRUE))
  names(habitat.performance) <- c(RF.model$classes, "weighted")
  habitat.performance$simulation <- sim.version
  
  #save
  write.csv(habitat.performance, paste0(output.path, "/HABITAT/", sim.version, "/performance.habitat.csv"), row.names = FALSE)
  
  print("habitat performance saved")
  
  #deal with the results regarding habitat prediction over the whole map
  all.map.prediction = results.simul[[1]]$y.all.map.predicted
  all.map.prediction = merge(all.map.prediction, dplyr::select(habitat.whole.area.df, c(pixel,habitat)), by = "pixel")
  all.map.prediction = rename(all.map.prediction, "true.habitat" = "habitat")
  
  #save
  write.csv(all.map.prediction,paste0(output.path,"/HABITAT/", sim.version, "/habitat.prediction.csv"), row.names = FALSE)
  
  #return results
  return(all.map.prediction)
  
}

