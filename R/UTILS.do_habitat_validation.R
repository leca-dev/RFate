#################################################################
##' @importFrom dplyr group_by %>% mutate rename select
##' @importFrom raster predict
##' @importFrom reshape2 dcast
##' @importFrom caret confusionMatrix
##' @importFrom utils write.csv
##' @importFrom tidyselect all_of
## @importFrom stringr str_split
#################################################################

do_habitat_validation <- function(output.path,
                                  RF.model, 
                                  PFG_names, 
                                  predict.all.map,
                                  sim,
                                  simu_PFG,
                                  hab.whole.df, 
                                  list.strata)
                                  # perStrata)
{
  
  # check consistency for PFG & strata classes between FATE output vs the RF model
  RF.predictors <- rownames(RF.model$importance)
  RF.PFG = unique(sapply(RF.predictors, function(x) strsplit(x, "_")[[1]][1]))
  # FATE.PFG = PFG_names
  # 
  # if (length(setdiff(FATE.PFG,RF.PFG)) > 0 || length(setdiff(RF.PFG,FATE.PFG)) > 0) {
  #   warning("PROBLEM") ## TODO
  #   # cat(paste0("\n > Warning : The PFG used to train the RF algorithm are not the same as the PFG used to run FATE ! The PFG "
  #   #            , setdiff(FATE.PFG,RF.PFG), " will be removed from the analyses"))
  #   # FATE.PFG = RF.PFG = intersect(FATE.PFG, RF.PFG)
  # }
  # toKeep.PFG = intersect(FATE.PFG, RF.PFG)

  #######################
  # I. Data preparation
  #######################
  
  # #transform absolute abundance into relative abundance
  # simu_PFG = simu_PFG[which(simu_PFG$PFG %in% toKeep.PFG), ]
  # # mat.PFG.agg = as.data.frame(
  # #   mat.PFG.agg %>% group_by(site, strata) %>% 
  # #     mutate(relative.metric = round(prop.table(abund), digits = 2))
  # # )
  # simu_PFG = as.data.frame(
  #   simu_PFG %>% group_by(pixel, strata) %>% 
  #     mutate(relative.metric = round(prop.table(abs), digits = 2))
  # )
  # simu_PFG$relative.metric[is.na(simu_PFG$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (necessary to avoid risk of confusion with NA in pixels because out of the map)
  # simu_PFG$abs <- NULL
  
  #transfo into factor to be sure to create all the combination when doing "dcast"
  #correct the levels (to have all PFG and all strata) to make the dcast transfo easier (all PFG*strata combination will be automatically created thanks to the factor structure, even if no line corresponds to it)
  simu_PFG$PFG <- as.factor(simu_PFG$PFG)
  # simu_PFG$PFG <- factor(simu_PFG$PFG, sort(unique(c(levels(simu_PFG$PFG), RF.PFG))))
  simu_PFG$strata <- as.factor(simu_PFG$strata)
  # simu_PFG$strata <- factor(simu_PFG$strata, sort(unique(c(levels(simu_PFG$strata), list.strata))))
  simu_PFG <- reshape2::dcast(simu_PFG, pixel ~ PFG * strata, value.var = "relative.metric", fill = 0, drop = FALSE)
  
  #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
  data.FATE.PFG.habitat <- merge(simu_PFG, hab.whole.df, by = "pixel") #at this stage we have all the pixels in the simulation area
  data.FATE.PFG.habitat$habitat <- factor(data.FATE.PFG.habitat$habitat, levels = RF.model$classes) #thanks to the "levels" argument, we have the same order for the habitat factor in the RF model and in the FATE outputs
  
  ############################
  # II. Prediction of habitat with the RF algorithm
  #################################
  
  data.validation <- data.FATE.PFG.habitat[which(data.FATE.PFG.habitat$for.validation == 1), ]
  x.validation <- dplyr::select(data.validation,all_of(RF.predictors))
  y.validation <- data.validation$habitat
  
  y.validation.predicted <- predict(object = RF.model, newdata = x.validation, type = "response", norm.votes = TRUE)
  
  ##############################
  # III. Analysis of the results
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
  # IV. Predict habitat for the whole map if option selected (do it only for a small number of simulations)
  ############################################
  
  if (predict.all.map == TRUE) {
    y.all.map.predicted = predict(object = RF.model, newdata =  dplyr::select(data.FATE.PFG.habitat, all_of(RF.predictors)), type = "response", norm.votes = TRUE)
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
  
  if(predict.all.map == TRUE){
    results.habitat <- list(output.validation = output.validation, y.all.map.predicted = y.all.map.predicted)
    names(results.habitat) <- c("output.validation", "y.all.map.predicted")
  }else if(predict.all.map == FALSE){
    results.habitat <- list(output.validation = output.validation)
    names(results.habitat) <- c("output.validation")
  }
  
  return(results.habitat)
  
}

