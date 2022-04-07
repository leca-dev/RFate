### HEADER #####################################################################
##'
##' @title Compare observed and simulated habitat of a \code{FATE} simulation
##' to a chosen year.
##' 
##' @name do_habitat_validation
##' 
##' @author Matthieu Combaud & Maxime Delprat
##' 
##' @description To compare observations and simulations, this function compute 
##' confusion matrix between observation and prediction and then compute the TSS 
##' for each habitats.
##' 
##' @param output.path access path to the for the folder where output files
##' will be created.
##' @param RF.model random forest model trained on observed abundance data (\code{\link{train.RF.habitat}}
##' function)
##' @param predict.all.map \code{Logical}. If TRUE, the script will predict 
##' habitat for the whole map.
##' @param sim name of the single simulation to validate.
##' @param simu_PFG a \code{data frame} with simulated abundance for each PFG and strata 
##' (if option selected) and pixel ID, extracted from a \code{FATE} simulation (see \code{\link{POST_FATE.temporalEvolution}}).
##' @param habitat.whole.area.df a \code{data frame} which contain habitat names and code for each pixel that need validation.
##' @param list.strata if abundance file is defined by strata : a character vector which contains \code{FATE} 
##' strata definition and correspondence with observed strata definition. \cr
##' If abundance file is defined for all strata : a character vector with value "all".
##' @param perStrata \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG abundance is defined by strata. 
##' If \code{FALSE}, PFG abundance defined for all strata.
##' 
##' @details
##' 
##' For a given simulation \code{sim}, this script takes the evolution abundance for each PFG
##' and all strata (or for each PFG & each strata if option selected) data frame and predicts 
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
##' @importFrom dplyr group_by %>% mutate rename select
##' @importFrom raster predict
##' @importFrom reshape2 dcast
##' @importFrom caret confusionMatrix
##' @importFrom utils write.csv
##' @importFrom tidyselect all_of
##' @importFrom stringr str_split
##' 
### END OF HEADER ##############################################################

do_habitat_validation <- function(output.path, RF.model, predict.all.map, sim, simu_PFG, habitat.whole.area.df, list.strata, perStrata)
{
  
  # check consistency for PFG & strata classes between FATE output vs the RF model
  RF.predictors <- rownames(RF.model$importance)
  PFG <- str_split(RF.predictors, "_")
  RF.PFG = NULL
  for(n in 1:length(PFG)){
    pfg = PFG[[n]][1]
    RF.PFG = c(RF.PFG,pfg)
    RF.PFG = unique(RF.PFG)
  }
  FATE.PFG <- .getGraphics_PFG(name.simulation  = str_split(output.path, "/")[[1]][1]
                               , abs.simulParam = paste0(str_split(output.path, "/")[[1]][1], "/PARAM_SIMUL/Simul_parameters_", str_split(sim, "_")[[1]][2], ".txt"))
  FATE.PFG = FATE.PFG$PFG
  if(length(setdiff(FATE.PFG,RF.PFG)) > 0 | length(setdiff(RF.PFG,FATE.PFG)) > 0) {
    stop("The PFG used to train the RF algorithm are not the same as the PFG used to run FATE.")
  }
  
  #######################
  # I. Data preparation
  #######################
  
  #transform absolute abundance into relative abundance
  simu_PFG <- simu_PFG %>% group_by(pixel,strata) %>% mutate(relative.abundance = round(prop.table(abs), digits = 2)) #those are proportions, not percentages
  simu_PFG$relative.abundance[is.na(simu_PFG$relative.abundance)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (necessary to avoid risk of confusion with NA in pixels because out of the map)
  simu_PFG <- as.data.frame(simu_PFG)
  
  #drop the absolute abundance
  simu_PFG$abs <- NULL
  
  #correct the levels (to have all PFG and all strata) to make the dcast transfo easier (all PFG*strata combination will be automatically created thanks to the factor structure, even if no line corresponds to it)
  simu_PFG$PFG <- as.factor(simu_PFG$PFG)
  simu_PFG$PFG <- factor(simu_PFG$PFG, sort(unique(c(levels(simu_PFG$PFG), RF.PFG))))
  simu_PFG$strata <- as.factor(simu_PFG$strata)
  simu_PFG$strata <- factor(simu_PFG$strata, sort(unique(c(levels(simu_PFG$strata), list.strata))))
  
  #cast
  simu_PFG <- reshape2::dcast(simu_PFG, pixel ~ PFG * strata, value.var = c("relative.abundance"), fill = 0, drop = FALSE)
  #merge PFG info and habitat + transform habitat into factor
  
  #here it is crucial to have exactly the same raster structure for "simulation.map" and "habitat.FATE.map", so as to be able to do the merge on the "pixel" variable
  data.FATE.PFG.habitat <- merge(simu_PFG, habitat.whole.area.df, by = "pixel") #at this stage we have all the pixels in the simulation area
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
  
  results.habitat <- list(output.validation = output.validation, y.all.map.predicted = y.all.map.predicted)
  names(results.habitat) <- c("output.validation", "y.all.map.predicted")
  
  return(results.habitat)
  
}

