### HEADER #####################################################################
##'
##' @title Compute habitat performance and create a prediction plot of habitat
##' for a whole map of a \code{FATE} simulation.
##' 
##' @name POST_FATE.validation.habitat
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script compare habitat simulations and observations and
##' create a map to visualize this comparison with all the the \code{FATE} and
##' observed data.
##' 
##' @param name.simulation simulation folder name.
##' @param sim.version name of the simulation we want to validate (it works with
##' only one sim.version).
##' @param obs.path the function needs observed data, please create a folder for them in your 
##' simulation folder and then indicate in this parmeter the access path to this folder.
##' @param releves.PFG name of file which contain the observed Braund-Blanquet abundance at each site
##' and each PFG and strata (with extension).
##' @param releves.site name of the file which contain coordinates and a description of
##' the habitat associated with the dominant species of each site in the studied map (with extension).
##' @param hab.obs name of the file which contain the extended studied map in the simulation (with extension).
##' @param habitat.FATE.map name of the file which contain the restricted studied map in the simulation (with extension).
##' @param validation.mask name of the file which contain a raster mask that specified which pixels need validation.
##' 
##' @details 
##' 
##' The observed habitat is derived from the cesbio map, the simulated habitat 
##' is derived from FATE simulated relative abundance, based on a random forest 
##' algorithm trained on CBNA data. To compare observations and simulations, the function
##' compute confusion matrix between observation and prediction and then compute the TSS 
##' for each habitat h (number of prediction of habitat h/number of observation 
##' of habitat h + number of non-prediction of habitat h/number of non-observation 
##' of habitat h). The final metrics this script use is the mean of TSS per habitat over all 
##' habitats, weighted by the share of each habitat in the observed habitat distribution.
##' 
##' @return 
##' 
##' Two folders are created in name.simulation folder :
##' \describe{
##'   \item{\file{VALIDATION/HABITAT/sim.version}}{containing the prepared CBNA data,
##'   RF model, the performance analyzes (confusion matrix and TSS) for the training and 
##' testing parts of the RF model, the habitat performance file, the habitat prediction file with 
##' observed and simulated habitat for each pixel of the whole map and the final prediction plot.}
##'   \item{\file{DATA_OBS}}{maps of observed habitat and csv files of PFG and sites releves.}
##' }
##' 
### END OF HEADER ##############################################################


POST_FATE.validation_habitat = function(name.simulation
                                        , sim.version
                                        , obs.path
                                        , releves.PFG
                                        , releves.sites
                                        , hab.obs
                                        , validation.mask)
{
  
  ## LIBRARIES
  require(data.table)
  require(raster)
  require(RFate)
  require(reshape2)
  require(stringr)
  require(foreign)
  require(stringr)
  require(dplyr)
  require(sp)
  options("rgdal_show_exportToProj4_warnings"="none")
  require(rgdal)
  require(randomForest)
  require(ggplot2)
  require(ggradar)
  require(tidyverse)
  require(ggpubr)
  require(gridExtra)
  require(vegan)
  require(parallel)
  require(scales)
  require(class)
  require(caret)
  require(sampling)
  require(tidyselect)
  require(grid)
  require(gtable)
  require(scales)
  require(cowplot)
  require(sf)
  require(visNetwork)
  require(foreach)
  require(doParallel)
  require(prettyR)
  require(vcd)
  
  ## GLOBAL PARAMETERS
  
  # Create directories
  dir.create(paste0(name.simulation, "/VALIDATION"), recursive = TRUE)
  dir.create(paste0(name.simulation, "/VALIDATION/HABITAT"), recursive = TRUE)
  dir.create(paste0(name.simulation, "/VALIDATION/HABITAT/", sim.version), recursive = TRUE)
  
  # General
  output.path = paste0(name.simulation, "/VALIDATION")
  
  # Useful elements to extract from the simulation
  simulation.map=raster(paste0(name.simulation,"/DATA/MASK/MASK_Champsaur.tif"))
  
  # For habitat validation
  # CBNA releves data habitat map
  releves.PFG<-read.csv(paste0(obs.path, releves.PFG),header=T,stringsAsFactors = T)
  releves.sites<-st_read(paste0(obs.path, releves.sites))
  hab.obs<-raster(paste0(obs.path, hab.obs))
  # Habitat mask at FATE simu resolution
  hab.obs.modif<-projectRaster(from = hab.obs, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
  habitat.FATE.map<-crop(hab.obs.modif, simulation.map)
  validation.mask<-raster(paste0(obs.path, validation.mask))
  
  # Provide a color df
  col.df<-data.frame(
    habitat=c("agricultural.grassland","coniferous.forest","deciduous.forest","natural.grassland","woody.heatland"),
    failure=c("yellow","blueviolet","aquamarine","chartreuse1","lightsalmon"),
    success=c("darkorange1","blue4","aquamarine3","chartreuse3","firebrick4"))
  
  # Other
  studied.habitat=c("coniferous.forest","deciduous.forest","natural.grassland","woody.heatland","agricultural.grassland")
  RF.param = list(
    share.training=0.7,
    ntree=500)
  predict.all.map<-T
  
  ## TRAIN A RF ON CBNA DATA
  
  RF.model <- train.RF.habitat(releves.PFG = releves.PFG
                               , releves.sites = releves.sites
                               , hab.obs = hab.obs
                               , external.training.mask = NULL
                               , studied.habitat = studied.habitat
                               , RF.param = RF.param
                               , output.path = output.path
                               , perStrata = F
                               , sim.version = sim.version)
  
  ## USE THE RF MODEL TO VALIDATE OUTPUT
  
  habitats.results <- do.habitat.validation(output.path = output.path
                                            , RF.model = RF.model
                                            , habitat.FATE.map = habitat.FATE.map
                                            , validation.mask = validation.mask
                                            , simulation.map = simulation.map
                                            , predict.all.map = predict.all.map
                                            , sim.version = sim.version
                                            , name.simulation = name.simulation
                                            , perStrata = F)
  
  ## AGGREGATE HABITAT PREDICTION AND PLOT PREDICTED HABITAT
  
  prediction.map <- plot.predicted.habitat(predicted.habitat = habitats.results
                                           , col.df = col.df
                                           , simulation.map = simulation.map
                                           , output.path = output.path
                                           , sim.version = sim.version)
  
  ## COMPARISON FAILURE/SUCCESS
  
  hab.pred = read.csv(paste0(output.path, "/HABITAT/", sim.version, "/hab.pred.csv"))
  failure = as.numeric((table(hab.pred$prediction.code)[1]/sum(table(hab.pred$prediction.code)))*100)
  success = as.numeric((table(hab.pred$prediction.code)[2]/sum(table(hab.pred$prediction.code)))*100)
  cat("\n ---------- END OF THE SIMULATION \n")
  cat(paste0("\n ---------- ", round(failure, digits = 2), "% of habitats are not correctly predicted by ", sim.version, " \n"))
  cat(paste0("\n ---------- ", round(success, digits = 2), "% of habitats are correctly predicted by ", sim.version, " \n"))
  return(prediction.map)
  
}

