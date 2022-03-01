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
##' create a map to visualize this comparison with all the \code{FATE} and
##' observed data.
##' 
##' @param name.simulation simulation folder name.
##' @param sim.version name of the simulation we want to validate (it works with
##' only one sim.version).
##' @param obs.path the function needs observed data, please create a folder for them in your 
##' simulation folder and then indicate in this parameter the access path to this new folder.
##' @param releves.PFG name of file which contain the observed Braund-Blanquet abundance at each site
##' and each PFG and strata (with extension).
##' @param releves.site name of the file which contain coordinates and a description of
##' the habitat associated with the dominant species of each site in the studied map (with extension).
##' @param hab.obs name of the file which contain the extended studied map in the simulation (with extension).
##' @param validation.mask name of the file which contain a raster mask that specified which pixels need validation (with extension).
##' @param studied.habitat default \code{NULL}. If \code{NULL}, the function will
##' take into account of all habitats in the hab.obs map. Otherwise, please specify 
##' in a vector the habitats that we take into account for the validation.
##' @param year year of simulation for validation.
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
##' }
##' 
##' @export
##' 
##' @importFrom raster raster projectRaster res crs
##' @importFrom sf st_read
##' @importFrom utils read.csv
##' 
### END OF HEADER ##############################################################


POST_FATE.validation_habitat = function(name.simulation
                                        , sim.version
                                        , obs.path
                                        , releves.PFG
                                        , releves.sites
                                        , hab.obs
                                        , validation.mask
                                        , studied.habitat = NULL
                                        , year)
{
  
  ## GLOBAL PARAMETERS
  
  dir.create(file.path(name.simulation, "VALIDATION", "HABITAT", sim.version), showWarnings = FALSE)
  
  # General
  output.path = paste0(name.simulation, "/VALIDATION")
  year = year
  
  # Useful elements to extract from the simulation
  name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                   flag = "MASK",
                   flag.split = "^--.*--$",
                   is.num = FALSE)
  simulation.map = raster(paste0(name))
  
  # For habitat validation
  # CBNA releves data habitat map
  releves.PFG<-read.csv(paste0(obs.path, releves.PFG),header=T,stringsAsFactors = T)
  releves.sites<-st_read(paste0(obs.path, releves.sites))
  hab.obs<-raster(paste0(obs.path, hab.obs))
  # Habitat mask at FATE simu resolution
  hab.obs.modif <- projectRaster(from = hab.obs, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
  habitat.FATE.map <- crop(hab.obs.modif, simulation.map)
  validation.mask<-raster(paste0(obs.path, validation.mask))
  
  # Other
  if(is.null(studied.habitat)){
    studied.habitat = studied.habitat
  } else if(is.character(studied.habitat)){
    studied.habitat = studied.habitat
  } else{
    stop("studied.habitat is not a vector of character")
  }
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
                                            , perStrata = F
                                            , hab.obs = hab.obs
                                            , year = year)
  
  ## AGGREGATE HABITAT PREDICTION AND PLOT PREDICTED HABITAT
  
  # Provide a color df
  col.df<-data.frame(
    habitat = RF.model$classes,
    failure = terrain.colors(length(RF.model$classes), alpha = 0.5),
    success = terrain.colors(length(RF.model$classes), alpha = 1))
  
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

