### HEADER ##########################################################################
##'
##' @title Computes validation data for habitat, PFG richness and composition for a \code{FATE} simulation.
##' 
##' @name POST_FATE.validation
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to compute validation data for : 
##' \code{Habitat} : compares habitat simulations and observations and
##' create a map to visualize this comparison with all the \code{FATE} and
##' observed data.
##' \code{PFG Composition} : produced a computation of observed distribution 
##' of relative abundance in the simulation area and a computation of distance between
##' observed and simulated distribution.
##' \code{PFG Richness} : computes the PFG richness over the whole simulation area 
##' for a \code{FATE} simulation and computes the difference between observed and simulated PFG richness.
##' 
##' @param name.simulation simulation folder name.
##' @param sim.version name of the simulation to validate (it works with only one \code{sim.version}).
##' @param year year of simulation for validation.
##' @param doHabitat logical. Default \code{TRUE}. If \code{TRUE}, habitat validation module is activated,
##' if \code{FALSE}, habitat validation module is disabled.
##' @param obs.path the function needs observed data, please create a folder for them in your 
##' simulation folder and then indicate in this parameter the access path to this new folder (habitat & PFG composition validation).
##' @param releves.PFG name of file which contain the observed Braund-Blanquet abundance at each site
##' and each PFG and strata (habitat & PFG composition validation).
##' @param releves.site name of the file which contain coordinates and a description of
##' the habitat associated with the dominant species of each site in the studied map (habitat & PFG composition validation).
##' @param hab.obs name of the file which contain the extended studied map in the simulation (habitat & PFG composition validation).
##' @param validation.mask name of the file which contain a raster mask that specified which pixels need validation 
##' (habitat & PFG composition validation).
##' @param studied.habitat default \code{NULL}. If \code{NULL}, the function will
##' take into account of all habitats in the \code{hab.obs} map. Otherwise, please specify 
##' in a vector habitats that will be take into account for the validation (habitat validation).
##' @param doComposition logical. Default \code{TRUE}. If \code{TRUE}, PFG composition validation module is activated,
##' if \code{FALSE}, PFG composition validation module is disabled.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation (PFG composition validation).
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation (PFG composition validation).
##' @param doRichness logical. Default \code{TRUE}. If \code{TRUE}, PFG richness validation module is activated,
##' if \code{FALSE}, PFG richness validation module is disabled.
##' @param list.PFG a character vector which contain all the PFGs taken account in
##' the simulation and observed in the simulation area (PFG richness validation).
##' @param exclude.PFG default \code{NULL}. A character vector containing the names 
##' of the PFG you want to exclude from the analysis (PFG richness validation).
##' 
##' @details 
##' 
##' \describe{
##'   \item{Habitat validation}{The observed habitat is derived from the cesbio map, the simulated habitat 
##' is derived from FATE simulated relative abundance, based on a random forest 
##' algorithm trained on CBNA data. To compare observations and simulations, the function
##' compute confusion matrix between observation and prediction and then compute the TSS 
##' for each habitat h (number of prediction of habitat h/number of observation 
##' of habitat h + number of non-prediction of habitat h/number of non-observation 
##' of habitat h). The final metrics this script use is the mean of TSS per habitat over all 
##' habitats, weighted by the share of each habitat in the observed habitat distribution.}
##'   \item{PFG composition validation}{This code firstly run the \code{get.observed.distribution} function in order to have
##' a \code{obs.distri} file which contain the observed distribution per PFG, strata and habitat.
##' This file is also an argument for the \code{do.PFG.composition.validation} function run next.
##' This second sub function provide the computation of distance between observed and simulated distribution. \cr 
##' NB : The argument \code{strata.considered_PFG.compo} is by default "A" in the 2 sub functions because 
##' it's easier for a \code{FATE} simulation to provide PFG abundances for all strata. \cr The argument 
##' \code{perStrata.compo} is by default \code{NULL} for the same reasons.}
##'   \item{PFG richness validation}{Firstly, the function updates the \code{list.PFG} with \code{exclude.PFG} vector.
##' Then, the script takes the abundance per PFG file from the results of the \code{FATE} 
##' simulation and computes the difference between the \code{list.PFG} and all the PFG 
##' which are presents in the abundance file, in order to obtain the PFG richness for a simulation.
##' The function also determine if an observed PFG is missing in the results of the simulation at 
##' a specific year.}
##' }
##' 
##' @return 
##' 
##' Output files : 
##' \describe{
##'   \item{\file{VALIDATION/HABITAT/sim.version}}{containing the prepared CBNA data,
##'   RF model, the performance analyzes (confusion matrix and TSS) for the training and 
##' testing parts of the RF model, the habitat performance file, the habitat prediction file with 
##' observed and simulated habitat for each pixel of the whole map and the final prediction plot.}
##' }
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION/sim.version}}{1 .csv file which contain the proximity 
##'   between observed and simulated data computed for each PFG/strata/habitat. \cr 1 .csv file which 
##'   contain the observed relevés transformed into relative metrics. \cr 1 .csv file which contain 
##'   the final output with the distribution per PFG, strata and habitat.}
##' }
##' \describe{
##'   \item{\file{VALIDATION/PFG_RICHNESS/sim.version}}{1 .csv file of PFG richness in a \code{FATE} simulation.
##'   \cr 1 .csv fie of the PFG extinction frequency in a \code{FATE} simulation. \cr 1 .rds file which is 
##'   the abundance per PFG file.
##' }
##' 
##' @examples 
##' 
##' ## Habitat validation ---------------------------------------------------------------------------------
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , doHabitat = TRUE
##'                       , obs.path = "FATE_Champsaur/DATA_OBS/"
##'                       , releves.PFG = "releves.PFG.abundance.csv"
##'                       , releves.sites = "releves.sites.shp"
##'                       , hab.obs = "simplified.cesbio.map.grd"
##'                       , validation.mask = "certain.habitat.100m.restricted.grd"
##'                       , studied.habitat = NULL
##'                       , doComposition = FALSE
##'                       , doRichness = FALSE)
##'                       
##' ## PFG composition validation --------------------------------------------------------------------------
##' list.PFG<-as.factor(c("C1","C2","C3","C4","H1","H2","H3","H4","H5","H6","P1","P2","P3","P4","P5"))
##' habitat.considered = c("coniferous.forest", "deciduous.forest", "natural.grassland", "woody.heatland")
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , doHabitat = FALSE
##'                       , obs.path = "FATE_Champsaur/DATA_OBS/"
##'                       , releves.PFG = "releves.PFG.abundance.csv"
##'                       , releves.sites = "releves.sites.shp"
##'                       , hab.obs = "simplified.cesbio.map.grd"
##'                       , validation.mask = "certain.habitat.100m.restricted.grd"
##'                       , studied.habitat = NULL
##'                       , doComposition = TRUE
##'                       , PFG.considered_PFG.compo = list.PFG
##'                       , habitat.considered_PFG.compo = habitat.considered
##'                       , doRichness = FALSE)
##'                       
##' ## PFG richness validation -----------------------------------------------------------------------------
##' list.PFG<-as.factor(c("C1","C2","C3","C4","H1","H2","H3","H4","H5","H6","P1","P2","P3","P4","P5"))
##' POST_FATE.validation(name.simulation = "FATE_CHampsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , doHabitat = FALSE
##'                       , doComposition = FALSE
##'                       , doRichness = TRUE
##'                       , list.PFG = list.PFG
##'                       , exclude.PFG = NULL)
##' 
##' @export
##' 
##' @importFrom stringr str_split
##' @importFrom raster raster projectRaster res crs crop
##' @importFrom utils read.csv write.csv
##' @importFrom sf st_read
##' @importFrom foreach foreach %dopar%
##' @importFrom forcats fct_expand
##' @importFrom readr write_rds
##' @importFrom doParallel registerDoParallel
##' @importFrom parallel detectCores
##' 
### END OF HEADER ###################################################################


POST_FATE.validation = function(name.simulation
                                , sim.version
                                , year
                                , doHabitat = TRUE
                                , obs.path
                                , releves.PFG
                                , releves.sites
                                , hab.obs
                                , validation.mask
                                , studied.habitat = NULL
                                , doComposition = TRUE
                                , PFG.considered_PFG.compo
                                , habitat.considered_PFG.compo
                                , doRichness = TRUE
                                , list.PFG
                                , exclude.PFG = NULL){
  
  if(doHabitat == TRUE){
    
    ## GLOBAL PARAMETERS
    
    dir.create(file.path(name.simulation, "VALIDATION", "HABITAT", sim.version), showWarnings = FALSE)
    
    # General
    output.path = paste0(name.simulation, "/VALIDATION")
    year = year # choice in the year for validation
    
    # Useful elements to extract from the simulation
    name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                     flag = "MASK",
                     flag.split = "^--.*--$",
                     is.num = FALSE) #isolate the access path to the simulation mask for any FATE simulation
    simulation.map = raster(paste0(name))
    
    # For habitat validation
    # CBNA releves data habitat map
    releves.PFG<-read.csv(paste0(obs.path, releves.PFG),header=T,stringsAsFactors = T)
    releves.sites<-st_read(paste0(obs.path, releves.sites))
    hab.obs = raster(paste0(obs.path, hab.obs))
    # Habitat mask at FATE simu resolution
    hab.obs.modif <- projectRaster(from = hab.obs, res = res(simulation.map)[1], crs = crs(projection(simulation.map)), method = "ngb")
    habitat.FATE.map <- crop(hab.obs.modif, simulation.map) #reprojection and croping of the extended habitat map in order to have a reduced observed habitat map
    validation.mask<-raster(paste0(obs.path, validation.mask))
    
    # Other
    if(is.null(studied.habitat)){
      studied.habitat = studied.habitat #if null, the function will study all the habitats in the map
    } else if(is.character(studied.habitat)){
      studied.habitat = studied.habitat #if a character vector with habitat names, the functuon will study only the habitats in the vector
    } else{
      stop("studied.habitat is not a vector of character")
    }
    RF.param = list(
      share.training=0.7,
      ntree=500)
    predict.all.map<-T
    
    ## TRAIN A RF ON OBSERVED DATA
    
    RF.model <- train.RF.habitat(releves.PFG = releves.PFG
                                 , releves.sites = releves.sites
                                 , hab.obs = hab.obs
                                 , external.training.mask = NULL
                                 , studied.habitat = studied.habitat
                                 , RF.param = RF.param
                                 , output.path = output.path
                                 , perStrata = F
                                 , sim.version = sim.version)
    
    ## USE THE RF MODEL TO VALIDATE FATE OUTPUT
    
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
    
  }
  
  if(doComposition == TRUE){
    
    ## GLOBAL PARAMETERS
    
    if(doHabitat == FALSE){

    # Get observed distribution
    releves.PFG = read.csv(paste0(obs.path, releves.PFG),header=T,stringsAsFactors = T)
    releves.sites = st_read(paste0(obs.path, releves.sites))
    hab.obs = raster(paste0(obs.path, hab.obs))
    # Do PFG composition validation
    validation.mask = raster(paste0(obs.path, validation.mask))
    }
    
    ## GET OBSERVED DISTRIBUTION
    
    obs.distri = get.observed.distribution(name.simulation = name.simulation
                                           , obs.path = obs.path
                                           , releves.PFG = releves.PFG
                                           , releves.sites = releves.sites
                                           , hab.obs = hab.obs
                                           , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                           , strata.considered_PFG.compo = "A"
                                           , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                           , perStrata.compo = FALSE
                                           , sim.version = sim.version)
    
    ## DO PFG COMPOSITION VALIDATION
    
    performance.composition = do.PFG.composition.validation(name.simulation = name.simulation
                                                            , obs.path = obs.path
                                                            , sim.version = sim.version
                                                            , hab.obs = hab.obs
                                                            , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                                            , strata.considered_PFG.compo = "A"
                                                            , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                                            , observed.distribution = obs.distri
                                                            , perStrata.compo = FALSE
                                                            , validation.mask = validation.mask
                                                            , year = year)
    
  }
  
  if(doRichness == TRUE){
    
    output.path = paste0(name.simulation, "/VALIDATION/PFG_RICHNESS/", sim.version)
    
    #exclude PFG : character vector containing the names of the PFG you want to exclude from the analysis #optional
    
    #list of PFG of interest
    list.PFG<-setdiff(list.PFG,exclude.PFG)
    
    registerDoParallel(detectCores()-2)
    dying.PFG.list<-foreach(i=1:length(sim.version)) %dopar% {
      
      simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
      simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))]
      colnames(simu_PFG) = c("PFG", "pixel", "abs")
      
      return(setdiff(list.PFG,unique(simu_PFG$PFG)))
    }
    
    #names the results
    names(dying.PFG.list) = sim.version
    
    #get table with PFG richness
    PFG.richness.df<-data.frame(simulation=names(dying.PFG.list),richness=length(list.PFG)-unlist(lapply(dying.PFG.list,FUN="length")))
    
    #get vector with one occurence per PFG*simulation with dying of the PFG, as factor with completed levels in order to have table with all PFG, including those which never die
    dyingPFG.vector<-as.factor(unlist(dying.PFG.list))
    dyingPFG.vector<-fct_expand(dyingPFG.vector,list.PFG)
    dying.distribution<-round(table(dyingPFG.vector)/length(sim.version),digits=2)
    
    #output
    output = list(PFG.richness.df, dying.distribution ,dying.PFG.list)
    names(output)<-c("PFG.richness.df","dying.distribution","dying.PFG.list")
    
    dir.create(output.path,recursive = TRUE, showWarnings = FALSE)
    
    write.csv(PFG.richness.df,paste0(output.path,"/performance.richness.csv"),row.names=F)
    write.csv(dying.distribution,paste0(output.path,"/PFG.extinction.frequency.csv"),row.names=F)
    write_rds(dying.PFG.list,file=paste0(output.path,"/dying.PFG.list.rds"),compress="none")
    
  }
  
  cat("\n ---------- END OF FUNCTION \n")
  
  if(doRichness == TRUE){
    cat("\n ---------- PFG RICHNESS VALIDATION RESULTS \n")
    cat(paste0("\n Richness at year ", year, " : ", output[[1]][2], "\n"))
    cat(paste0("\n Number of PFG extinction at year ", year, " : ", sum(output[[2]]), "\n"))
  } else{cat("\n ---------- PFG RICHNESS VALIDATION DISABLED \n")
  }
  if(doHabitat == TRUE){
    hab.pred = read.csv(paste0(name.simulation, "/VALIDATION/HABITAT/", sim.version, "/hab.pred.csv"))
    failure = as.numeric((table(hab.pred$prediction.code)[1]/sum(table(hab.pred$prediction.code)))*100)
    success = as.numeric((table(hab.pred$prediction.code)[2]/sum(table(hab.pred$prediction.code)))*100)
    cat("\n ---------- HABITAT VALIDATION RESULTS \n")
    cat(paste0("\n", round(failure, digits = 2), "% of habitats are not correctly predicted by ", sim.version, " \n"))
    cat(paste0("\n", round(success, digits = 2), "% of habitats are correctly predicted by ", sim.version, " \n"))
    plot(prediction.map)
  } else{cat("\n ---------- HABITAT VALIDATION DISABLED \n")
  }
  if(doComposition == TRUE){
    cat("\n ---------- PFG COMPOSITION VALIDATION RESULTS \n")
    return(performance.composition)
  } else{cat("\n ---------- PFG COMPOSITION VALIDATION DISABLED \n")
  }
}
