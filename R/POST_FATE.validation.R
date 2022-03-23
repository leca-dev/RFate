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
##' @param perStrata \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG abundance is defined by strata. 
##' If \code{FALSE}, PFG abundance defined for all strata (habitat & PFG composition & PFG richness validation).
##' @param opt.no_CPU default \code{1}. \cr The number of resources that can be used to 
##' parallelize the computation of prediction performance for habitat & richness validation.
##' 
##' @param doHabitat \code{Logical}. Default \code{TRUE}. If \code{TRUE}, habitat validation module is activated,
##' if \code{FALSE}, habitat validation module is disabled.
##' @param releves.PFG a data frame with abundance (column named abund) at each site
##' and for each PFG and strata (habitat & PFG composition validation).
##' @param releves.sites a data frame with coordinates and a description of the habitat associated with 
##' the dominant species of each site in the studied map (habitat & PFG composition validation).
##' @param hab.obs a raster map of the extended studied map in the simulation, with same projection 
##' & resolution than simulation mask (habitat & PFG composition validation).
##' @param validation.mask a raster mask that specified which pixels need validation, with same projection 
##' & resolution than simulation mask (habitat & PFG composition validation).
##' @param studied.habitat default \code{NULL}. If \code{NULL}, the function will
##' take into account of habitats define in the \code{hab.obs} map. Otherwise, please specify 
##' in a 2 columns data frame the habitats (2nd column) and the ID (1st column) for each of them which will be taken 
##' into account for the validation (habitat validation).
##' @param list.strata.simulations default \code{NULL}. A character vector which contain \code{FATE} 
##' strata definition and correspondence with observed strata definition.
##' 
##' @param doComposition \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG composition validation module is activated,
##' if \code{FALSE}, PFG composition validation module is disabled.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation (PFG composition validation).
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation (PFG composition validation).
##' @param strata.considered_PFG.compo If \code{perStrata} = \code{FALSE}, a character vector with value "A" 
##' (selection of one or several specific strata disabled). If \code{perStrata} = \code{TRUE}, a character 
##' vector with at least one of the observed strata (PFG composition validation).
##' 
##' @param doRichness \code{Logical}. Default \code{TRUE}. If \code{TRUE}, PFG richness validation module is activated,
##' if \code{FALSE}, PFG richness validation module is disabled.
##' @param list.PFG a character vector which contain all the PFGs taken account in
##' the simulation and observed in the simulation area (PFG richness validation).
##' @param exclude.PFG default \code{NULL}. A character vector containing the names 
##' of the PFG you want to exclude from the analysis (PFG richness validation).
##' 
##' @details 
##' 
##' \describe{
##'   \item{Habitat validation}{The observed habitat is derived from a map of the area, the simulated habitat 
##' is derived from \code{FATE} simulated relative abundance, based on a random forest 
##' algorithm trained on observed releves data (see \code{\link{train.RF.habitat}}) \cr
##' To compare observations and simulations, the function computes confusion matrix between 
##' observations and predictions and then compute the TSS for each habitat h 
##' (number of prediction of habitat h/number of observation of habitat h + number of non-prediction 
##' of habitat h/number of non-observation of habitat h). The final metrics this script use is the 
##' mean of TSS per habitat over all habitats, weighted by the share of each habitat in the observed 
##' habitat distribution. The habitat validation also provides a visual comparison of observed and 
##' simulated habitat on the whole studied area (see \code{\link{do.habitat.validation}} &
##'  \code{\link{plot.predicted.habitat}}).} \cr
##'   \item{PFG composition validation}{This code firstly run the \code{get.observed.distribution} 
##' function in order to have a \code{obs.distri} file which contain the observed distribution 
##' per PFG, strata and habitat. This file is also an argument for the \code{do.PFG.composition.validation} 
##' function run next. This second sub function provides the computation of distance between observed 
##' and simulated distribution.}
##'   \item{PFG richness validation}{Firstly, the function updates the \code{list.PFG} with \code{exclude.PFG} vector.
##' Then, the script takes the abundance per PFG (and per strata if option selected) file from the 
##' results of the \code{FATE} simulation and computes the difference between the \code{list.PFG} 
##' and all the PFG which are presents in the abundance file, in order to obtain the PFG richness 
##' for a simulation. The function also determine if an observed PFG is missing in the results of the 
##' simulation at a specific year.}
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
##'   contain the observed releves transformed into relative metrics. \cr 1 .csv file which contain 
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
##' list.strata.simulations = list(S = c(1,2,3), M = c(4), B = c(5,6,7))
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , perStrata = TRUE
##'                       , doHabitat = TRUE
##'                       , obs.path = "FATE_Champsaur/DATA_OBS/"
##'                       , releves.PFG = "releves.PFG.abundance.csv"
##'                       , releves.sites = "releves.sites.shp"
##'                       , hab.obs = "simplified.cesbio.map.grd"
##'                       , validation.mask = "certain.habitat.100m.restricted.grd"
##'                       , studied.habitat = NULL
##'                       , list.strata.simulations = list.strata.simulations
##'                       , doComposition = FALSE
##'                       , doRichness = FALSE)
##'                       
##' ## PFG composition validation --------------------------------------------------------------------------
##' list.strata.simulations = list(S = c(1,2,3), M = c(4), B = c(5,6,7))
##' list.PFG = as.factor(c("C1","C2","C3","C4","H1","H2","H3","H4","H5","H6","P1","P2","P3","P4","P5"))
##' habitat.considered = c("coniferous.forest", "deciduous.forest", "natural.grassland", "woody.heatland")
##' strata.considered_PFG.compo = c("S", "M", "B")
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , perStrata = TRUE
##'                       , doHabitat = FALSE
##'                       , obs.path = "FATE_Champsaur/DATA_OBS/"
##'                       , releves.PFG = "releves.PFG.abundance.csv"
##'                       , releves.sites = "releves.sites.shp"
##'                       , hab.obs = "simplified.cesbio.map.grd"
##'                       , validation.mask = "certain.habitat.100m.restricted.grd"
##'                       , studied.habitat = NULL
##'                       , list.strata.simulations = list.strata.simulations
##'                       , doComposition = TRUE
##'                       , PFG.considered_PFG.compo = list.PFG
##'                       , habitat.considered_PFG.compo = habitat.considered
##'                       , strata.considered_PFG.compo = strata.considered_PFG.compo
##'                       , doRichness = FALSE)
##'                       
##' ## PFG richness validation -----------------------------------------------------------------------------
##' list.PFG = as.factor(c("C1","C2","C3","C4","H1","H2","H3","H4","H5","H6","P1","P2","P3","P4","P5"))
##' POST_FATE.validation(name.simulation = "FATE_Champsaur"
##'                       , sim.version = "SIMUL_V4.1"
##'                       , year = 2000
##'                       , perStrata = TRUE
##'                       , doHabitat = FALSE
##'                       , doComposition = FALSE
##'                       , doRichness = TRUE
##'                       , list.PFG = list.PFG
##'                       , exclude.PFG = NULL)
##' 
##' @export
##' 
##' @importFrom stringr str_split
##' @importFrom raster raster projectRaster res crs crop origin
##' @importFrom utils read.csv write.csv
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
                                , perStrata = TRUE
                                , opt.no_CPU = 1
                                , doHabitat = TRUE
                                , releves.PFG
                                , releves.sites
                                , hab.obs
                                , validation.mask
                                , studied.habitat = NULL
                                , list.strata.simulations = NULL
                                , doComposition = TRUE
                                , PFG.considered_PFG.compo
                                , habitat.considered_PFG.compo
                                , strata.considered_PFG.compo
                                , doRichness = TRUE
                                , list.PFG
                                , exclude.PFG = NULL){
  
  if(doHabitat == TRUE){
    
    cat("\n\n #------------------------------------------------------------#")
    cat("\n # HABITAT VALIDATION")
    cat("\n #------------------------------------------------------------# \n")
    
    ## GLOBAL PARAMETERS
    
    dir.create(file.path(name.simulation, "VALIDATION", "HABITAT", sim.version), showWarnings = FALSE)
    
    # General
    output.path = paste0(name.simulation, "/VALIDATION")
    year = year # choice in the year for validation
    perStrata = perStrata
    opt.no_CPU = opt.no_CPU
    
    # For habitat validation
    # Observed releves data
    releves.PFG = releves.PFG
    if(perStrata==TRUE){
    list.strata.releves = as.character(unique(releves.PFG$strata))
    list.strata.simulations = list.strata.simulations
    }else {
      list.strata.releves = NULL
      list.strata.simulations = NULL
    }
    releves.sites = releves.sites
    
    # Habitat map
    hab.obs = hab.obs
    validation.mask = validation.mask
    
    # Simulation mask
    name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                     flag = "MASK",
                     flag.split = "^--.*--$",
                     is.num = FALSE) #isolate the access path to the simulation mask for any FATE simulation
    simulation.map = raster(paste0(name))
    
    # Check hab.obs map
    if(!compareCRS(simulation.map, hab.obs) | !all(res(habitat.FATE.map)==res(simulation.map))){
      stop(paste0("Projection & resolution of hab.obs map does not match with simulation mask. Please reproject hab.obs map with projection & resolution of ", names(simulation.map)))
    }else if(extent(simulation.map) != extent(hab.obs)){
      habitat.FATE.map = crop(hab.obs, simulation.map)
    }else {
      habitat.FATE.map = hab.obs
    }
    if(!all(origin(simulation.map) == origin(habitat.FATE.map))){
      print("setting origin habitat.FATE.map to match simulation.map")
      raster::origin(habitat.FATE.map) <- raster::origin(simulation.map)
    }
    
    # Check validation mask
    if(!compareCRS(simulation.map, validation.mask) | !all(res(validation.mask)==res(simulation.map))){
      stop(paste0("Projection & resolution of validation mask does not match with simulation mask. Please reproject validation mask with projection & resolution of ", names(simulation.map)))
    }else if(extent(validation.mask) != extent(simulation.map)){
      validation.mask = crop(validation.mask, simulation.map)
    }else {
      validation.mask = validation.mask
    }
    if(!all(origin(simulation.map) == origin(validation.mask))){
      print("setting origin validation mask to match simulation.map")
      raster::origin(validation.mask) <- raster::origin(simulation.map)
    }

    # Other
    if(is.null(studied.habitat)){
      studied.habitat = studied.habitat #if null, the function will study all the habitats in the map
    } else if(is.character(studied.habitat)){
      studied.habitat = studied.habitat #if a character vector with habitat names, the function will study only the habitats in the vector
    } else{
      stop("studied.habitat is not a vector of character")
    }
    RF.param = list(
      share.training = 0.7,
      ntree = 500)
    predict.all.map = T
    
    ## TRAIN A RF ON OBSERVED DATA
    
    RF.model = train.RF.habitat(releves.PFG = releves.PFG
                                 , releves.sites = releves.sites
                                 , hab.obs = hab.obs
                                 , external.training.mask = NULL
                                 , studied.habitat = studied.habitat
                                 , RF.param = RF.param
                                 , output.path = output.path
                                 , perStrata = perStrata
                                 , sim.version = sim.version)
    
    ## USE THE RF MODEL TO VALIDATE FATE OUTPUT
    
    habitats.results = do.habitat.validation(output.path = output.path
                                              , RF.model = RF.model
                                              , habitat.FATE.map = habitat.FATE.map
                                              , validation.mask = validation.mask
                                              , simulation.map = simulation.map
                                              , predict.all.map = predict.all.map
                                              , sim.version = sim.version
                                              , name.simulation = name.simulation
                                              , perStrata = perStrata
                                              , hab.obs = hab.obs
                                              , year = year
                                              , list.strata.releves = list.strata.releves
                                              , list.strata.simulations = list.strata.simulations
                                              , opt.no_CPU = opt.no_CPU
                                              , studied.habitat = studied.habitat)
    
    ## AGGREGATE HABITAT PREDICTION AND PLOT PREDICTED HABITAT
    
    # Provide a color df
    col.df = data.frame(
      habitat = RF.model$classes,
      failure = terrain.colors(length(RF.model$classes), alpha = 0.5),
      success = terrain.colors(length(RF.model$classes), alpha = 1))
    
    prediction.map = plot.predicted.habitat(predicted.habitat = habitats.results
                                             , col.df = col.df
                                             , simulation.map = simulation.map
                                             , output.path = output.path
                                             , sim.version = sim.version)
    
  }
  
  if(doComposition == TRUE){
    
    cat("\n\n #------------------------------------------------------------#")
    cat("\n # PFG COMPOSITION VALIDATION")
    cat("\n #------------------------------------------------------------# \n")
    
    ## GLOBAL PARAMETERS
    
    if(doHabitat == FALSE){
      
      perStrata = perStrata
      opt.no_CPU = opt.no_CPU
      
      # Habitat map
      hab.obs = hab.obs
      validation.mask = validation.mask
      
      # Simulation mask
      name = .getParam(params.lines = paste0(name.simulation, "/PARAM_SIMUL/Simul_parameters_", str_split(sim.version, "_")[[1]][2], ".txt"),
                       flag = "MASK",
                       flag.split = "^--.*--$",
                       is.num = FALSE) #isolate the access path to the simulation mask for any FATE simulation
      simulation.map = raster(paste0(name))
      
      # Check hab.obs map
      if(!compareCRS(simulation.map, hab.obs) | !all(res(habitat.FATE.map)==res(simulation.map))){
        stop(paste0("Projection & resolution of hab.obs map does not match with simulation mask. Please reproject hab.obs map with projection & resolution of ", names(simulation.map)))
      }else if(extent(simulation.map) != extent(hab.obs)){
        habitat.FATE.map = crop(hab.obs, simulation.map)
      }else {
        habitat.FATE.map = hab.obs
      }
      if(!all(origin(simulation.map) == origin(habitat.FATE.map))){
        print("setting origin habitat.FATE.map to match simulation.map")
        raster::origin(habitat.FATE.map) <- raster::origin(simulation.map)
      }
      
      # Check validation mask
      if(!compareCRS(simulation.map, validation.mask) | !all(res(validation.mask)==res(simulation.map))){
        stop(paste0("Projection & resolution of validation mask does not match with simulation mask. Please reproject validation mask with projection & resolution of ", names(simulation.map)))
      }else if(extent(validation.mask) != extent(simulation.map)){
        validation.mask = crop(validation.mask, simulation.map)
      }else {
        validation.mask = validation.mask
      }
      if(!all(origin(simulation.map) == origin(validation.mask))){
        print("setting origin validation mask to match simulation.map")
        raster::origin(validation.mask) <- raster::origin(simulation.map)
      }
      
      # Get observed distribution
      releves.PFG = releves.PFG
      releves.sites = releves.sites
      
      # Do PFG composition validation
      if(perStrata==TRUE){
        list.strata.releves = as.character(unique(releves.PFG$strata))
        list.strata.simulations = list.strata.simulations
      }else {
        list.strata.releves = NULL
        list.strata.simulations = NULL
      }
      
      # Studied.habitat
      if(is.null(studied.habitat)){
        studied.habitat = studied.habitat #if null, the function will study all the habitats in the map
      } else if(is.character(studied.habitat)){
        studied.habitat = studied.habitat #if a character vector with habitat names, the function will study only the habitats in the vector
      } else{
        stop("studied.habitat is not a vector of character")
      }
      
    }
    
    ## GET OBSERVED DISTRIBUTION
    
    obs.distri = get.observed.distribution(name.simulation = name.simulation
                                           , releves.PFG = releves.PFG
                                           , releves.sites = releves.sites
                                           , hab.obs = hab.obs
                                           , studied.habitat = studied.habitat
                                           , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                           , strata.considered_PFG.compo = strata.considered_PFG.compo
                                           , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                           , perStrata = perStrata
                                           , sim.version = sim.version)
    
    ## DO PFG COMPOSITION VALIDATION
    
    performance.composition = do.PFG.composition.validation(name.simulation = name.simulation
                                                            , sim.version = sim.version
                                                            , hab.obs = hab.obs
                                                            , PFG.considered_PFG.compo = PFG.considered_PFG.compo
                                                            , strata.considered_PFG.compo = strata.considered_PFG.compo
                                                            , habitat.considered_PFG.compo = habitat.considered_PFG.compo
                                                            , observed.distribution = obs.distri
                                                            , perStrata = perStrata
                                                            , validation.mask = validation.mask
                                                            , year = year
                                                            , list.strata.simulations = list.strata.simulations
                                                            , list.strata.releves = list.strata.releves
                                                            , habitat.FATE.map = habitat.FATE.map)
    
  }
  
  if(doRichness == TRUE){
    
    cat("\n\n #------------------------------------------------------------#")
    cat("\n # PFG RICHNESS VALIDATION")
    cat("\n #------------------------------------------------------------# \n")
    
    output.path = paste0(name.simulation, "/VALIDATION/PFG_RICHNESS/", sim.version)
    perStrata = perStrata
    
    #list of PFG of interest
    list.PFG = setdiff(list.PFG,exclude.PFG)
    
    print("processing simulations")
    
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
    dying.PFG.list = foreach(i=1:length(sim.version)) %dopar% {
      
      if(perStrata == FALSE){
        
        if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))){
          simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_", sim.version, ".csv"))
          simu_PFG = simu_PFG[,c("PFG","ID.pixel", paste0("X",year))]
          colnames(simu_PFG) = c("PFG", "pixel", "abs")
        }
        
      } else if(perStrata == T){
        
          if(file.exists(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv"))){
            simu_PFG = read.csv(paste0(name.simulation, "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_perStrata_", sim.version, ".csv"))
            simu_PFG = simu_PFG[,c("PFG","ID.pixel", "strata", paste0("X", year))]
            colnames(simu_PFG) = c("PFG", "pixel", "strata", "abs")
          }
        
      }
      
      return(setdiff(list.PFG,unique(simu_PFG$PFG)))
    }
    
    #names the results
    names(dying.PFG.list) = sim.version
    
    #get table with PFG richness
    PFG.richness.df = data.frame(simulation = names(dying.PFG.list), richness = length(list.PFG) - unlist(lapply(dying.PFG.list, FUN = "length")))
    
    #get vector with one occurence per PFG*simulation with dying of the PFG, as factor with completed levels in order to have table with all PFG, including those which never die
    dyingPFG.vector = as.factor(unlist(dying.PFG.list))
    dyingPFG.vector = fct_expand(dyingPFG.vector, list.PFG)
    dying.distribution = round(table(dyingPFG.vector)/length(sim.version), digits = 2)
    
    #output
    output = list(PFG.richness.df, dying.distribution , dying.PFG.list)
    names(output) = c("PFG.richness.df", "dying.distribution", "dying.PFG.list")
    
    dir.create(output.path,recursive = TRUE, showWarnings = FALSE)
    
    write.csv(PFG.richness.df, paste0(output.path, "/performance.richness.csv"), row.names = F)
    write.csv(dying.distribution, paste0(output.path, "/PFG.extinction.frequency.csv"), row.names = F)
    write_rds(dying.PFG.list, file = paste0(output.path, "/dying.PFG.list.rds"), compress = "none")
    
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # RESULTS : ")
  cat("\n #------------------------------------------------------------# \n")
  
  if(doRichness == TRUE){
    
    cat("\n ---------- PFG RICHNESS : \n")
    cat(paste0("\n Richness at year ", year, " : ", output[[1]][2], "\n"))
    cat(paste0("\n Number of PFG extinction at year ", year, " : ", sum(output[[2]]), "\n"))
    
  } else{ 
    
    cat("\n ---------- PFG RICHNESS VALIDATION DISABLED \n")
    
  }
  
  if(doHabitat == TRUE){
    
    hab.pred = read.csv(paste0(name.simulation, "/VALIDATION/HABITAT/", sim.version, "/hab.pred.csv"))
    failure = as.numeric((table(hab.pred$prediction.code)[1]/sum(table(hab.pred$prediction.code)))*100)
    success = as.numeric((table(hab.pred$prediction.code)[2]/sum(table(hab.pred$prediction.code)))*100)
    
    cat("\n ---------- HABITAT : \n")
    cat(paste0("\n", round(failure, digits = 2), "% of habitats are not correctly predicted by ", sim.version, " \n"))
    cat(paste0("\n", round(success, digits = 2), "% of habitats are correctly predicted by ", sim.version, " \n"))
    plot(prediction.map)
    
  } else{
    
    cat("\n ---------- HABITAT VALIDATION DISABLED \n")
    
  }
  
  if(doComposition == TRUE){
    
    cat("\n ---------- PFG COMPOSITION : \n")
    return(performance.composition)
    
  } else{
    
    cat("\n ---------- PFG COMPOSITION VALIDATION DISABLED \n")
    
  }
  
}
