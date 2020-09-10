### HEADER #####################################################################
##' @title Create tables of pixel temporal evolution of PFG abundances (and 
##' light and soil resources if activated) for a \code{FATE} simulation
##' 
##' @name POST_FATE.temporalEvolution
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce from 1 to 3 tables 
##' containing pixel temporal evolution of PFG abundances, as well as light and 
##' soil resources if those modules were activated, in a \code{FATE} 
##' simulation.
##' 
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param no_years an \code{integer} corresponding to the number of simulation 
##' years that will be used to extract PFG abundance / light / soil maps
##' @param opt.ras_habitat (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster mask, with an 
##' \code{integer} value within each pixel, corresponding to a specific habitat
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files, as well as the extraction of values from raster files
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{one to three 
##' preanalytical tables that can then be used to create graphics}. \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_perPFG_allStrata} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{abundance for each Plant Functional Group} 
##'   for each selected simulation year(s) in every pixel in which the PFG is 
##'   present for at least one of the selected simulation year(s) \cr \cr
##'   }
##' }
##' 
##' \strong{If the light module was activated} (see 
##' \code{\link{PRE_FATE.params_globalParameters}}), for each height stratum 
##' and each selected simulation year, raster maps are retrieved from the 
##' results folder \code{LIGHT} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{light resources for each height stratum} for 
##'   each selected simulation year(s) in every pixel \cr \cr
##'   }
##' }
##' 
##' \strong{If the soil module was activated} (see 
##' \code{\link{PRE_FATE.params_globalParameters}}), for each selected 
##' simulation year, raster maps are retrieved from the results folder 
##' \code{SOIL} and unzipped.
##' Informations extracted lead to the production of one table before the maps 
##' are compressed again :
##' 
##' \itemize{
##'   \item{the value of \strong{soil resources} for each selected simulation 
##'   year(s) in every pixel \cr \cr
##'   }
##' }
##' 
##' \strong{If a raster mask for habitat has been provided}, the tables will 
##' also contain information about the pixel habitat. \cr \cr
##' 
##' 
##' \strong{These \code{.csv} files can then be used by other functions} :
##' 
##' \itemize{
##'   \item to produce graphics of temporal evolution of modelled abundances 
##'   and space occupancy at the \emph{whole area level} \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionCoverage}})
##'   \item to produce graphics of temporal evolution of modelled abundances 
##'   and / or resources at the \emph{pixel level} \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionPixels}})
##'   \item to produce graphics of temporal evolution of community composition 
##'   at the \emph{habitat level} \cr (see 
##'   \code{\link{POST_FATE.graphic_evolutionStability}})
##' }
##' 
##' 
##' 
##' @return A \code{list} containing three \code{data.frame} objects with the 
##' following columns :
##' \describe{
##'   \item{\code{PFG}}{concerned plant functional group (for abundance)}
##'   \item{\code{STRATUM}}{concerned height stratum (for LIGHT)}
##'   \item{\code{ID.pixel}}{number of the concerned pixel}
##'   \item{\code{X, Y}}{coordinates of the concerned pixel}
##'   \item{\code{HAB}}{habitat of the concerned pixel}
##'   \item{\emph{years}}{values of the corresponding object (abundance / LIGHT 
##'   / SOIL) for each selected simulation year(s) \cr \cr}
##' }
##' 
##' One to three \file{POST_FATE_TABLE_PIXEL_evolution_[...].csv} files are created : 
##' \describe{
##'   \item{\file{abundance}}{always}
##'   \item{\file{light}}{\emph{if light module was activated}}
##'   \item{\file{soil}}{\emph{if soil module was activated}}
##' }
##' 
##' 
##' @keywords FATE, outputs, temporal evolution
##' 
##' @seealso \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{POST_FATE.graphic_evolutionCoverage}},
##' \code{\link{POST_FATE.graphic_evolutionPixels}},
##' \code{\link{POST_FATE.graphic_evolutionStability}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.temporalEvolution(name.simulation = "FATE_simulation"
##'                             , file.simulParam = "Simul_parameters_V1.txt"
##'                             , no_years = 50
##'                             , opt.no_CPU = 1)
##' }
##'                                                         
##'                                                         
##'                                                         
##' ## ----------------------------------------------------------------------------------------- ##
##' ## Load example data
##'                                                                          
##' 
##' @export
##' 
##' @importFrom foreach foreach %do% %dopar%
##' @importFrom data.table rbindlist fwrite
##' @importFrom raster raster stack 
##' rasterToPoints as.data.frame extract cellFromXY
##' @importFrom doParallel registerDoParallel
##'
##'
## END OF HEADER ###############################################################


POST_FATE.temporalEvolution = function(
  name.simulation
  , file.simulParam = NULL
  , no_years
  , opt.ras_habitat = NULL
  , opt.no_CPU = 1
){
  
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  ## CHECK parameter no_years
  .testParam_notInteger.m("no_years", no_years)
  ## CHECK parameter opt.ras_habitat
  if (!.testParam_notDef(opt.ras_habitat))
  {
    .testParam_notChar.m("opt.ras_habitat", opt.ras_habitat)
    .testParam_existFile(opt.ras_habitat)
    ras.habitat = raster(opt.ras_habitat)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.temporalEvolution")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n+++++++\n")
    cat("\n  Simulation name : ", name.simulation)
    cat("\n  Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories ----------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs ---------------------------------------------------
    ## Get PFG names --------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask ------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get habitat information ----------------------------------------------
    if (exists("ras.habitat"))
    {
      ras.habitat = ras.habitat * ras.mask
    }
    
    ## Get list of arrays and extract years of simulation -------------------
    raster.perPFG.allStrata = .getRasterNames(years = NULL, "allStrata", "ABUND")
    years = sapply(sub("Abund_YEAR_", "", raster.perPFG.allStrata)
                   , function(x) strsplit(as.character(x), "_")[[1]][1])
    years = sort(unique(as.numeric(years)))
    years = years[round(seq(1, length(years)
                            , length.out = min(no_years, length(years))))]
    no_years = length(years)
    
    cat("\n  Selected years : ", years)
    cat("\n  Number of years : ", no_years)
    cat("\n")
    
    ## UNZIP the raster saved -----------------------------------------------
    .unzip_ALL(folder_name = dir.output.perPFG.allStrata, no_cores = opt.no_CPU)
    if (doLight) .unzip_ALL(folder_name = dir.output.light, no_cores = opt.no_CPU)
    if (doSoil) .unzip_ALL(folder_name = dir.output.soil, no_cores = opt.no_CPU)
    
    doWriting.abund = TRUE
    doWriting.light = ifelse(doLight, TRUE, FALSE)
    doWriting.soil = ifelse(doSoil, TRUE, FALSE)
    
    
    ## get the data inside the rasters --------------------------------------
    cat("\n ---------- GETTING ABUNDANCE for pfg")
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
    tabAbund.list = foreach (pfg = PFG) %dopar%
    {
      cat(" ", pfg)
      file_name = paste0(dir.output.perPFG.allStrata,
                         "Abund_YEAR_",
                         years,
                         "_",
                         pfg,
                         "_STRATA_all")
      if (length(which(file.exists(paste0(file_name, ".tif")))) > 0)
      {
        file_name = paste0(file_name, ".tif")
      } else if (length(which(file.exists(paste0(file_name, ".img")))) > 0)
      {
        file_name = paste0(file_name, ".img")
      } else if (length(which(file.exists(paste0(file_name, ".asc")))) > 0)
      {
        file_name = paste0(file_name, ".asc")
      }
      
      ye = years[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        ras.df = rasterToPoints(ras)
        ras.df = as.data.frame(ras.df)
        colnames(ras.df) = c("X", "Y", ye)
        ID.abund = rowSums(ras.df[, 3:ncol(ras.df), drop = FALSE])
        ras.df = ras.df[which(ID.abund > 0), , drop = FALSE]
        
        if (nrow(ras.df) > 0)
        {
          ras.df$ID.pixel = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
          ras.df$PFG = pfg
          
          if (exists("ras.habitat"))
          {
            ras.df$HAB = extract(ras.habitat, ras.df[, c("X", "Y")])
          } else
          {
            ras.df$HAB = "ALL"
          }
          ras.df = ras.df[, c("PFG", "ID.pixel", "X", "Y", "HAB", ye)]
          
          return(ras.df)
        }
      }
    } ## END loop on PFG
    cat("\n")
    
    tabAbund = rbindlist(tabAbund.list, fill = TRUE)
    tabAbund = as.data.frame(tabAbund, stringsAsFactors = FALSE)
    
    if (nrow(tabAbund) > 0 && ncol(tabAbund) > 0)
    {
      fwrite(tabAbund
             , file = paste0(name.simulation
                             , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                             , basename(dir.save)
                             , ".csv")
             , row.names = FALSE)
    } else
    {
      tabAbund = NA
      doWriting.abund = FALSE
      warning("No abundance values were found! Please check.")
    }
    
    
    ## get the data inside the rasters --------------------------------------
    if (doLight)
    {
      cat("\n ---------- GETTING LIGHT for stratum")
      tabLight.list = foreach (stra = c(1:no_STRATA)-1) %dopar%
      {
        cat(" ", stra)
        file_name = paste0(dir.output.light
                           , "Light_Resources_YEAR_"
                           , years
                           , "_STRATA_"
                           , stra)
        if (length(which(file.exists(paste0(file_name, ".tif")))) > 0)
        {
          file_name = paste0(file_name, ".tif")
        } else if (length(which(file.exists(paste0(file_name, ".img")))) > 0)
        {
          file_name = paste0(file_name, ".img")
        } else if (length(which(file.exists(paste0(file_name, ".asc")))) > 0)
        {
          file_name = paste0(file_name, ".asc")
        }
        ye = years[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        
        if (length(file_name) > 0)
        {
          ras = stack(file_name) * ras.mask
          ras.df = rasterToPoints(ras)
          ras.df = as.data.frame(ras.df)
          colnames(ras.df) = c("X", "Y", ye)
          ras.df$ID.pixel = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
          ras.df$STRATUM = stra
          
          if (exists("ras.habitat"))
          {
            ras.df$HAB = extract(ras.habitat, ras.df[, c("X", "Y")])
          } else
          {
            ras.df$HAB = "ALL"
          }
          ras.df = ras.df[, c("STRATUM", "ID.pixel", "X", "Y", "HAB", ye)]
          
          return(ras.df)
        }
      } ## END loop on STRATUM
      cat("\n")
      
      tabLight = rbindlist(tabLight.list, fill = TRUE)
      tabLight = as.data.frame(tabLight, stringsAsFactors = FALSE)
      
      if (nrow(tabLight) > 0 && ncol(tabLight) > 0)
      {
        fwrite(tabLight
               , file = paste0(name.simulation
                               , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_light_"
                               , basename(dir.save)
                               , ".csv")
               , row.names = FALSE)
      } else
      {
        tabLight = NA
        doWriting.light = FALSE
        warning("No light values were found! Please check.")
      }
    } else
    {
      tabLight = NA
    } ## END loop for light
    
    
    ## get the data inside the rasters --------------------------------------
    if (doSoil)
    {
      cat("\n ---------- GETTING SOIL")
      file_name = paste0(dir.output.soil,
                         "Soil_Resources_YEAR_",
                         years)
      if (length(which(file.exists(paste0(file_name, ".tif")))) > 0)
      {
        file_name = paste0(file_name, ".tif")
      } else if (length(which(file.exists(paste0(file_name, ".img")))) > 0)
      {
        file_name = paste0(file_name, ".img")
      } else if (length(which(file.exists(paste0(file_name, ".asc")))) > 0)
      {
        file_name = paste0(file_name, ".asc")
      }
      ye = years[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        ras.df = rasterToPoints(ras)
        ras.df = as.data.frame(ras.df)
        colnames(ras.df) = c("X", "Y", ye)
        ras.df$ID.pixel = cellFromXY(ras.mask, ras.df[, c("X", "Y")])
        
        if (exists("ras.habitat"))
        {
          ras.df$HAB = extract(ras.habitat, ras.df[, c("X", "Y")])
        } else
        {
          ras.df$HAB = "ALL"
        }
        tabSoil = ras.df[, c("ID.pixel", "X", "Y", "HAB", ye)]
      } else
      {
        tabSoil = data.frame()
      }
      
      if (nrow(tabSoil) > 0 && ncol(tabSoil) > 0)
      {
        fwrite(tabSoil
               , file = paste0(name.simulation
                               , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_soil_"
                               , basename(dir.save)
                               , ".csv")
               , row.names = FALSE)
      } else
      {
        tabSoil = NA
        doWriting.soil = FALSE
        warning("No soil values were found! Please check.")
      }
    } else
    {
      tabSoil = NA
    } ## END loop for soil
    cat("\n")
    
    
    ## ZIP the raster saved -------------------------------------------------
    .zip_ALL(folder_name = dir.output.perPFG.allStrata, no_cores= opt.no_CPU)
    if (doLight) .zip_ALL(folder_name = dir.output.light, no_cores = opt.no_CPU)
    if (doSoil) .zip_ALL(folder_name = dir.output.soil, no_cores = opt.no_CPU)
    
    cat("\n> Done!\n")
    
    if(doWriting.abund || doWriting.light || doWriting.soil)
    {
      message(paste0("\n The output files \n"
                     , ifelse(doWriting.abund
                              , paste0(" > POST_FATE_TABLE_PIXEL_evolution_abundance_"
                                       , basename(dir.save)
                                       , ".csv \n")
                              , "")
                     , ifelse(doWriting.light
                              , paste0(" > POST_FATE_TABLE_PIXEL_evolution_light_"
                                       , basename(dir.save)
                                       , ".csv \n")
                              , "")
                     , ifelse(doWriting.soil
                              , paste0(" > POST_FATE_TABLE_PIXEL_evolution_soil_"
                                       , basename(dir.save)
                                       , ".csv \n")
                              , "")
                     , "have been successfully created !\n"))
      
      return(list(tab.abundance = tabAbund
                  , tab.light = tabLight
                  , tab.soil = tabSoil))
    }
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}
