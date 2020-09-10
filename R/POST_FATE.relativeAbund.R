### HEADER #####################################################################
##' @title Create relative abundance maps for each Plant Functional Group for 
##' one (or several) specific year of a \code{FATE} simulation
##' 
##' @name POST_FATE.relativeAbund
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG 
##' simulated relative abundances for one (or several) specific \code{FATE} 
##' simulation year.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' abundance maps
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{raster maps of PFG 
##' relative abundance}. \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_perPFG_allStrata} and unzipped. 
##' Informations extracted lead to the production of the same number of raster 
##' before the maps are compressed again :
##' 
##' \itemize{
##'   \item{for each selected simulation year(s), \strong{relative abundances} 
##'   for all strata combined are calculated :
##'   \deqn{\frac{abund_{\text{ PFG}_i\text{, }\text{Stratum}_{all}}}
##'   {abund_{\text{ PFG}_{all}\text{, }\text{Stratum}_{all}}}} \cr \cr
##'   }
##' }
##' 
##' \strong{These \code{raster} files can then be used by other functions} :
##' 
##' \itemize{
##'   \item to produce \emph{presence/absence maps} and \emph{validation 
##'   statistics}, and associated graphics \cr (see 
##'   \code{\link{POST_FATE.graphic_validationStatistics}})
##' }
##' 
##' 
##' @return One result folder is created :
##' \describe{
##'   \item{\file{ABUND_REL_perPFG \cr_allStrata}}{containing relative 
##'   abundance raster maps for each PFG across all strata}
##' }
##' 
##' 
##' @keywords FATE, outputs, relative abundance
##' 
##' @seealso \code{\link{POST_FATE.graphic_validationStatistics}}
##' 
##' 
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
##'                         , file.simulParam = "Simul_parameters_V1.txt"
##'                         , years = 850
##'                         , opt.no_CPU = 1)
##'                                     
##' POST_FATE.relativeAbund(name.simulation = "FATE_simulation"
##'                         , file.simulParam = "Simul_parameters_V1.txt"
##'                         , years = c(850, 950)
##'                         , opt.no_CPU = 1)
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
##' @importFrom raster stack writeRaster
##'
## END OF HEADER ###############################################################


POST_FATE.relativeAbund = function(
  name.simulation
  , file.simulParam = NULL
  , years
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
  ## CHECK parameter years
  .testParam_notInteger.m("years", years)
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.relativeAbund")
  cat("\n #------------------------------------------------------------# \n")

  #############################################################################
  
  for (abs.simulParam in abs.simulParams)
  {
    
    cat("\n+++++++\n")
    cat("\n  Simulation name : ", name.simulation)
    cat("\n  Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories ------------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)

    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    
    ## Get list of arrays and extract years of simulation ---------------------
    years = sort(unique(as.numeric(years)))
    
    ## UNZIP the raster saved -------------------------------------------------
    raster.perPFG.allStrata = .getRasterNames(years, "allStrata", "ABUND")
    .unzip(folder_name = dir.output.perPFG.allStrata
           , list_files = raster.perPFG.allStrata
           , no_cores = opt.no_CPU)
    
    
    ## get the data inside the rasters ----------------------------------------
    cat("\n ---------- GETTING RELATIVE ABUNDANCES for year")
    for (y in years)
    {
      cat(" ", y)
      
      file_name = paste0(dir.output.perPFG.allStrata
                         , "Abund_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_all")
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
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        ras_REL = ras / sum(ras)
        names(ras_REL) = gp
        
        new_name = paste0(dir.output.perPFG.allStrata.REL
                          , "Abund_relative_YEAR_"
                          , y
                          , "_"
                          , names(ras_REL)
                          , "_STRATA_all.tif")
        
        writeRaster(x = ras_REL
                    , filename = new_name
                    , overwrite = TRUE
                    , bylayer = TRUE)
        
        message(paste0("\n The output files \n"
                       , paste0(" > ", basename(new_name), " \n"
                                , collapse = "")
                       , "have been successfully created !\n"))
      }
    } ## end loop on years
    
    ## ZIP the raster saved ---------------------------------------------------
    .zip(folder_name = dir.output.perPFG.allStrata
         , list_files = raster.perPFG.allStrata
         , no_cores = opt.no_CPU)
    
    cat("\n> Done!\n")
    
  }
}

