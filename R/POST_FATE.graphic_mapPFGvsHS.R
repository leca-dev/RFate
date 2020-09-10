### HEADER #####################################################################
##' @title Create maps of both habitat suitability and simulated occurrences of 
##' each Plant Functional Group for one (or several) specific year of a 
##' \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_mapPFGvsHS
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG habitat 
##' suitability and simulated occurrences for one (or several) specific 
##' \code{FATE} simulation year.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' binary maps
##' @param opt.stratum (\emph{optional}) default \code{all}. \cr The stratum 
##' number from which to extract PFG binary maps
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, one preanalytical graphic. 
##' \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{BIN_perPFG_allStrata} (unless the 
##' \code{opt.stratum} is used, then it will be from the folder 
##' \code{BIN_perPFG_perStrata}) and unzipped. 
##' Informations extracted lead to the production of one graphic before the 
##' maps are compressed again :
##' 
##' \itemize{
##'   \item{the comparison between each \strong{PFG habitat suitability map and 
##'   its simulated map of presence} \cr \cr
##'   }
##' }
##' 
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.relativeAbund}} and the 
##' \code{\link{POST_FATE.binaryMaps}} function have been run 
##' and that the folders \code{BIN_perPFG_allStrata} and 
##' \code{BIN_perPFG_perStrata} exist.
##' 
##' 
##' 
##' @return A \code{list} containing one or several (one for each simulation 
##' year) \code{list} of \code{ggplot2} objects, representing for each plant 
##' functional group its map of modelled presence / absence \code{vs} its 
##' habitat suitability map. \cr \cr
##' 
##' 
##' One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_B \cr map_PFGvsHS}}{to visualize the PFG presence 
##'   within the studied area (probability and simulated occurrence)}
##' }
##' 
##' 
##' @keywords FATE, outputs, habitat suitability, binary
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.binaryMaps}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , years = 850)
##'                                     
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , years = c(850, 950))
##'                                     
##' POST_FATE.graphic_mapPFGvsHS(name.simulation = "FATE_simulation"
##'                              , file.simulParam = "Simul_parameters_V1.txt"
##'                              , years = 850
##'                              , opt.stratum = 2)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach %do%
##' @importFrom raster stack rasterToPoints as.data.frame
##' @importFrom grid unit
##' @importFrom colorspace heat_hcl
##' @importFrom grDevices pdf
##' @importFrom graphics plot
##' 
##' @importFrom ggplot2 ggplot aes_string 
##' geom_raster 
##' scale_fill_gradientn coord_equal
##' facet_wrap labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##' 
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFGvsHS = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , opt.stratum = "all"
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
  cat("\n # POST_FATE.graphic_mapPFGvsHS")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n+++++++\n")
    cat("\n  Simulation name : ", name.simulation)
    cat("\n  Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories -----------------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get raster HS ----------------------------------------------------------
    if (doHabsuit)
    {
      file.hs = .getParam(params.lines = abs.simulParam
                          , flag = "PFG_MASK_HABSUIT"
                          , flag.split = "^--.*--$"
                          , is.num = FALSE)
      .testParam_existFile(file.hs)
      
      ## SDM maps -------------------------------------------------------------
      ras.hs = stack(file.hs) * ras.mask
      ras.hs.pts = as.data.frame(rasterToPoints(ras.hs))
      colnames(ras.hs.pts) = c("X", "Y", PFG)
    } else
    {
      warning(paste0("Missing data!\n The habitat suitability module "
                     , "was not activated. Please check."))
      ras.hs.pts = as.data.frame(rasterToPoints(ras.mask))
      colnames(ras.hs.pts) = c("X", "Y", "layer")
      for (fg in PFG) ras.hs.pts[[fg]] = ras.hs.pts$layer
    }
    
    
    ## Get list of arrays and extract years of simulation ---------------------
    years = sort(unique(as.numeric(years)))
    
    if (opt.stratum == "all")
    {
      raster.perPFG.allStrata.BIN = .getRasterNames(years, "allStrata", "BIN")
      dir.tmp = paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_allStrata/")
    } else
    {
      raster.perPFG.perStrata.BIN = .getRasterNames(years, "perStrata", "BIN")
      dir.tmp = paste0("RESULTS/", basename(dir.save), "/BIN_perPFG_perStrata/")
    }
    .testParam_existFolder(name.simulation, dir.tmp)
    dir.tmp = paste0(name.simulation, "/", dir.tmp)
    
    
    ###########################################################################
    
    ## get the data inside the rasters ----------------------------------------
    cat("\n ---------- GETTING PFG and SDM maps for")
    plot_list = foreach (y = years) %do%
    {
      cat("\n> year", y, "\n")
      
      ## PFG maps -------------------------------------------------------------
      file_name = paste0(dir.tmp
                         , "Binary_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_"
                         , opt.stratum
                         , ".tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        ras.pts = as.data.frame(rasterToPoints(ras))
        colnames(ras.pts) = c("X", "Y", gp)
        
        
        ## produce the plot ---------------------------------------------------
        cat("\n ---------- PRODUCING PLOT(S)")
        plot_list.PFG = foreach(pfg = PFG) %do%
        {
          cat("\n> Preparing for PFG ", pfg)
          
          if (pfg %in% colnames(ras.hs.pts))
          {
            tab.hs = data.frame(ras.hs.pts[, c("X", "Y", pfg)]
                                , TYPE = "Habitat Suitability"
                                , stringsAsFactors = FALSE)
            if (pfg %in% colnames(ras.pts))
            {
              tab.pfg = data.frame(ras.pts[, c("X", "Y", pfg)]
                                   , TYPE = "FATE"
                                   , stringsAsFactors = FALSE)
              tab = rbind(tab.hs, tab.pfg)
              
              pp = ggplot(tab, aes_string(x = "X", y = "Y", fill = pfg)) +
                scale_fill_gradientn("Presence probability"
                                     , colors = rev(heat_hcl(9))
                                     , breaks = seq(0, 1, 0.1)) +
                coord_equal() +
                geom_raster() +
                facet_wrap(~ TYPE, ncol = 2) +
                labs(x = "", y = ""
                     , title = paste0("GRAPH B : Habitat suitability vs FATE \n"
                                      , "Simulation year : ", y, " - PFG : ", pfg)
                     , subtitle = paste0("For each pixel and stratum, first "
                                         , "relative abundances are calculated, "
                                         , "then transformed into binary values :\n"
                                         , "1 if the PFG abundance represents more than "
                                         , "5 % of the pixel abundance, 0 otherwise.\n"
                                         , "If the PFG is present in one stratum, then "
                                         , "it is considered present within the pixel.\n")) +
                .getGraphics_theme() +
                theme(axis.text = element_blank()
                      , legend.key.width = unit(3, "lines"))
              return(pp)
            }
          }
        } ## END loop on PFG
        names(plot_list.PFG) = PFG
        return(plot_list.PFG)
      }
      
    } ## END loop on years
    names(plot_list) = years
    
    ## SAVE plots into file ---------------------------------------------------
    if (sum(sapply(plot_list, is.null)) < length(plot_list))
    {
      pdf(file = paste0(name.simulation
                        , "/RESULTS/POST_FATE_GRAPHIC_B_map_PFGvsHS_"
                        , basename(dir.save)
                        , ".pdf")
          , width = 10, height = 10)
      for (y in years)
      {
        for (pfg in PFG)
        {
          pp = plot_list[[as.character(y)]][[pfg]]
          if (!is.null(pp)) plot(pp)
        }
      }
      dev.off()
    }
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(plot_list)
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

