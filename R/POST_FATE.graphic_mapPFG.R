### HEADER #####################################################################
##' @title Create a map related to plant functional group results (richness, 
##' relative cover, light or soil CWM) for one (or several) specific year of a 
##' \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_mapPFG
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce one (or several) raster map 
##' related to plant functional group results (richness, relative cover, light 
##' or soil CWM) for one (or several) specific \code{FATE} simulation year.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' abundance and binary maps
##' @param opt.stratum_min (\emph{optional}) default \code{1}. \cr An 
##' \code{integer} corresponding to the lowest stratum from which PFG 
##' abundances will be summed up
##' @param opt.stratum_max (\emph{optional}) default \code{10}. \cr An 
##' \code{integer} corresponding to the highest stratum from which PFG 
##' abundances will be summed up
##' @param opt.doBinary (\emph{optional}) default \code{TRUE}. \cr If 
##' \code{TRUE}, abundance maps (absolute or relative) are systematically 
##' multiplied by binary maps (see 
##' \href{POST_FATE.graphic_mapPFG#details}{\code{Details}})
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{up to six raster 
##' maps} and preanalytical graphics. \cr \cr
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folders \code{ABUND_perPFG_perStrata} and 
##' \code{BIN_perPFG_perStrata} and unzipped. 
##' Informations extracted lead to the production of up to six graphics before 
##' the maps are compressed again :
##' 
##' \itemize{
##'   \item{map of \strong{PFG richness} within each pixel, representing the sum 
##'   of binary maps \cr
##'   \strong{Richness} is calculated with the 
##'   \strong{\href{http://www.jstor.org/stable/23143936}{Leinster & Cobbold 
##'   2012 Ecology} framework} which allows to give more or less importance to 
##'   the commun species through the \code{q} parameter :
##'   \describe{
##'     \item{\code{q = 0}}{species richness}
##'     \item{\code{q = 1}}{Shannon entropy}
##'     \item{\code{q = 2}}{Simpson concentration \cr \cr}
##'   }
##'   }
##'   \item{map of \strong{PFG relative cover}, representing the sum of relative 
##'   abundance maps of all PFG \cr (potentially above a height threshold 
##'   defined by \code{opt.stratum_min})}
##'   \item{\strong{if light was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \strong{community 
##'   weighted mean of PFG light preferences} (extracted from \code{LIGHT} 
##'   parameter within \code{LIGHT} files, see 
##'   \code{\link{PRE_FATE.params_PFGlight}})}
##'   \item{\strong{if soil was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \strong{community 
##'   weighted mean of PFG soil preferences} (extracted from \code{SOIL_CONTRIB} 
##'   parameter within \code{SOIL} files, see 
##'   \code{\link{PRE_FATE.params_PFGsoil}}) \cr \cr}
##' }
##' 
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.relativeAbund}}, 
##' (\code{\link{POST_FATE.graphic_validationStatistics}}) and 
##' \code{\link{POST_FATE.binaryMaps}} functions have been run and that the 
##' folders \code{BIN_perPFG_allStrata} and \code{BIN_perPFG_perStrata} exist. 
##' \cr \cr
##' 
##' If \code{opt.doBinary = TRUE}, abundance maps (absolute or relative) are 
##' systematically multiplied by binary maps extracted from 
##' \code{BIN_perPFG_allStrata} and \code{BIN_perPFG_perStrata} folders and 
##' produced by \code{\link{POST_FATE.binaryMaps}} function. 
##' This way, produced raster maps reflect the \emph{validated/refined} predictions. 
##' \code{opt.doBinary} can be set to \code{FALSE} to reflect \emph{pure} 
##' simulation results.
##' 
##' 
##' 
##' @return A \code{list} containing one or several (one for each simulation 
##' year) \code{list} of \code{raster} and \code{ggplot2} objects :
##' 
##' \describe{
##'   \item{tab}{ 
##'     \describe{
##'       \item{\code{cover}}{\code{raster} of relative coverage}
##'       \item{\code{DIV.0}}{\code{raster} of species richness}
##'       \item{\code{DIV.1}}{\code{raster} of Shannon entropy}
##'       \item{\code{DIV.2}}{\code{raster} of Simpson concentration}
##'       \item{\code{CWM.light}}{\code{raster} of light community weighted mean}
##'       \item{\code{CWM.soil}}{\code{raster} of soil community weighted mean}
##'     }
##'   }
##'   \item{plot}{
##'     \describe{
##'       \item{\code{cover}}{\code{ggplot2} object, representing \code{cover} 
##'       raster}
##'       \item{\code{richness}}{\code{ggplot2} object, representing 
##'       \code{DIV.0} raster}
##'       \item{\code{CWM.light}}{\code{ggplot2} object, representing 
##'       \code{CWM.light} raster}
##'       \item{\code{CWM.soil}}{\code{ggplot2} object, representing 
##'       \code{CWM.soil} raster \cr \cr}
##'     }
##'   }
##' }
##' 
##' 
##' \file{POST_FATE_GRAPHIC_C_map_PFG_[...].pdf} file is created containing up 
##' to four graphics : 
##' \describe{
##'   \item{\file{map_PFGcover}}{to visualize the PFG cover within the studied 
##'   area}
##'   \item{\file{map_PFGrichness}}{to visualize the PFG richness within the 
##'   studied area}
##'   \item{\file{PFGlight}}{to visualize the light CWM within the studied area}
##'   \item{\file{PFGsoil}}{to visualize the soil CWM within the studied area}
##' }
##' 
##' Three \file{PFGrichness_YEAR_[...]_STRATA_all_q[...].tif} files are created 
##' into the simulation results folder :
##' \describe{
##'   \item{\file{q0}}{PFG richness}
##'   \item{\file{q1}}{PFG Shannon entropy}
##'   \item{\file{q2}}{PFG Simpson concentration}
##' }
##' 
##' Raster files are also created for cover, and light and soil CWM if those 
##' modules were selected (see \code{\link{PRE_FATE.params_globalParameters}}).
##' 
##' 
##' @keywords FATE, outputs, richness, relative abundance, forest cover, 
##' community weighted mean, light, soil
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}}, 
##' \code{\link{POST_FATE.graphic_validationStatistics}},
##' \code{\link{POST_FATE.binaryMaps}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
##'                          , file.simulParam = "Simul_parameters_V1.txt"
##'                          , years = 850
##'                          , opt.stratum_min = 3
##'                          , opt.no_CPU = 1)
##'                                     
##' POST_FATE.graphic_mapPFG(name.simulation = "FATE_simulation"
##'                          , file.simulParam = "Simul_parameters_V1.txt"
##'                          , year = c(850, 950)
##'                          , opt.doBinary = FALSE)
##' }
##'                                     
##'                                     
##' 
##' @export
##' 
##' @importFrom foreach foreach %do%
##' @importFrom raster raster stack as.data.frame rasterToPoints xyFromCell
##' @importFrom grid unit
##' @importFrom grDevices pdf
##' @importFrom RColorBrewer brewer.pal
##' @importFrom reshape2 melt
##' 
##' @importFrom ggplot2 ggplot aes aes_string ggsave
##' geom_raster element_blank coord_equal
##' scale_fill_gradientn labs theme element_rect
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_mapPFG = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , opt.stratum_min = 1
  , opt.stratum_max = 10
  , opt.doBinary = TRUE
  , opt.no_CPU = 1
  , opt.doPlot = TRUE
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
  ## CHECK parameter opt.stratum_...
  .testParam_notInteger.m("opt.stratum_min", opt.stratum_min)
  .testParam_notInteger.m("opt.stratum_max", opt.stratum_max)
  if (opt.stratum_min > opt.stratum_max){
    stop(paste0("Wrong type of data!\n `opt.stratum_min` must contain "
                , "value equal or inferior to `opt.stratum_max`"))
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.graphic_mapPFG")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n+++++++\n")
    cat("\n  Simulation name : ", name.simulation)
    cat("\n  Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories ------------------------------------------------
    GLOB_DIR = .getGraphics_results(name.simulation  = name.simulation
                                    , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    GLOB_SIM = .getGraphics_PFG(name.simulation  = name.simulation
                                , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                  , abs.simulParam = abs.simulParam)
    
    ## Get list of arrays and extract years of simulation -------------------
    years = sort(unique(as.numeric(years)))
    no_years = length(years)
    
    opt.doStrata = FALSE
    name_strata = "all"
    range_strata = max(1, opt.stratum_min):min(GLOB_SIM$no_STRATA, opt.stratum_max)
    if (opt.stratum_min > 1 || opt.stratum_max < GLOB_SIM$no_STRATA)
    {
      opt.doStrata = TRUE
      name_strata = paste0(range_strata[1], "_", range_strata[length(range_strata)])
      cat("\n  Number of strata : ", GLOB_SIM$no_STRATA)
      cat("\n  Selected strata : ", range_strata)
      cat("\n")
    }
    
    ## UNZIP the raster saved -------------------------------------------------
    if (opt.doBinary)
    {
      raster.perPFG.allStrata.BIN = .getRasterNames(years, "allStrata", "BIN", GLOB_DIR)
    }
    ## If opt.stratum_... used
    if (opt.doStrata)
    {
      # raster.perPFG.perStrata = .getRasterNames(years, "perStrata", "ABUND", GLOB_DIR)
      combi = expand.grid(year = years
                          , pfg = GLOB_SIM$PFG
                          , stratum = range_strata
                          , stringsAsFactors = FALSE)
      raster.perPFG.perStrata = sapply(1:nrow(combi), function(i)
        paste0("Abund_YEAR_"
               , combi$year[i]
               , "_"
               , combi$pfg[i]
               , "_STRATA_"
               , combi$stratum[i]
               , ".tif.gz"))
      .unzip(folder_name = GLOB_DIR$dir.output.perPFG.perStrata
             , list_files = raster.perPFG.perStrata
             , no_cores = opt.no_CPU)
    } else
    {
      raster.perPFG.allStrata = .getRasterNames(years, "allStrata", "ABUND", GLOB_DIR)
      .unzip(folder_name = GLOB_DIR$dir.output.perPFG.allStrata
             , list_files = raster.perPFG.allStrata
             , no_cores = opt.no_CPU)
    }
    ## If light activated
    if (GLOB_SIM$doLight)
    {
      combi = expand.grid(year = years, stratum = range_strata, stringsAsFactors = FALSE)
      raster.light.perStrata = sapply(1:nrow(combi), function(i)
        paste0("Light_Resources_YEAR_"
               , combi$year[i]
               , "_STRATA_"
               , combi$stratum[i]
               , ".tif.gz"))
      .unzip(folder_name = GLOB_DIR$dir.output.light
             , list_files = raster.light.perStrata
             , no_cores = opt.no_CPU)
    }
    ## If soil activated
    if (GLOB_SIM$doSoil)
    {
      raster.soil = paste0("Soil_Resources_YEAR_", years, ".tif.gz")
      .unzip(folder_name = GLOB_DIR$dir.output.soil
             , list_files = raster.soil
             , no_cores = opt.no_CPU)
    }
    
    ## get the data inside the rasters --------------------------------------
    cat("\n ---------- GETTING GRAPHIC for")
    year_list = foreach (y = years) %do%
    {
      cat("\n> year", y, "\n")
      
      if (!opt.doStrata)
      {
        ## GET PFG abundance maps (all strata) ------------------------------
        file_name = paste0(GLOB_DIR$dir.output.perPFG.allStrata
                           , "Abund_YEAR_"
                           , y
                           , "_"
                           , GLOB_SIM$PFG
                           , "_STRATA_all.tif")
        gp = GLOB_SIM$PFG[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        
        if (length(file_name) > 0)
        {
          ras.PFG = stack(file_name) * GLOB_MASK$ras.mask
          names(ras.PFG) = gp
        } else
        {
          stop(paste0("Missing data!\n The folder "
                      , GLOB_DIR$dir.output.perPFG.allStrata
                      , " does not contain adequate files"))
        }
      } else
      {
        ## GET PFG abundance maps (selected strata) -------------------------
        ras.PFG = foreach (fg = GLOB_SIM$PFG) %do%
        {
          file_name = paste0(GLOB_DIR$dir.output.perPFG.perStrata
                             , "Abund_YEAR_"
                             , y
                             , "_"
                             , fg
                             , "_STRATA_"
                             , range_strata
                             , ".tif")
          st = range_strata[which(file.exists(file_name))]
          file_name = file_name[which(file.exists(file_name))]
          
          if (length(file_name) > 0)
          {
            ras = stack(file_name) * GLOB_MASK$ras.mask
            ras.tot = sum(ras)
            names(ras.tot) = fg
            return(ras.tot)
          }
        }
        names(ras.PFG) = GLOB_SIM$PFG
        ras.PFG = lapply(which(sapply(ras.PFG, is.null) == FALSE), function(x) ras.PFG[[x]])
        
        if (length(ras.PFG) == 0)
        {
          stop(paste0("Missing data!\n The folder "
                      , GLOB_DIR$dir.output.perPFG.perStrata
                      , " does not contain adequate files"))
        }
        ras.PFG = stack(ras.PFG)
      }
      if (opt.doBinary)
      {
        ## Multiplied by binary maps ----------------------------------------
        file_name = paste0(GLOB_DIR$dir.output.perPFG.allStrata.BIN
                           , "Binary_YEAR_"
                           , y
                           , "_"
                           , names(ras.PFG)
                           , "_STRATA_all.tif")
        gp = names(ras.PFG)[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        
        if (length(file_name) == nlayers(ras.PFG))
        {
          ras.bin = stack(file_name) * GLOB_MASK$ras.mask
          ras.PFG = ras.PFG * ras.bin
          names(ras.PFG) = gp
        } else
        {
          warning(paste0("Missing data!\n The folder "
                         , GLOB_DIR$dir.output.perPFG.allStrata.BIN
                         , " does not contain all required files. "
                         , "`opt.doBinary` set to FALSE. Please check."))
        }
      }
      ras.TOT = sum(ras.PFG)
      output.names = vector()
      ras_list = list()
      
      ## GET abundance relative maps ------------------------------------------
      ras.REL = ras.PFG / max(ras.TOT[], na.rm = TRUE)
      ras_list$cover.PFG = ras.REL
      
      ## GET cover map --------------------------------------------------------
      ras.COVER = ras.TOT / max(ras.TOT[], na.rm = TRUE)
      ras_list$cover.all = ras.COVER
      
      output.name = paste0(GLOB_DIR$dir.save
                           , "/PFGcover_YEAR_"
                           , y
                           , "_STRATA_"
                           , name_strata
                           , ".tif")
      output.names = c(output.names, output.name)
      writeRaster(ras.COVER, filename = output.name, overwrite = TRUE)
      
      ## GET richness map -----------------------------------------------------
      ras.pts = as.data.frame(ras.REL)
      ras.pts = as.matrix(ras.pts)
      ras.DIV = foreach(qq = 0:2) %do%
      {
        div_q = divLeinster(spxp = ras.pts, q = qq)
        ras.div = GLOB_MASK$ras.mask
        ras.div[] = div_q
        ras.div = ras.div * GLOB_MASK$ras.mask
        ras_list[[paste0("DIV.", qq)]] = ras.div
        
        output.name = paste0(GLOB_DIR$dir.save
                             , "/PFGrichness_YEAR_"
                             , y
                             , "_STRATA_"
                             , name_strata
                             , "_q"
                             , qq
                             , ".tif")
        output.names = c(output.names, output.name)
        writeRaster(ras.div, filename = output.name, overwrite = TRUE)
        
        return(ras.div)
      }
      
      ## GET CWM map ----------------------------------------------------------
      if (GLOB_SIM$doLight)
      {
        light_files = .getParam(params.lines = abs.simulParam
                                , flag = "PFG_PARAMS_LIGHT"
                                , flag.split = "^--.*--$"
                                , is.num = FALSE)
        light_need = foreach(fi = light_files, .combine = "c") %do%
        {
          .getParam(params.lines = fi
                    , flag = "LIGHT"
                    , flag.split = " "
                    , is.num = TRUE)
        }
        names(light_need) = GLOB_SIM$PFG
        if (length(na.exclude(light_need)) == 0)
        {
          warning(paste0("Missing data!\n The files \n"
                         , paste0(" > ", light_files, " \n", collapse = "")
                         , " do not contain `LIGHT` flag parameter. Please check."))
        } else
        {
          ras.CWM.light = sum(ras.REL * light_need[names(ras.REL)])
          ras_list$CWM.light = ras.CWM.light
          
          output.name = paste0(GLOB_DIR$dir.save
                               , "/PFGlight_YEAR_"
                               , y
                               , "_STRATA_"
                               , name_strata
                               , ".tif")
          output.names = c(output.names, output.name)
          writeRaster(ras.CWM.light, filename = output.name, overwrite = TRUE)
        }
        
        ## GET light maps (selected strata) -----------------------------------
        file_name = paste0(GLOB_DIR$dir.output.light, sub(".gz$", "", raster.light.perStrata))
        st = range_strata[which(file.exists(file_name))]
        file_name = file_name[which(file.exists(file_name))]
        if (length(file_name) > 0)
        {
          ras.light = stack(file_name) * GLOB_MASK$ras.mask
          names(ras.light) = paste0("Stratum_", st)
          ras_list$LIGHT = ras.light
        }
      }
      if (GLOB_SIM$doSoil)
      {
        soil_files = .getParam(params.lines = abs.simulParam
                               , flag = "PFG_PARAMS_SOIL"
                               , flag.split = "^--.*--$"
                               , is.num = FALSE)
        soil_contrib = foreach(fi = soil_files, .combine = "c") %do%
        {
          .getParam(params.lines = fi
                    , flag = "SOIL_CONTRIB"
                    , flag.split = " "
                    , is.num = TRUE)
        }
        names(soil_contrib) = GLOB_SIM$PFG
        ras.CWM.soil = sum(ras.REL * soil_contrib[names(ras.REL)])
        ras_list$CWM.soil = ras.CWM.soil
        
        output.name = paste0(GLOB_DIR$dir.save
                             , "/PFGsoil_YEAR_"
                             , y
                             , "_STRATA_"
                             , name_strata
                             , ".tif")
        output.names = c(output.names, output.name)
        writeRaster(ras.CWM.soil, filename = output.name, overwrite = TRUE)
        
        ## GET soil maps ------------------------------------------------------
        file_name = paste0(GLOB_DIR$dir.output.soil, sub(".gz$", "", raster.soil))
        if (length(file_name) > 0)
        {
          ras.soil = stack(file_name) * GLOB_MASK$ras.mask
          names(ras.soil) = years
          ras_list$SOIL = ras.soil
        }
      }
      
      message(paste0("\n The output files \n"
                     , paste0(" > ", output.names, " \n"
                              , collapse = "")
                     , "have been successfully created !\n"))
      
      #########################################################################
      
      ## produce the plot -----------------------------------------------------
      if (opt.doPlot)
      {
        cat("\n ---------- PRODUCING PLOT(S)")
        
        pp.i = function(tab, i.col, i.axis, i.lim, i.bre, i.lab, i.title, i.subtitle)
        {
          pp.i = ggplot(tab, aes_string(x = "X", y = "Y", fill = "VALUE")) +
            scale_fill_gradientn(i.axis
                                 , colors = i.col
                                 , limits = i.lim
                                 , breaks = i.bre
                                 , labels = i.lab) +
            coord_equal() +
            geom_raster() +
            labs(x = "", y = ""
                 , title = i.title
                 , subtitle = i.subtitle) +
            .getGraphics_theme() +
            theme(axis.text = element_blank()
                  , legend.key.width = unit(2, "lines"))
          
          return(pp.i)
        }
        
        pp_list = list()
        
        ## PFG COVER ------------------------------------------------------------
        cat("\n> PFG cover...")
        ras.pts = as.data.frame(rasterToPoints(ras.COVER))
        colnames(ras.pts) = c("X", "Y", "VALUE")
        pp_list$cover = pp.i(tab = ras.pts
                             , i.col = brewer.pal(9, "YlGn")
                             , i.axis = "Abundance (%)"
                             , i.lim = c(0, 1)
                             , i.bre = seq(0, 1, 0.2)
                             , i.lab = seq(0, 100, 20)
                             , i.title = paste0("GRAPH C : map of PFG cover - Simulation year : ", y)
                             , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                   , opt.stratum_min, " to ", GLOB_SIM$no_STRATA, " are summed,\n"
                                                   , "then transformed into relative values by dividing "
                                                   , "by the maximum abundance obtained.\n\n"))
        
        ## PFG RICHNESS ---------------------------------------------------------
        cat("\n> PFG richness...")
        ras.pts = as.data.frame(rasterToPoints(ras.DIV[[1]]))
        colnames(ras.pts) = c("X", "Y", "VALUE")
        mini = floor(min(ras.pts$VALUE, na.rm = TRUE))
        maxi = ceiling(max(ras.pts$VALUE, na.rm = TRUE))
        pp_list$richness = pp.i(tab = ras.pts
                                , i.col = brewer.pal(9, "RdPu")
                                , i.axis = "Number of PFG"
                                , i.lim = c(mini, maxi)
                                , i.bre = seq(mini, maxi, 2)
                                , i.lab = seq(mini, maxi, 2)
                                , i.title = paste0("GRAPH C : map of PFG richness - Simulation year : ", y)
                                , i.subtitle = paste0("For each pixel and stratum, first relative abundances are calculated, "
                                                      , "then transformed into binary values.\n"
                                                      , "If the PFG is present in one stratum, then it is considered present within the pixel.\n"
                                                      , "Finally, simulated PFG occurrences are summed.\n"))
        
        ## PFG CWM LIGHT --------------------------------------------------------
        if (GLOB_SIM$doLight && exists("ras.CWM.light"))
        {
          cat("\n> PFG CWM light...")
          ras.pts = as.data.frame(rasterToPoints(ras.CWM.light))
          colnames(ras.pts) = c("X", "Y", "VALUE")
          pp_list$CWM.light = pp.i(tab = ras.pts
                                   , i.col = rev(brewer.pal(9, "YlOrRd"))
                                   , i.axis = "PFG light CWM"
                                   , i.lim = c(0, 4)
                                   , i.bre = c(1, 2, 3)
                                   , i.lab = c("Low", "Medium", "High")
                                   , i.title = paste0("GRAPH C : map of light CWM - Simulation year : ", y)
                                   , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                         , opt.stratum_min, " to ", GLOB_SIM$no_STRATA, " are summed,\n"
                                                         , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                                         , "Community Weighted Mean is then calculated with observed values of light for each PFG.\n"))
        }
        
        if (GLOB_SIM$doLight && exists("ras.light"))
        {
          cat("\n> Pixel light resources...")
          ras.pts = as.data.frame(rasterToPoints(ras.light))
          ras.pts = melt(ras.pts, id.vars = c("x", "y"))
          colnames(ras.pts) = c("X", "Y", "VARIABLE", "VALUE")
          pp_list$light = pp.i(tab = ras.pts
                               , i.col = rev(brewer.pal(9, "YlOrRd"))
                               , i.axis = "Pixel light resources"
                               , i.lim = c(1, 3)
                               , i.bre = c(1, 2, 3)
                               , i.lab = c("Low", "Medium", "High")
                               , i.title = paste0("GRAPH C : map of pixel light resources - Simulation year : ", y)
                               , i.subtitle = "\n\n\n")
          pp_list$light = pp_list$light + facet_wrap(~ VARIABLE)
        }
        
        ## PFG CWM SOIL ---------------------------------------------------------
        if (GLOB_SIM$doSoil && exists("ras.CWM.soil"))
        {
          cat("\n> PFG CWM soil...")
          ras.pts = as.data.frame(rasterToPoints(ras.CWM.soil))
          colnames(ras.pts) = c("X", "Y", "VALUE")
          mini = floor(min(ras.pts$VALUE, na.rm = TRUE))
          maxi = ceiling(max(ras.pts$VALUE, na.rm = TRUE))
          pp_list$CWM.soil = pp.i(tab = ras.pts
                                  , i.col = brewer.pal(9, "YlGnBu")
                                  , i.axis = "PFG soil CWM"
                                  , i.lim = c(mini, maxi)
                                  , i.bre = seq(mini, maxi, 1)
                                  , i.lab = seq(mini, maxi, 1)
                                  , i.title = paste0("GRAPH C : map of soil CWM - Simulation year : ", y)
                                  , i.subtitle = paste0("For each pixel, PFG abundances from strata "
                                                        , opt.stratum_min, " to ", GLOB_SIM$no_STRATA, " are summed,\n"
                                                        , "then transformed into relative values by dividing by the maximum abundance obtained.\n"
                                                        , "Community Weighted Mean is then calculated with observed values of soil for each PFG.\n"))
        }
        
        if (GLOB_SIM$doSoil && exists("ras.soil"))
        {
          cat("\n> Pixel soil resources...")
          ras.pts = as.data.frame(rasterToPoints(ras.soil))
          colnames(ras.pts) = c("X", "Y", "VALUE")
          mini = floor(min(ras.pts$VALUE, na.rm = TRUE))
          maxi = ceiling(max(ras.pts$VALUE, na.rm = TRUE))
          pp_list$soil = pp.i(tab = ras.pts
                              , i.col = brewer.pal(9, "YlGnBu")
                              , i.axis = "Pixel soil resources"
                              , i.lim = c(mini, maxi)
                              , i.bre = seq(mini, maxi, 1)
                              , i.lab = seq(mini, maxi, 1)
                              , i.title = paste0("GRAPH C : map of pixel soil resources - Simulation year : ", y)
                              , i.subtitle = "\n\n\n")
        }
        
        return(list(ras = ras_list, plot = pp_list))
      } ## END opt.doPlot
      cat("\n")
    } ## END loop on years
    names(year_list) = years
    
    ## SAVE plots into file -------------------------------------------------
    if (opt.doPlot && sum(sapply(year_list, is.null)) < length(year_list))
    {
      pdf(file = paste0(name.simulation
                        , "/RESULTS/POST_FATE_GRAPHIC_C_map_PFG_"
                        , basename(GLOB_DIR$dir.save)
                        , "_STRATA_", name_strata, ".pdf")
          , width = 12, height = 10)
      for (y in years)
      {
        for (pp in year_list[[as.character(y)]]$plot)
        {
          if (!is.null(pp)) plot(pp)
        }
      }
      dev.off()
    }
    
    ## ZIP the raster saved -------------------------------------------------
    if (opt.doStrata)
    {
      .zip(folder_name = GLOB_DIR$dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , no_cores = opt.no_CPU)
    } else
    {
      .zip(folder_name = GLOB_DIR$dir.output.perPFG.allStrata
           , list_files = raster.perPFG.allStrata
           , no_cores = opt.no_CPU)
    }
    if (GLOB_SIM$doLight)
    {
      .zip(folder_name = GLOB_DIR$dir.output.light
           , list_files = raster.light.perStrata
           , no_cores = opt.no_CPU)
    }
    if (GLOB_SIM$doSoil)
    {
      .zip(folder_name = GLOB_DIR$dir.output.soil
           , list_files = raster.soil
           , no_cores = opt.no_CPU)
    }
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(year_list)
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}
