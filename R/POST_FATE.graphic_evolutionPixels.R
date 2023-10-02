### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG abundance 
##' through time for 5 (or more) pixels of a \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_evolutionPixels
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce one graphical representation 
##' for a \code{FATE} simulation : the evolution through time of the 
##' abundance of each PFG for 5 (or more) randomly selected cells of the studied 
##' area.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param opt.cells_ID (\emph{optional}) default \code{NULL}. \cr The cells ID 
##' of the studied area for which PFG abundances will be extracted
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, one preanalytical graphic :
##' 
##' \itemize{
##'   \item{the evolution of \strong{abundance} of each Plant Functional Group 
##'   through simulation time, within 5 (or more) randomly selected pixels of 
##'   the studied area (\code{FATE} \emph{arbitrary unit})
##'   }
##'   \item{\strong{if light was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
##'   \strong{light resources} within the selected pixels is also represented 
##'   (\emph{\code{1}: Low, \code{2}: Medium, \code{3}: High})
##'   }
##'   \item{\strong{if soil was activated} (see 
##'   \code{\link{PRE_FATE.params_globalParameters}}), \cr evolution of 
##'   \strong{soil resources} within the selected pixels is also represented 
##'   (user-defined scale) \cr \cr
##'   }
##' }
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.temporalEvolution}} 
##' function has been run and that the file 
##' \code{POST_FATE_TABLE_PIXEL_evolution_abundance.csv} exists (as well as the 
##' \code{POST_FATE_TABLE_PIXEL_evolution_light.csv} and 
##' \code{POST_FATE_TABLE_PIXEL_evolution_soil.csv} files if those modules were 
##' activated).
##' 
##' 
##' @return A \code{list} containing one \code{data.frame} object with the 
##' following columns, and one \code{ggplot2} object :
##' 
##' \describe{
##'   \item{tab}{
##'     \describe{
##'       \item{\code{TYPE}}{concerned information (either '\code{light}', 
##'       '\code{abundance}' or '\code{soil}')}
##'       \item{\code{GROUP}}{concerned entity (either 
##'       '\code{STRATUM_[...]}', PFG name or '\code{soil}')}
##'       \item{\code{ID.pixel}}{number of the concerned pixel}
##'       \item{\code{HAB}}{habitat of the concerned pixel}
##'       \item{\code{YEAR}}{concerned simulation year}
##'       \item{\code{value}}{concerned value extracted from \code{.csv} files 
##'       produced by \code{\link{POST_FATE.temporalEvolution}}}
##'     }
##'   }
##'   \item{plot}{\code{ggplot2} object, representing the evolution of each PFG 
##'   abundance, \emph{and light and soil resources if those modules were 
##'   activated} \cr \cr}
##' }
##' 
##' 
##' One \code{POST_FATE_TABLE_PIXEL_evolution_pixels_[...].csv} file is created : 
##' \describe{
##'   \item{\emph{pixels ids}}{always, containing the \code{data.frame} detailed 
##'   above}
##' }
##' 
##' 
##' One \code{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr pixels}}{to visualize for each PFG the evolution 
##'   of its abundance within each selected pixel through simulation time}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom grDevices colorRampPalette
##' @importFrom reshape2 melt
##' @importFrom foreach foreach %do%
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string 
##' geom_line geom_point
##' scale_color_manual
##' facet_grid labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
## @importFrom ggnewscale new_scale_color
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_evolutionPixels = function(
  name.simulation
  , file.simulParam = NULL
  , opt.cells_ID = NULL
  , opt.doPlot = TRUE
){
  
  #############################################################################
  
  if (!isNamespaceLoaded("ggnewscale")) { 
    if (!requireNamespace('ggnewscale', quietly = TRUE)) stop("Package 'ggnewscale' not found")
  }
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.graphic_evolutionPixels")
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
    
    ## Get the abundance table ------------------------------------------------
    file.abundance = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(GLOB_DIR$dir.save)
                            , ".csv")
    .testParam_existFile(file.abundance)
    tab.abundance = fread(file.abundance)
    tab.abundance = as.data.frame(tab.abundance, stringsAsFactors = FALSE)
    tab.abundance$TYPE = "abundance"
    colnames(tab.abundance)[which(colnames(tab.abundance) == "PFG")] = "GROUP"
    
    years = colnames(tab.abundance)
    years = years[which(!(years %in% c("TYPE", "GROUP", "ID.pixel", "X", "Y", "HAB")))]
    years = as.numeric(years)
    
    strata = paste0("Stratum ", 0:(GLOB_SIM$no_STRATA - 1))
    
    ## Get resources tables ---------------------------------------------------
    if (GLOB_SIM$doLight)
    {
      file.light = paste0(name.simulation
                          , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_light_"
                          , basename(GLOB_DIR$dir.save)
                          , ".csv")
      .testParam_existFile(file.light)
      tab.light = fread(file.light)
      tab.light = as.data.frame(tab.light, stringsAsFactors = FALSE)
      tab.light$STRATUM = paste0("Stratum ", tab.light$STRATUM)
      tab.light$TYPE = "light"
      colnames(tab.light)[which(colnames(tab.light) == "STRATUM")] = "GROUP"
      
      tab.abundance = rbind(tab.abundance, tab.light)
    }
    
    if (GLOB_SIM$doSoil)
    {
      file.soil = paste0(name.simulation
                         , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_soil_"
                         , basename(GLOB_DIR$dir.save)
                         , ".csv")
      .testParam_existFile(file.soil)
      tab.soil = fread(file.soil)
      tab.soil = as.data.frame(tab.soil, stringsAsFactors = FALSE)
      tab.soil$GROUP = "soil"
      tab.soil$TYPE = "soil"
      
      tab.abundance = rbind(tab.abundance, tab.soil)
    }
    
    
    ## Get concerned cells id -------------------------------------------------
    IDS = sort(sample(unique(tab.abundance$ID.pixel), 5))
    if (!is.null(opt.cells_ID))
    {
      if (sum(opt.cells_ID %in% GLOB_MASK$ind_1_mask) == length(opt.cells_ID))
      {
        IDS = sort(opt.cells_ID)
      } else
      {
        warning(paste0("The values given in `opt.cells_ID` do not match "
                       , "with any cells of the studied area \n"
                       , "(obtained from the raster file `"
                       , GLOB_MASK$file.mask
                       , "`)\n"
                       , "They will be replaced by randomly selected cells."))
      }
    }
    
    cat("\n  Number of years : ", length(years))
    cat("\n  Selected years : ", years)
    cat("\n  Selected cells : ", IDS)
    cat("\n")
    
    ## Transform the data inside the table ------------------------------------
    distriAbund = tab.abundance[which(tab.abundance$ID.pixel %in% IDS), , drop = FALSE]
    distriAbund = distriAbund[, c("TYPE", "GROUP", "ID.pixel", "HAB", as.character(years))]
    distriAbund = melt(distriAbund, id.vars = c("TYPE", "GROUP", "ID.pixel", "HAB"))
    colnames(distriAbund) = c("TYPE", "GROUP", "ID.pixel", "HAB", "YEAR", "value")
    distriAbund$YEAR = as.numeric(as.character(distriAbund$YEAR))
    distriAbund$TYPE = factor(distriAbund$TYPE, c("light", "abundance", "soil"))
    distriAbund$GROUP = factor(distriAbund$GROUP, c(strata, GLOB_SIM$PFG, "soil"))
    
    write.csv(distriAbund
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_pixels_"
                              , ifelse(length(IDS) <= 5
                                       , paste0(IDS, collapse = "_")
                                       , length(IDS))
                              , "_"
                              , basename(GLOB_DIR$dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    message(paste0("\n The output file \n"
                   , " > POST_FATE_TABLE_PIXEL_evolution_pixels_"
                   , ifelse(length(IDS) <= 5
                            , paste0(IDS, collapse = "_")
                            , length(IDS))
                   , "_"
                   , basename(GLOB_DIR$dir.save)
                   , ".csv \n"
                   , "has been successfully created !\n"))
    
    ## produce the plot -------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n ---------- PRODUCING PLOT \n")
      vec_col1 = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')
      val_col1 = c(rep(rgb(1,1,1,1), GLOB_SIM$no_STRATA)
                   , colorRampPalette(vec_col1)(GLOB_SIM$no_PFG)
                   , "grey30")
      names(val_col1) = c(strata, GLOB_SIM$PFG, "soil")
      
      vec_col2 = c('#FEC44F', '#FB9A29', '#EC7014', '#CC4C02', '#993404', '#662506')
      val_col2 = rev(colorRampPalette(vec_col2)(3))
      names(val_col2) = 1:3
      
      
      pp = ggplot(distriAbund, aes_string(x = "YEAR", y = "value")) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "soil"),]
                  , color = "grey30"
                  , lwd = 0.7) +
        geom_line(data = distriAbund[which(distriAbund$TYPE == "abundance"),]
                  , aes_string(color = "GROUP")
                  , lwd = 0.7) +
        scale_color_manual("", values = val_col1) +
        ggnewscale::new_scale_color() +
        geom_point(data = distriAbund[which(distriAbund$TYPE == "light"), ]
                   , aes_string(x = "YEAR", y = "as.numeric(GROUP)-1", color = "factor(value)")
                   , alpha = 0.4, pch = 15, size = 15
                   , inherit.aes = FALSE) +
        scale_color_manual("", values = val_col2, labels = c("Low", "Medium", "High")) +
        facet_grid("TYPE ~ ID.pixel", scales = "free_y") +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' abundance")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its abundance\n"
                                 , "(as well as the light and soil resources if available)"
                                 , "for the selected pixels within the studied area.\n")) +
        .getGraphics_theme()
      
      ggsave(filename = paste0(name.simulation
                               , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_pixels_"
                               , ifelse(length(IDS) <= 5
                                        , paste0(IDS, collapse = "_")
                                        , length(IDS))
                               , "_"
                               , basename(GLOB_DIR$dir.save)
                               , ".pdf")
             , plot = pp, width = 10, height = 8)
    } else
    {
      pp = NULL
    } ## END opt.doPlot
    
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(list(tab = distriAbund, plot = pp))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

