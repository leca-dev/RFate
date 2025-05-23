### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of PFG coverage
##' and abundance through time for a \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_evolutionCoverage
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce two graphical 
##' representations for a \code{FATE} simulation : 1) the evolution through
##' time of the space occupation of each PFG ; 2) the evolution through time
##' of the abundance of each PFG. These graphics represent both the evolution 
##' over the whole area. 
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param opt.fixedScale (\emph{optional}) default \code{TRUE}. \cr If 
##' \code{FALSE}, the ordinate scale will be adapted for each PFG for the 
##' graphical representation of the evolution of abundances through time
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, two preanalytical 
##' graphics : 
##' 
##' \itemize{
##'   \item{the evolution of \strong{space occupancy} of each plant functional 
##'   group through simulation time, \cr with \emph{space occupancy} 
##'   representing the percentage of pixels within the mask of studied area 
##'   where the PFG is present
##'   }
##'   \item{the evolution of \strong{total abundance} of each plant functional 
##'   group through simulation time, \cr with \emph{total abundance} being the 
##'   sum over the whole studied area of the PFG abundances (\code{FATE} 
##'   \emph{arbitrary unit})
##'   }
##' }
##' 
##' If the information has been provided (see 
##' \code{\link{POST_FATE.temporalEvolution}}), the graphics will be also done 
##' per habitat. \cr \cr
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.temporalEvolution}} 
##' function has been run and that the file 
##' \code{POST_FATE_TABLE_PIXEL_evolution_abundance.csv} exists.
##' 
##' 
##' 
##' @return A \code{list} containing two \code{data.frame} objects with the 
##' following columns, and two \code{ggplot2} objects :
##' 
##' \describe{
##'   \item{tab.spaceOccupancy}{
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group (for abundance)}
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\emph{year}}{concerned simulation year}
##'       \item{\code{spaceOccupancy}}{number of occupied pixels divided by the 
##'       total number of pixels within the studied area}
##'     }
##'   }
##'   \item{tab.totalAbundance}{
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group (for abundance)}
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\emph{year}}{concerned simulation year}
##'       \item{\code{totalAbundance}}{total abundance over all the pixels 
##'       within the studied area}
##'     }
##'   }
##'   \item{plot.spaceOccupancy}{\code{ggplot2} object, representing the 
##'   evolution of each PFG space occupancy}
##'   \item{plot.totalAbundance}{\code{ggplot2} object, representing the 
##'   evolution of each PFG total abundance \cr \cr}
##' }
##' 
##' 
##' Two \code{POST_FATE_TABLE_ZONE_evolution_[...].csv} files are created : 
##' \describe{
##'   \item{\file{spaceOccupancy}}{always, containing \code{tab.spaceOccupancy}}
##'   \item{\file{totalAbundance}}{always, containing \code{tab.totalAbundance}}
##' }
##' 
##' 
##' One \code{POST_FATE_GRAPHIC_A_evolution_coverage_[...].pdf} file is created 
##' containing two types of graphics : 
##' \describe{
##'   \item{spaceOccupancy}{to visualize for each PFG the evolution of its 
##'   occupation of the studied area through simulation time}
##'   \item{totalAbundance}{to visualize for each PFG the evolution of its 
##'   abundance within the whole studied area through simulation time}
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
##' @importFrom data.table fread
##' @importFrom grDevices pdf dev.off
##' 
##' @importFrom ggplot2 ggplot aes_string 
##' geom_line 
##' scale_color_manual
##' facet_wrap labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################



POST_FATE.graphic_evolutionCoverage = function(
  name.simulation
  , file.simulParam = NULL
  , opt.fixedScale = TRUE
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
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.graphic_evolutionCoverage")
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
    
    ## Get raster mask --------------------------------------------------------
    GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                  , abs.simulParam = abs.simulParam)
    
    ## Get the abundance table ------------------------------------------------
    file.abundance = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(GLOB_DIR$dir.save)
                            , ".csv")
    .testParam_existFile(file.abundance)
    tab.totalAbundance = fread(file.abundance)
    tab.totalAbundance = as.data.frame(tab.totalAbundance, stringsAsFactors = FALSE)
    
    years = colnames(tab.totalAbundance)
    years = years[which(!(years %in% c("PFG", "ID.pixel", "X", "Y", "HAB")))]
    years = as.numeric(years)
    
    hab_names = sort(unique(tab.totalAbundance$HAB))
    no_hab = length(hab_names)
    
    cat("\n  Number of years : ", length(years))
    cat("\n  Number of habitat : ", no_hab)
    cat("\n")
    
    ## Transform the data inside the table ------------------------------------
    cat("\n ---------- GETTING COVERAGE and ABUNDANCE over the whole area...")
    
    tab.totalAbundance.split = split(tab.totalAbundance
                                     , list(tab.totalAbundance$PFG
                                            , tab.totalAbundance$HAB))
    distriAbund.melt = foreach(i = 1:length(tab.totalAbundance.split)
                               , .combine = "rbind"
    ) %do%
    {
      pfg = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][1]
      hab = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][2]
      tab = tab.totalAbundance.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , totalAbundance = colSums(tab, na.rm = TRUE)
                          , stringsAsFactors = FALSE)) 
      }
    }
    
    distri.melt = foreach(i = 1:length(tab.totalAbundance.split)
                          , .combine = "rbind"
    ) %do%
    {
      pfg = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][1]
      hab = strsplit(names(tab.totalAbundance.split)[i], "[.]")[[1]][2]
      tab = tab.totalAbundance.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        tab = as.matrix(tab)
        tab = apply(tab, 2, function(x) length(which(x > 0)))
        
        return(data.frame(PFG = pfg, HAB = hab, YEAR = years
                          , spaceOccupancy = tab / GLOB_MASK$no_1_mask
                          , stringsAsFactors = FALSE))
      }
    }
    cat("\n")
    
    write.csv(distri.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_ZONE_evolution_spaceOccupancy_"
                              , basename(GLOB_DIR$dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    write.csv(distriAbund.melt
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_ZONE_evolution_totalAbundance_"
                              , basename(GLOB_DIR$dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    message(paste0("\n The output files \n"
                   , " > POST_FATE_TABLE_ZONE_evolution_spaceOccupancy_"
                   , basename(GLOB_DIR$dir.save)
                   , ".csv \n"
                   , " > POST_FATE_TABLE_ZONE_evolution_totalAbundance_"
                   , basename(GLOB_DIR$dir.save)
                   , ".csv \n"
                   , "have been successfully created !\n"))
    
    
    ## produce the plot -------------------------------------------------------
    if (opt.doPlot)
    {
      cat("\n ---------- PRODUCING PLOTS \n")
      col_vec = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
      col_fun = colorRampPalette(col_vec)
      list.pp1 = list.pp2 = list()
      
      ## ----------------------------------------------------------------------
      pdf(file = paste0(name.simulation
                        , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_coverage_"
                        , basename(GLOB_DIR$dir.save), ".pdf")
          , width = 10, height = 8)
      

      ## ----------------------------------------------------------------------
      ## Evolution of space occupation
      pp1 = ggplot(distri.melt, aes_string(x = "YEAR"
                                           , y = "spaceOccupancy * 100"
                                           , color = "factor(HAB, hab_names)")) +
        geom_line(lwd = 1) +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        geom_line(lwd = 1) +
        facet_wrap("~ PFG") +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' space occupation")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its space "
                                 , "occupancy,\n meaning the percentage of "
                                 , "pixels in which the abundance of the "
                                 , "species is greater than 0.\n")) +
        .getGraphics_theme()
      
      plot(pp1)
      list.pp1[["ALL"]] = pp1

      ## ----------------------------------------------------------------------
      ## Evolution of abundance
      pp2 = ggplot(distriAbund.melt, aes_string(x = "YEAR"
                                                , y = "totalAbundance"
                                                , color = "factor(HAB, hab_names)")) +
        geom_line(lwd = 1) +
        scale_color_manual("Habitat", values = col_fun(no_hab)) +
        facet_wrap("~ PFG", scales = ifelse(opt.fixedScale, "fixed", "free_y")) +
        labs(x = "", y = ""
             , title = paste0("GRAPH A : evolution of species' abundance")
             , subtitle = paste0("For each PFG, the line represents the "
                                 , "evolution through time of its abundance\n"
                                 , "over the whole studied area, meaning the "
                                 , "sum of its abundances in every pixel.\n")) +
        .getGraphics_theme()
      
      plot(pp2)
      list.pp2[["ALL"]] = pp2
      
      ## ----------------------------------------------------------------------
      ## SAME per habitat
      if (no_hab > 1)
      {
        for (habi in hab_names)
        {
          tabi = distri.melt[which(distri.melt$HAB == habi), ]
          pp1 = ggplot(tabi, aes_string(x = "YEAR"
                                        , y = "spaceOccupancy * 100")) +
            geom_line(lwd = 1) +
            facet_wrap("~ PFG") +
            labs(x = "", y = ""
                 , title = paste0("GRAPH A : evolution of species' space occupation : ", habi)
                 , subtitle = paste0("For each PFG, the line represents the "
                                     , "evolution through time of its space "
                                     , "occupancy,\n meaning the percentage of "
                                     , "pixels in which the abundance of the "
                                     , "species is greater than 0.\n")) +
            .getGraphics_theme()
          
          plot(pp1)
          list.pp1[[as.character(habi)]] = pp1
        }
        
        for (habi in hab_names)
        {
          tabi = distriAbund.melt[which(distriAbund.melt$HAB == habi), ]
          pp2 = ggplot(tabi, aes_string(x = "YEAR"
                                        , y = "totalAbundance")) +
            geom_line(lwd = 1) +
            facet_wrap("~ PFG", scales = ifelse(opt.fixedScale, "fixed", "free_y")) +
            labs(x = "", y = ""
                 , title = paste0("GRAPH A : evolution of species' abundance : ", habi)
                 , subtitle = paste0("For each PFG, the line represents the "
                                     , "evolution through time of its abundance\n"
                                     , "over the whole studied area, meaning the "
                                     , "sum of its abundances in every pixel.\n")) +
            .getGraphics_theme()
          
          plot(pp2)
          list.pp2[[as.character(habi)]] = pp2
        }
      }
      
      ## ----------------------------------------------------------------------
      dev.off()

    } else
    {
      list.pp1 = list.pp2 = NULL
    } ## END opt.doPlot
    
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(list(tab.spaceOccupancy = distri.melt
                , tab.totalAbundance = distriAbund.melt
                , plot.spaceOccupancy = list.pp1
                , plot.totalAbundance = list.pp2))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

