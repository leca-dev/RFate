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
      tab.totalAbundance = fread(file.abundance)
      tab.totalAbundance = as.data.frame(tab.totalAbundance, stringsAsFactors = FALSE)
      
      years = colnames(tab.totalAbundance)
      years = years[which(!(years %in% c("PFG", "ID.pixel", "X", "Y", "HAB")))]
      years = as.numeric(years)
      
      hab_names = sort(unique(tab.totalAbundance$HAB))
      no_hab = length(hab_names)
      
      file.globalParam = .getParam(params.lines = abs.simulParam
                                   , flag = "GLOBAL_PARAMS"
                                   , flag.split = "^--.*--$"
                                   , is.num = FALSE)
      path.globalParam = paste0(sub(basename(name.simulation), "", name.simulation)
                                , file.globalParam)
      seeding = .getParam(params.lines = path.globalParam
                          , flag = "SEEDING_DURATION"
                          , flag.split = " "
                          , is.num = TRUE)
      
      cat("\n  Number of years : ", length(years))
      cat("\n  Number of habitat : ", no_hab)
      cat("\n")
      
      ## Get the PFG characteristics --------------------------------------------
      succ_files = .getParam(params.lines = abs.simulParam
                             , flag = "PFG_PARAMS_LIFE_HISTORY"
                             , flag.split = "^--.*--$"
                             , is.num = FALSE)
      max_stratum = foreach(fi = succ_files, .combine = "c") %do%
        {
          .getParam(params.lines = fi
                    , flag = "MAX_STRATUM"
                    , flag.split = " "
                    , is.num = TRUE)
        }
      
      light_files = .getParam(params.lines = abs.simulParam
                              , flag = "PFG_PARAMS_LIGHT"
                              , flag.split = "^--.*--$"
                              , is.num = FALSE)
      shade_factor = foreach(fi = light_files, .combine = "c") %do%
        {
          .getParam(params.lines = fi
                    , flag = "SHADE_FACTOR"
                    , flag.split = " "
                    , is.num = TRUE)
        }
      tab_char = data.frame(PFG = GLOB_SIM$PFG, ST = max_stratum, SF = shade_factor)
      
      
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
      
      ind.pix = sample(unique(tab.totalAbundance$ID.pixel), 1000)
      distriPix = tab.totalAbundance[which(tab.totalAbundance$ID.pixel %in% ind.pix), ]
      distriPix = merge(distriPix, tab_char, by = "PFG")
      distriPix.split = split(distriPix, distriPix$ID.pixel)
      
      distriPix.melt = foreach(tab = distriPix.split, .combine = "rbind") %do%
        {
          tab.split = split(tab, list(tab$SF, tab$ST), drop = TRUE)
          TAB = foreach(tabi = tab.split, .combine = "rbind") %do%
            {
              return(data.frame(unique(tabi[, c("ID.pixel", "X", "Y", "HAB", "PFG", "SF", "ST")])
                                , t(colSums(tabi[, 6:(ncol(tabi) - 2)]))))
            }
          return(TAB)
        }
      
      distriPix.melt = melt(distriPix.melt, id.vars = c("ID.pixel", "X", "Y", "HAB", "PFG", "SF", "ST"))
      distriPix.melt$YEAR = as.numeric(sub("X", "", distriPix.melt$variable))
      print(head(distriPix.melt))
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
        list.pp1 = list.pp2 = list.pp3 = list()
        
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
        
        distriPix.melt.split = split(distriPix.melt, list(distriPix.melt$SF, distriPix.melt$ST), drop = TRUE)
        distriPix.quant = foreach(tab = distriPix.melt.split, .combine = "rbind") %do%
          {
            res = data.frame(
              do.call(rbind, tapply(tab$value, tab$YEAR, function(i){ quantile(i, seq(0, 1, 0.1)) }))
              , YEAR = unique(tab$YEAR), ST = unique(tab$ST), SF = unique(tab$SF))
            colnames(res) = c(paste0("QUANT.", seq(0, 1, 0.1) * 100), "YEAR", "ST", "SF")
            return(res)
          }
        distriPix.quant$ST = factor(paste0("Stratum ", distriPix.quant$ST)
                                    , paste0("Stratum ", unique(distriPix.quant$ST)))
        distriPix.quant$SF = factor(paste0("Shade factor ", distriPix.quant$SF)
                                    , paste0("Shade factor ", unique(distriPix.quant$SF)))
        head(distriPix.quant)
        
        
        hop = foreach(tab = distriPix.melt.split, .combine = "rbind") %do%
          {
            res = data.frame(MEAN = tapply(tab$value, tab$YEAR, mean), YEAR = unique(tab$YEAR))
            res_seeding = round(mean(res$MEAN[which(res$YEAR <= seeding)]))
            res_simul = round(mean(res$MEAN[which(res$YEAR > seeding)]))
            res = data.frame(MEAN.seeding = res_seeding, MEAN.simul = res_simul, ST = unique(tab$ST), SF = unique(tab$SF))
            return(res)
          }
        hap = merge(tab_char, hop, by = c("ST", "SF"))
        hop$ST = factor(paste0("Stratum ", hop$ST)
                        , paste0("Stratum ", unique(hop$ST)))
        hop$SF = factor(paste0("Shade factor ", hop$SF)
                        , paste0("Shade factor ", unique(hop$SF)))
        print(hop)
        
        hip = foreach(st = unique(hap$ST), .combine = "rbind") %do%
          {
            tmp = hap[which(hap$ST == st), ]
            if (nrow(tmp) > 0) {
              return(data.frame(ST = st
                                , MAX.seeding = sum(tmp$SF * tmp$MEAN.seeding)
                                , MAX.simul = sum(tmp$SF * tmp$MEAN.simul)))
            }
          }
        print(hip)
        
        # pp3 = ggplot(distriPix.melt, aes_string(x = "YEAR"
        #                                         , y = "value"
        #                                         , group = "ID.pixel"
        #                                         , color = "SF")) +
        #   geom_line(alpha = 0.5) +
        #   facet_grid("SF ~ ST", scales = "free_y") +
        #   .getGraphics_theme()
        
        
        pp3 = ggplot(distriPix.quant, aes_string(x = "YEAR")) +
          geom_ribbon(aes(ymin = QUANT.0, ymax = QUANT.100), fill = "gray30", alpha = 0.2) +
          geom_ribbon(aes(ymin = QUANT.10, ymax = QUANT.90), fill = "gray30", alpha = 0.2) +
          geom_ribbon(aes(ymin = QUANT.20, ymax = QUANT.80), fill = "gray30", alpha = 0.2) +
          geom_ribbon(aes(ymin = QUANT.30, ymax = QUANT.70), fill = "gray30", alpha = 0.2) +
          geom_ribbon(aes(ymin = QUANT.40, ymax = QUANT.60), fill = "gray30", alpha = 0.2) +
          geom_vline(xintercept = seeding, lty = 2, color = "gray20") +
          geom_label_repel(data = hop, aes(x = 150, y = MEAN.seeding, label = MEAN.seeding)) +
          geom_label_repel(data = hop, aes(x = 1500, y = MEAN.simul, label = MEAN.simul)) +
          facet_grid("SF ~ ST", scales = "free_y") +
          labs(x = "", y = ""
               , title = paste0("GRAPH C : evolution of species' pixel abundances (quantiles)")
               , subtitle = paste0("For each MAX_STRATUM and each SHADE_FACTOR, the lines represent\n"
                                   , "the evolution through time of all PFG with these characteristics within a sample of 1000 pixels.\n"
                                   , "It gives an estimation of maximum abundances that can be reached within each height stratum\n"
                                   , "to help parameterize LIGHT_THRESHOLD global parameters.\n")) +
          .getGraphics_theme()
        
        plot(pp3)
        
        ## ----------------------------------------------------------------------
        dev.off()
        
      } else
      {
        list.pp1 = list.pp2 = pp3 = NULL
      } ## END opt.doPlot
      
      
      ## ------------------------------------------------------------------------
      
      cat("\n> Done!\n")
      
      return(list(tab.spaceOccupancy = distri.melt
                  , tab.totalAbundance = distriAbund.melt
                  , tab.maxAbund = distriPix.melt
                  , plot.spaceOccupancy = list.pp1
                  , plot.totalAbundance = list.pp2
                  , plot.maxAbund = pp3))
    } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

