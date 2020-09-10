### HEADER #####################################################################
##' @title Create a graphical representation of the evolution of habitat 
##' composition through time for a \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_evolutionStability
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce one graphical representation 
##' for a \code{FATE} simulation : the evolution through time of the total 
##' abundance and evenness of each habitat.
##' 
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param movingWindow_size default \code{3}. \cr An \code{integer} 
##' corresponding to the size (\emph{in years}) of the moving window that will 
##' be used to calculate metrics of habitat stability
##' @param movingWindow_step default \code{1}. \cr An \code{integer} 
##' corresponding to the step (\emph{in years}) between the years of the moving 
##' window
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, one preanalytical graphic :
##' 
##' \itemize{
##'   \item{the evolution of \strong{total abundance} (\code{FATE} 
##'   \emph{arbitrary unit}) and \strong{evenness} (\emph{between \code{0} and 
##'   \code{1}}) of each habitat through simulation time, with \emph{evenness} 
##'   representing the uniformity of the species composition of the habitat 
##'   (similar to \strong{Shannon entropy}) :
##'   \deqn{
##'   \text{evenness} = - \frac{\Sigma(\text{proportion}_{\text{ PFG}_i} *
##'   log(\text{proportion}_{\text{ PFG}_i}))}{log(\text{no.PFG})}
##'   }
##'   with \deqn{
##'   \text{proportion}_{\text{ PFG}_i} = \frac{abund_{\text{ PFG}_i
##'   \text{, }\text{Habitat}_j}}{abund_{\text{ PFG}_{all}\text{, }
##'   \text{Habitat}_j}}
##'   }
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
##' following columns, and one \code{ggplot2} object :
##' 
##' \describe{
##'   \item{tab.hab}{
##'     \describe{
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\code{year}}{concerned simulation year}
##'       \item{\code{totalAbundance}}{total abundance over all the pixels 
##'       within the concerned habitat}
##'       \item{\code{no.PFG}}{number of PFG over all the pixels within the 
##'       concerned habitat}
##'       \item{\code{evenness}}{evenness over all the pixels within the 
##'       concerned habitat}
##'     }
##'   }
##'   \item{tab.stab}{
##'     \describe{
##'       \item{\code{HAB}}{concerned habitat}
##'       \item{\code{no.years}}{number of simulation years used (\emph{moving 
##'       window size})}
##'       \item{\code{yearStep}}{step between each simulation year of the moving 
##'       window}
##'       \item{\code{yearStart}}{first simulation year of the moving window}
##'       \item{\code{yearEnd}}{last simulation year of the moving window}
##'       \item{\code{metric}}{concerned metric (either \code{totalAbundance} or 
##'       \code{evenness})}
##'       \item{\code{mean}}{mean value of the concerned metric over the years 
##'       of the concerned moving window}
##'       \item{\code{sd}}{value of standard deviation of the concerned metric 
##'       over the years of the concerned moving window}
##'       \item{\code{cv}}{value of coefficient of variation of the concerned 
##'       metric over the years of the concerned moving window}
##'     }
##'   }
##'   \item{plot.stab}{\code{ggplot2} object, representing the evolution of 
##'   total abundance and evenness of each habitat \cr \cr}
##' }
##' 
##' Two \code{POST_FATE_TABLE_HAB_evolution_[...].csv} files are created : 
##' \describe{
##'   \item{\file{stability1}}{always, containing \code{tab.hab}}
##'   \item{\file{stability2}}{\emph{if successive years available}, containing 
##'   \code{tab.stab}}
##' }
##' 
##' 
##' One \code{POST_FATE_[...].pdf} files is created : 
##' \describe{
##'   \item{\file{GRAPHIC_A \cr stability}}{to visualize for each habitat the 
##'   evolution of its total abundance and its evenness through simulation time}
##' }
##' 
##' 
##' @keywords FATE, outputs, abundance through time, evenness
##' 
##' @seealso \code{\link{POST_FATE.temporalEvolution}}
##' 
##' @examples
##' 
##' \dontrun{                      
##' POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt")
##'                                     
##' POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
##'                                     , file.simulParam = "Simul_parameters_V1.txt"
##'                                     , opt.fixedScale = FALSE)
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
##' @importFrom utils write.csv
##' @importFrom data.table fread
##' @importFrom foreach foreach %do%
##' @importFrom reshape2 melt
##' @importFrom grDevices colorRampPalette
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string 
##' geom_line geom_point geom_rect
##' scale_color_manual scale_fill_manual
##' facet_grid labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################



POST_FATE.graphic_evolutionStability = function(
  name.simulation
  , file.simulParam = NULL
  , movingWindow_size = 3
  , movingWindow_step = 1
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
  ## CHECK parameter movingWindow_size
  .testParam_notInteger.m("movingWindow_size", movingWindow_size)
  ## CHECK parameter movingWindow_step
  .testParam_notInteger.m("movingWindow_step", movingWindow_step)

  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.graphic_evolutionStability")
  cat("\n #------------------------------------------------------------# \n")
    
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
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
    
    ## Get the abundance table ------------------------------------------------
    file.abundance = paste0(name.simulation
                            , "/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_"
                            , basename(dir.save)
                            , ".csv")
    .testParam_existFile(file.abundance)
    tab.totalAbundance = fread(file.abundance)
    tab.totalAbundance = as.data.frame(tab.totalAbundance, stringsAsFactors = FALSE)
    tab.totalAbundance$HAB = as.character(tab.totalAbundance$HAB)
    
    years = colnames(tab.totalAbundance)
    years = years[which(!(years %in% c("PFG", "ID.pixel", "X", "Y", "HAB")))]
    years = as.numeric(years)
    no_years = length(years)
    
    hab_names = unique(tab.totalAbundance$HAB)
    no_hab = length(hab_names)
    
    cat("\n  Number of years : ", no_years)
    cat("\n  Number of habitat : ", no_hab)
    cat("\n")
    
    ###########################################################################
    ## Transform the data inside the table ------------------------------------
    cat("\n ---------- GETTING TOTAL ABUNDANCE and EVENNESS within each habitat... \n")
    
    ## Getting total abundance within each habitat
    cat("\n> Habitat total abundance...")
    tab.split = split(tab.totalAbundance, list(tab.totalAbundance$HAB))
    tab.hab = foreach(i = 1:length(tab.split), .combine = "rbind") %do%
    {
      hab = names(tab.split)[i]
      tab = tab.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        return(data.frame(HAB = hab, year = years
                          , totalAbundance = colSums(tab, na.rm = TRUE)
                          , stringsAsFactors = FALSE))
      }
    }
    
    ## Getting PFG relative abundance within each habitat
    cat("\n> PFG relative abundance...")
    tab.split = split(tab.totalAbundance, list(tab.totalAbundance$PFG
                                               , tab.totalAbundance$HAB))
    tab.prop = foreach(i = 1:length(tab.split), .combine = "rbind") %do%
    {
      pfg = strsplit(names(tab.split)[i], "[.]")[[1]][1]
      hab = strsplit(names(tab.split)[i], "[.]")[[1]][2]
      tab = tab.split[[i]]
      if (nrow(tab) > 0)
      {
        tab = tab[, as.character(years), drop = FALSE]
        tot.pfg = colSums(tab, na.rm = TRUE)
        tot.hab = tab.hab[which(tab.hab$HAB == hab), ]
        rownames(tot.hab) = tot.hab$year
        
        prop = tot.pfg / tot.hab[as.character(years), "totalAbundance"]
        return(data.frame(PFG = pfg, HAB = hab, year = years
                          , PROP = prop, stringsAsFactors = FALSE)) 
      }
    }
    
    ## Calculating evenness within each habitat
    cat("\n> Habitat evenness...")
    tab.split = split(tab.prop, list(tab.prop$HAB, tab.prop$year))
    tab.even = foreach(i = 1:length(tab.split), .combine = "rbind") %do%
    {
      hab = strsplit(names(tab.split)[i], "[.]")[[1]][1]
      ye = strsplit(names(tab.split)[i], "[.]")[[1]][2]
      tab = tab.split[[i]]
      if (nrow(tab) > 0)
      {
        even = (- sum(tab$PROP * log(tab$PROP)) / log(nrow(tab)))
        return(data.frame(HAB = hab, year = ye, no.PFG = nrow(tab)
                          , evenness = even, stringsAsFactors = FALSE)) 
      }
    }
    
    ## Merging total abundance and evenness for each habitat
    if (nrow(tab.even) > 0) {
      tab.HAB = merge(tab.hab, tab.even, by = c("HAB", "year"))
    } else {
      tab.HAB = tab.hab
    }
    
    cat("\n")
    
    write.csv(tab.HAB
              , file = paste0(name.simulation
                              , "/RESULTS/POST_FATE_TABLE_HAB_evolution_stability1_"
                              , basename(dir.save)
                              , ".csv")
              , row.names = FALSE)
    
    message(paste0("\n The output file \n"
                   , " > POST_FATE_TABLE_HAB_evolution_stability1_"
                   , basename(dir.save)
                   , ".csv \n"
                   , "has been successfully created !\n"))
    
    ###########################################################################
    cat("\n ---------- EVALUATE, if possible, ABUNDANCE and EVENNESS STABILITY within each habitat... \n")
    
    .getSucc = function(vec.years)
    {
      if (length(vec.years) > 1)
      {
        ## gaps between years
        gaps = vec.years[2:length(vec.years)] - vec.years[1:(length(vec.years) - 1)]
        ## periods with successive years
        gaps = ifelse(gaps == movingWindow_step, 1, 0)
        succ = c(gaps[1], rep(0, length(gaps) - 1))
        for (i in 2:length(gaps))
        {
          if (gaps[i] == 1){
            succ[i] = succ[i - 1] + 1
          } else {
            succ[i] = 0
          }
        }
        sel = vec.years[which(succ > 0) + 1]
        if (succ[1] == 1) sel = c(vec.years[1], sel)
        return(list(selected = sel, max = max(succ, na.rm = TRUE) + 1))
      } else
      {
        return(list(selected = vec.years, max = length(vec.years)))
      }
    }
    
    ## Calculate number of successive years -----------------------------------
    max_years.succ = .getSucc(years)$max
    years.succ = .getSucc(years)$selected
    no_years.succ = length(years.succ)
    no_movingWindow = no_years.succ - movingWindow_size + 1
    
    ## Evaluate if stability can be done --------------------------------------
    if (length(years.succ) < movingWindow_size || 
        movingWindow_size > max_years.succ ||
        no_movingWindow < 1)
    {
      tab.STAB = pp = NULL
      warning(paste0("Missing data!\n No moving window can be defined "
                     , "with the current parameters and available data :\n"
                     , " > mw_size = ", movingWindow_size, "\n"
                     , " > number of successive years = ", no_years.succ, "\n"
                     , " > maximum number of successive years = ", max_years.succ
                     , "\n Please check."))
    } else
    {
      tab.split = split(tab.HAB, list(tab.HAB$HAB))
      tab.STAB = foreach(i = 1:length(tab.split), .combine = "rbind") %do%
      {
        hab = names(tab.split)[i]
        tab = tab.split[[i]]
        if (nrow(tab) > 0)
        {
          combi = expand.grid(mw = 1:no_movingWindow, metric = c("totalAbundance", "evenness")
                              , stringsAsFactors = FALSE)
          tab.mw = foreach(mw = combi$mw
                           , metric = combi$metric
                           , .combine = "rbind"
          ) %do%
          {
            mw_years = years[mw:(mw + movingWindow_size - 1)]
            mw_years = .getSucc(mw_years)$selected
            
            tab.mw = tab[which(tab$year %in% c(mw_years)), ]
            if (nrow(tab.mw) > 0)
            {
              val = sum(tab.mw[, metric])
              val.sq = sum(tab.mw[, metric] * tab.mw[, metric])
              val.mean = val / movingWindow_size
              val.sd = sqrt(val.sq / movingWindow_size - val.mean * val.mean)
              val.cv = val.sd / val.mean
              return(data.frame(HAB = hab
                                , no.years = length(mw_years)
                                , yearStep = movingWindow_step
                                , yearStart = min(mw_years)
                                , yearEnd = max(mw_years)
                                , metric = metric
                                , mean = val.mean
                                , sd = val.sd
                                , cv = val.cv
                                , stringsAsFactors = FALSE))
            }
          }
          if (nrow(tab.mw) > 0)
          {
            return(tab.mw)
          }
        }
      }
      
      write.csv(tab.STAB
                , file = paste0(name.simulation
                                , "/RESULTS/POST_FATE_TABLE_HAB_evolution_stability2_"
                                , basename(dir.save)
                                , ".csv")
                , row.names = FALSE)
      
      message(paste0("\n The output file \n"
                     , " > POST_FATE_TABLE_HAB_evolution_stability2_"
                     , basename(dir.save)
                     , ".csv \n"
                     , "has been successfully created !\n"))
      
      ## Compare quantiles ----------------------------------------------------
      ## TO BE DONE ??
      
      ## produce the plot -----------------------------------------------------
      if (opt.doPlot && !is.null(tab.HAB))
      {
        cat("\n ---------- PRODUCING PLOT \n")
        col_vec = c('#6da34d', '#297373', '#58a4b0', '#5c4742', '#3f334d')
        col_fun = colorRampPalette(col_vec)
        
        
        ## Evolution of abundance and evenness through time -------------------
        tab.plot1 = melt(tab.HAB, id.vars = c("HAB", "year"))
        colnames(tab.plot1) = c("HAB", "year", "metric", "value")
        tab.plot1$metric = factor(tab.plot1$metric, c("totalAbundance", "evenness", "no.PFG"))
        
        ## plot
        pp = ggplot(tab.plot1, aes_string(x = "year", y = "value", color = "HAB"))
        
        if (!is.null(tab.STAB))
        {
          ## Evolution of stability through time --------------------------------
          tab.plot2 = tab.STAB
          tab.plot2$year_median = sapply(1:nrow(tab.plot2), function(x) {
            median(as.numeric(as.character(tab.plot2[x, c("yearStart", "yearEnd")]))) })
          tab.plot2 = tab.plot2[which(tab.plot2$sd > 0.00001), ]
          
          ## plot
          pp = pp +
            geom_rect(data = tab.plot2, inherit.aes = FALSE, alpha = 0.5
                      , aes_string(xmin = "yearStart", xmax = "yearEnd"
                                   , ymin = "mean - sd"
                                   , ymax = "mean + sd"
                                   , fill = "HAB"))
        }
        
        ## plot
        pp = pp +
          geom_line(lwd = 1) +
          geom_point() +
          facet_grid("metric ~ .", scales = "free_y"
                     , labeller = as_labeller(c("totalAbundance" = "Total abundance"
                                                , "evenness" = "Evenness"
                                                , "no.PFG" = "Number of PFG"))) +
          scale_color_manual("Habitat", values  = col_fun(no_hab)) +
          scale_fill_manual(guide = FALSE, values  = col_fun(no_hab)) +
          labs(x = "", y = ""
               , title = paste0("GRAPH A : evolution of habitat composition")) +
          .getGraphics_theme()
        
        
        ggsave(filename = paste0(name.simulation
                                 , "/RESULTS/POST_FATE_GRAPHIC_A_evolution_stability_"
                                 , basename(dir.save), ".pdf")
               , plot = pp, width = 10, height = 8)
        
      } else
      {
        pp = NULL
      } ## END opt.doPlot
    }
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(list(tab.hab = tab.HAB
                , tab.stab = tab.STAB
                , plot.stab = pp))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}

