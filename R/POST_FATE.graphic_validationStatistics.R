### HEADER #####################################################################
##' @title Create a graphical representation of several statistics for each PFG 
##' to asses the quality of the model for one (or several) specific year of a 
##' \code{FATE} simulation
##' 
##' @name POST_FATE.graphic_validationStatistics
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce a graphical representation 
##' of several statistics (sensitivity, specificity, TSS, AUC) for quality 
##' assessment for one (or several) specific \code{FATE} simulation year.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' relative abundance maps
##' @param mat.PFG.obs a \code{data.frame} with 4 columns : \code{PFG}, 
##' \code{X}, \code{Y}, \code{obs}
##' @param opt.ras_habitat (\emph{optional}) default \code{NULL}. \cr 
##' A \code{string} corresponding to the file name of a raster mask, with an 
##' \code{integer} value within each pixel, corresponding to a specific habitat
##' @param opt.doPlot (\emph{optional}) default \code{TRUE}. \cr If \code{TRUE}, 
##' plot(s) will be processed, otherwise only the calculation and reorganization 
##' of outputs will occur, be saved and returned
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{PFG validation 
##' statistic values} and one preanalytical graphic. \cr \cr
##' 
##' 
##' Observation records (presences and absences) are required for each PFG 
##' within the \code{mat.PFG.obs} object :
##' 
##' \describe{
##'   \item{\code{PFG}}{the concerned plant functional group}
##'   \item{\code{X, Y}}{the coordinates of each observation, matching with the 
##'   projection of the mask of \code{name.simulation}}
##'   \item{\code{obs}}{either \code{0} or \code{1} to indicate presence or 
##'   absence \cr \cr}
##' }
##' 
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_REL_perPFG_allStrata} and lead to the 
##' production of one table :
##' 
##' \itemize{
##'   \item{the value of \strong{several statistics to evaluate the predictive 
##'   quality of the model} for each plant functional group \cr 
##'   (\code{\link[PresenceAbsence]{sensitivity}}, 
##'   \code{\link[PresenceAbsence]{specificity}}, 
##'   \code{\link[PresenceAbsence]{auc}}, 
##'   \code{TSS = sensitivity + specificity - 1})}
##' }
##' 
##' \strong{If a raster mask for habitat has been provided}, the values and 
##' graphics will be also calculated per habitat. \cr \cr
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.relativeAbund}} 
##' function has been run and that the folder \code{ABUND_REL_perPFG_allStrata} 
##' exists. \cr \cr
##' 
##' \strong{This \code{.csv} file can then be used by other functions} :
##' 
##' \itemize{
##'   \item to produce maps of PFG \emph{presence / absence} from modelled 
##'   abundances \cr (see \code{\link{POST_FATE.binaryMaps}})
##' }
##' 
##' 
##' @return A \code{list} containing one \code{data.frame} object with the 
##' following columns, and one \code{ggplot2} object :
##' 
##' \describe{
##'   \item{tab}{ 
##'     \describe{
##'       \item{\code{PFG}}{concerned plant functional group}
##'       \item{\code{AUC.sd}}{standard deviation of the AUC values}
##'       \item{\code{sensitivity.sd}}{standard deviation of the sensitivity 
##'       values}
##'       \item{\code{specificity.sd}}{standard deviation of the specificity 
##'       values}
##'       \item{\code{variable}}{name of the calculated statistic among 
##'       \code{sensitivity}, \code{specificity}, \code{TSS} and \code{AUC}}
##'       \item{\code{value}}{value of the corresponding statistic}
##'     }
##'   }
##'   \item{plot}{\code{ggplot2} object, representing the values for each PFG 
##'   of these four validation statistics (sensitivity, specificity, TSS, AUC) 
##'   \cr \cr}
##' }
##' 
##' One \file{POST_FATE_TABLE_YEAR_[...].csv} file is created : 
##' \describe{
##'   \item{\file{validationStatistics}}{containing the \code{data.frame} 
##'   detailed above}
##' }
##' 
##' One \file{POST_FATE_[...].pdf} file is created : 
##' \describe{
##'   \item{\file{GRAPHIC_B \cr validationStatistics}}{to assess the modeling 
##'   quality of each PFG based on given observations within the studied area}
##' }
##' 
##' 
##' @keywords FATE, outputs, area under curve, sensitivity, specificity,
##' true skill statistic
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}},
##' \code{\link[PresenceAbsence]{cmx}},
##' \code{\link[PresenceAbsence]{sensitivity}},
##' \code{\link[PresenceAbsence]{specificity}},
##' \code{\link[PresenceAbsence]{auc}},
##' \code{\link{.getCutoff}},
##' \code{\link{POST_FATE.binaryMaps}}
##' 
##' @examples
##' 
##'                                                         
##'                                                         
##' ## ----------------------------------------------------------------------------------------- ##
##' ## Load example data
##' 
##' @export
##' 
##' @importFrom foreach foreach %do%
##' @importFrom reshape2 melt
##' @importFrom raster raster stack cellFromXY
##' @importFrom grid unit
##' @importFrom PresenceAbsence cmx sensitivity specificity auc
##' 
##' @importFrom ggplot2 ggplot aes_string 
##' geom_bar geom_point geom_hline geom_errorbar annotate
##' scale_fill_gradientn scale_y_continuous
##' labs ylim theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggExtra ggMarginal
##' @importFrom gridExtra grid.arrange
##' @importFrom cowplot get_legend
##' @importFrom RColorBrewer brewer.pal
##' @importFrom grDevices pdf
##' @importFrom graphics plot
##'
## END OF HEADER ###############################################################


POST_FATE.graphic_validationStatistics = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , mat.PFG.obs
  , opt.ras_habitat = NULL
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
  ## CHECK parameter mat.PFG.obs
  if (.testParam_notDf(mat.PFG.obs))
  {
    .stopMessage_beDataframe("mat.PFG.obs")
  } else
  {
    if (nrow(mat.PFG.obs) == 0 || ncol(mat.PFG.obs) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.obs", c("PFG", "X", "Y", "obs"))
    } else
    {
      if(.testParam_notColnames(mat.PFG.obs, c("PFG", "X", "Y", "obs")))
      {
        .stopMessage_columnNames("mat.PFG.obs", c("PFG", "X", "Y", "obs"))
      }
    }
    mat.PFG.obs$PFG = as.character(mat.PFG.obs$PFG)
    mat.PFG.obs = na.exclude(mat.PFG.obs)
    if (nrow(mat.PFG.obs) == 0)
    {
      stop("Missing data!\n Too many NA values. Please check.")
    }
    .testParam_notChar.m("mat.PFG.obs$PFG", mat.PFG.obs$PFG)
    .testParam_notNum.m("mat.PFG.obs$X", mat.PFG.obs$X)
    .testParam_notNum.m("mat.PFG.obs$Y", mat.PFG.obs$Y)
    .testParam_notInValues.m("mat.PFG.obs$obs", mat.PFG.obs$obs, c(0, 1))
  }
  ## CHECK parameter opt.ras_habitat
  if (!.testParam_notDef(opt.ras_habitat))
  {
    .testParam_notChar.m("opt.ras_habitat", opt.ras_habitat)
    .testParam_existFile(opt.ras_habitat)
    ras.habitat = raster(opt.ras_habitat)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.graphic_validationStatistics")
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
    
    ## Get PFG observations ---------------------------------------------------
    mat.PFG.obs = mat.PFG.obs[which(mat.PFG.obs$PFG %in% PFG), ]
    if (nrow(mat.PFG.obs) == 0)
    {
      stop(paste0("Missing data!\n The names of PFG within `mat.PFG.obs`"
                  , " is different from the names of PFG contained from "
                  , name.simulation, "/DATA/PFGS/SUCC/"))
    }
    
    ## Get habitat information ------------------------------------------------
    no_hab = 1
    hab_names = "ALL"
    if (exists("ras.habitat"))
    {
      ras.habitat = ras.habitat * ras.mask
      df.habitat = data.frame(ID = cellFromXY(ras.habitat, xy.1)
                              , stringsAsFactors = FALSE)
      df.habitat$HAB = ras.habitat[df.habitat$ID]
      hab_names = c(hab_names, unique(df.habitat$HAB))
      hab_names = hab_names[which(!is.na(hab_names))]
      no_hab = length(hab_names)
    }
    
    ## Get list of arrays and extract years of simulation ---------------------
    years = sort(unique(as.numeric(years)))
    
    ## UNZIP the raster saved -------------------------------------------------
    raster.perPFG.allStrata.REL = .getRasterNames(years, "allStrata", "REL")
    
    
    ## get the data inside the rasters ----------------------------------------
    cat("\n ---------- GETTING STATISTICS for")
    mat.valid_list = list()
    plot_list = foreach (y = years) %do%
    {
      cat("\n> year", y)
      
      file_name = paste0(dir.output.perPFG.allStrata.REL
                         , "Abund_relative_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_all.tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        mat.y = mat.PFG.obs
        mat.y$ID = as.numeric(as.factor(paste0(mat.y$X, "_", mat.y$Y)))
        mat.y$CELL = cellFromXY(ras, mat.y[, c("X", "Y")])
        mat.y = na.exclude(mat.y)
        
        if (exists("df.habitat"))
        {
          mat.y = merge(mat.y, df.habitat, by.x = "CELL", by.y = "ID", all.x = TRUE)
        } else
        {
          mat.y$HAB = "ALL"
        }
        
        mat.y.split = split(mat.y, mat.y$PFG)
        mat.valid = foreach(i = 1:length(mat.y.split)
                            , .combine = "rbind"
        ) %do%
        {
          fg = names(mat.y.split)[i]
          mat = mat.y.split[[i]]
          isThereValues = TRUE
          
          cat("\n  PFG ", fg)
          if (fg %in% gp)
          {
            if (nrow(mat) > 0)
            {
              mat$fg = ras[[fg]][mat$CELL]
              mat = mat[, c("ID", "CELL", "obs", "fg", "HAB")]
              mat = na.exclude(mat)
            }
            if (nrow(mat) == 0)
            {
              isThereValues = FALSE
            } else
            {
              cat(", habitat")
              mat.split = split(mat, mat$HAB)
              mat.res = foreach (habi = hab_names, .combine = "rbind") %do%
              {
                mat.res.habi = data.frame(PFG = fg, HAB = habi
                                          , auc = NA, sens = NA
                                          , spec = NA, TSS = NA
                                          , stringsAsFactors = FALSE)
                cat(" ", habi)
                if (habi == "ALL")
                {
                  tmp = mat
                } else
                {
                  tmp = mat.split[[as.character(habi)]]
                }
                tmp = tmp[, -which(colnames(tmp) %in% c("Row.names", "HAB")), drop = FALSE]
                
                if (nrow(tmp) > 0 &&
                    length(which(tmp$obs == 0)) > 0 &&
                    length(which(tmp$obs == 1)) > 0)
                {
                  if (habi == "ALL")
                  {
                    assign("cutoff", NULL, envir = .GlobalEnv) 
                    cutoff <<- .getCutoff(Obs = tmp[, "obs"], Fit = tmp[, "fg"])
                  }
                  if (exists("cutoff") && !(is.na(cutoff[1])))
                  {
                    ## Calculate validation statistics ------------------------
                    mat.bin = tmp
                    mat.bin$fg = ifelse(mat.bin$fg >= cutoff$Cut, 1, 0)
                    mat.conf = cmx(mat.bin[, c("ID", "obs", "fg")])
                    sens = sensitivity(mat.conf)
                    spec = specificity(mat.conf)
                    TSS = sens$sensitivity + spec$specificity - 1
                    auc = auc(mat.bin[, c("ID", "obs", "fg")])
                    
                    mat.res.habi = data.frame(PFG = fg, HAB = habi
                                              , cutoff = cutoff$Cut
                                              , auc, sens, spec, TSS
                                              , stringsAsFactors = FALSE)
                  }
                } 
                return(mat.res.habi)
              }
              if (nrow(na.exclude(mat.res)) == 0)
              {
                isThereValues = FALSE
              }
            }
          } else
          {
            isThereValues = FALSE
          }
          if (!isThereValues)
          {
            warning(paste0("Missing data!\n No values for PFG ", fg
                           , ".\n No validation statistics will be produced!"))
            return(data.frame(PFG = fg
                              , cutoff = NA
                              , HAB = "ALL"
                              , AUC = NA, AUC.sd = NA
                              , sensitivity = NA, sensitivity.sd = NA
                              , specificity = NA, specificity.sd = NA
                              , TSS = NA
                              , stringsAsFactors = FALSE))
          } else
          {
            return(mat.res)
          }
        } ## END mat.valid
        cat("\n")
        
        if (nrow(na.exclude(mat.valid)) > 0)
        {
          ## save table -------------------------------------------------------
          write.csv(mat.valid
                    , file = paste0(name.simulation
                                    , "/RESULTS/POST_FATE_TABLE_YEAR_"
                                    , y
                                    , "_validationStatistics_"
                                    , basename(dir.save)
                                    , ".csv")
                    , row.names = FALSE)
          
          message(paste0("\n The output file POST_FATE_TABLE_YEAR_"
                         , y
                         , "_validationStatistics_"
                         , basename(dir.save)
                         , ".csv has been successfully created !\n"))
          
          ## prepare the plot -------------------------------------------------
          mat.valid = melt(mat.valid
                           , id.vars = c("PFG", "HAB", "AUC.sd"
                                         , "sensitivity.sd", "specificity.sd"))
          mat.valid$variable = factor(mat.valid$variable
                                      , c("sensitivity", "TSS"
                                          , "specificity", "AUC"))
          
          mat.valid$AUC.sd[which(mat.valid$variable != "AUC")] = NA
          mat.valid$sensitivity.sd[which(mat.valid$variable != "sensitivity")] = NA
          mat.valid$specificity.sd[which(mat.valid$variable != "specificity")] = NA
          
          mat.valid_list[[as.character(y)]] = mat.valid
          
          mat.valid$hline = 0.5
          mat.valid$hline[which(mat.valid$variable == "AUC")] = 0.8
          mat.valid$hline[which(mat.valid$variable == "TSS")] = 0.4
          
          ## produce the plot -------------------------------------------------
          if (opt.doPlot)
          {
            cat("\n ---------- PRODUCING PLOT(S)")
            plot_list.hab = foreach(habi = hab_names) %do%
            {
              cat("\n> Preparing for habitat ", habi)
              mat.plot = mat.valid[which(mat.valid$HAB == habi), ]
              
              ## 1. get the legend
              pp = ggplot(mat.plot, aes_string(x = "PFG", y = "value", fill = "value")) +
                scale_fill_gradientn(""
                                     , colors = brewer.pal(9, "RdYlGn")
                                     , breaks = seq(0, 1, 0.2)
                                     , limits = c(0, 1)) +
                geom_bar(stat = "identity", na.rm = TRUE) +
                ylim(0, 1) +
                .getGraphics_theme() +
                theme(legend.key.width = unit(2, "lines"))
              
              pp_leg = suppressWarnings(get_legend(pp))
              
              ## 2. get one plot for the title and for each statistic
              pp_list = foreach(vari = c("all", "sensitivity", "specificity", "TSS", "AUC")
              ) %do%
              {
                if (vari == "all"){
                  pp = ggplot(mat.plot, aes_string(x = "PFG"
                                                   , y = "value"
                                                   , fill = "value")) +
                    labs(x = "", y = ""
                         , title = paste0("GRAPH B : validation statistics"
                                          , " - Simulation year : "
                                          , y, " - Habitat ", habi)
                         , subtitle = paste0("Sensitivity (or specificity) measures "
                                             , "the proportion of actual positives "
                                             , "(or negatives) that are correctly "
                                             , "identified as such.\n"
                                             , "True skill statistic (TSS) values "
                                             , "of -1 indicate predictive abilities "
                                             , "of not better than a random model,\n"
                                             , "0 indicates an indiscriminate model "
                                             , "and +1 a perfect model.\n"
                                             , "AUC corresponds to the area under "
                                             , "the ROC curve (Receiver Operating "
                                             , "Characteristic).\n")) +
                    .getGraphics_theme() +
                    theme(panel.grid = element_blank()
                          , axis.text = element_blank())
                } else
                {
                  if (vari == "sensitivity") subti = "Sensitivity - True positive rate"
                  if (vari == "specificity") subti = "Specificity - True negative rate"
                  if (vari == "TSS") subti = "True Skill Statistic (TSS)"
                  if (vari == "AUC") subti = "Area Under Curve (AUC)"
                  pp = ggplot(mat.plot[which(mat.plot$variable == vari), ]
                              , aes_string(x = "PFG", y = "value", fill = "value")) +
                    scale_fill_gradientn(guide = F
                                         , colors = brewer.pal(9, "RdYlGn")
                                         , breaks = seq(0, 1, 0.2)
                                         , limits = c(0, 1)) +
                    scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.08)) +
                    geom_point(alpha = 0) +
                    geom_bar(stat = "identity", na.rm = TRUE) +
                    geom_hline(aes_string(yintercept = "hline")
                               , lty = 2, color = "grey30") +
                    geom_errorbar(aes_string(ymin = "value - sensitivity.sd"
                                             , ymax = "value + sensitivity.sd")
                                  , color = "grey30", na.rm = TRUE) +
                    geom_errorbar(aes_string(ymin = "value - specificity.sd"
                                             , ymax = "value + specificity.sd")
                                  , color = "grey30", na.rm = TRUE) +
                    geom_errorbar(aes_string(ymin = "value - AUC.sd"
                                             , ymax = "value + AUC.sd")
                                  , color = "grey30", na.rm = TRUE) +
                    annotate(geom = "text"
                             , x = no_PFG / 2
                             , y = 1.05
                             , label = subti
                             , size = 4) +
                    .getGraphics_theme() +
                    theme(axis.text.x = element_text(angle = 90))
                  
                  pp = suppressWarnings(ggMarginal(pp
                                                   , type = "boxplot"
                                                   , margins = "y"
                                                   , size = 7))
                }
                
                return(pp)
              }
              
              ## 3. gather everything
              pp_list[[6]] = pp_leg
              pp_final = grid.arrange(grobs = pp_list
                                      , layout_matrix = matrix(c(1,1,2,3,2,3,4,5,4,5,6,6)
                                                               , ncol = 2, byrow = TRUE)
                                      , newpage = ifelse(y == years[1], FALSE, TRUE))                    
              pp_final = cowplot::ggdraw(pp_final) + 
                theme(plot.background = element_rect(fill = "transparent", color = NA))
              # require(patchwork)
              # pp_final <- pp_list[[1]] / (pp_list[[2]] | pp_list[[3]]) / (pp_list[[4]] | pp_list[[5]]) / pp_list[[6]]
              
              return(pp_final)
            } ## END loop on hab_names
            names(plot_list.hab) = hab_names
            return(plot_list.hab)
          } ## END opt.doPlot
        } else
        {
          warning(paste0("Missing data!\n No validation has been calculated for year ", y, "!"))
          return(NULL)
        }
      } ## END condition file_name
    } ## END loop on years
    names(plot_list) = years
    cat("\n")
    
    ## SAVE plots into file ---------------------------------------------------
    if (opt.doPlot && sum(sapply(plot_list, is.null)) < length(plot_list))
    {
      pdf(file = paste0(name.simulation
                        , "/RESULTS/POST_FATE_GRAPHIC_B_validationStatistics_"
                        , basename(dir.save), ".pdf")
          , width = 12, height = 10)
      for (y in years)
      {
        for (habi in hab_names)
        {
          pp = plot_list[[as.character(y)]][[habi]]
          if (!is.null(pp)) plot(pp)
        }
      }
      dev.off()
    }
    
    ## ------------------------------------------------------------------------
    
    cat("\n> Done!\n")
    
    return(list(tab = mat.valid_list, plot = plot_list))
  } ## END loop on abs.simulParams
  names(res) = abs.simulParams
  
  return(res)
}
