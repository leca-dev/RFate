### HEADER #####################################################################
##' @title Calculate PFG traits values based on determinant species traits 
##' values
##' 
##' @name PRE_FATE.speciesClustering_step3
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to calculate PFG traits values based 
##' on determinant species traits values. Either the mean or the 
##' median is used depending on the trait class (i.e. numeric or 
##' categorical).
##'              
##' @param mat.traits a \code{data.frame} with at least 3 columns :
##' \describe{
##'   \item{\code{species}}{the ID of each determinant species (see 
##'   \code{\link{PRE_FATE.speciesClustering_step2}})}
##'   \item{\code{PFG}}{the corresponding Plant Functional Group (see 
##'   \code{\link{PRE_FATE.speciesClustering_step2}})}
##'   \item{\code{...}}{one column for each functional trait (see 
##'   \href{PRE_FATE.speciesClustering_step3#details}{\code{Details}})}
##' }
##' 
##' @details
##' 
##' This function allows to \strong{obtain '\emph{average}' functional trait 
##' values for each Plant Functional Group}, based on values at the determinant 
##' species level. \cr \cr
##' 
##' 
##' \emph{A graphic is automatically produced for each functional 
##' trait given, with boxplot representing the values of determinant species, 
##' and colored points the values calculated for each PFG. \cr
##' However, some traits can have 'specific' representation, as long as their 
##' names within \code{mat.traits} match one of the configuration detailed 
##' below :}
##' \describe{
##'   \item{\code{maturity}, \code{longevity}}{to visualize the difference 
##'   between these two values, for the maturity time has an impact on the 
##'   fecundity of the PFG within \code{FATE} (see 
##'   \href{../articles/fate_tutorial_3_MODULES.html#core-module-succession}{CORE module})
##'   \cr \emph{If there is NO values for longevity within one PFG, and some 
##'   maturity values are available, some values might be inferred as 
##'   \eqn{\text{maturity} * 2}. If there is NO values for maturity within 
##'   one PFG, and some longevity values are available, some values might be 
##'   inferred as \eqn{\text{longevity} / 2}.}
##'   }
##'   \item{\code{height}, \code{light}}{to visualize the PFG light 
##'   preference, and help decide and understand the choice of the height 
##'   limits of strata in \code{FATE} (see 
##'   \href{../articles/fate_tutorial_3_MODULES.html#light-module-competition}{LIGHT 
##'   competition module})}
##'   \item{\code{soil_contrib}, \cr \code{soil_tol_min}, \cr 
##'   \code{soil_tol_max}}{to visualize the PFG soil preference, and help 
##'   parameterize the global parameters of the soil competition module 
##'   within \code{FATE} (see 
##'   \href{../articles/fate_tutorial_3_MODULES.html#soil-module-competition}{SOIL 
##'   competition module})}
##'   \item{\cr \code{soil_contrib}, \cr \code{soil_tolerance}}{same as the 
##'   previous one, but \code{soil_tol_min} and \code{soil_tol_max} values 
##'   are obtained by adding or removing \code{soil_tolerance} to 
##'   \code{soil_contrib}}
##' }
##' 
##' 
##' @return A \code{list} containing one \code{data.frame} with the following 
##' columns, and one \code{list} with as many \code{ggplot2} objects as 
##' functional traits given in \code{mat.traits} :
##' 
##' \describe{
##'   \item{tab}{ \cr
##'   \describe{
##'     \item{\code{PFG}}{the concerned plant functional group}
##'     \item{\code{no.species}}{the number of species contained in this PFG}
##'     \item{\code{...}}{one column for each functional trait, computed as the 
##'     \code{mean} (for numeric traits) or the \code{median} (for categorical 
##'     traits) of the values of the determinant species of this PFG}
##'   }
##'   }
##'   \item{plot}{\cr
##'   \describe{
##'     \item{\code{...}}{one for each functional trait, 'specific' cases 
##'     excepted (see 
##'     \href{PRE_FATE.speciesClustering_step3#details}{\code{Details}})}
##'   }
##'   }
##' }
##'     
##' The information is written in \file{PRE_FATE_PFG_TABLE_traits.csv} and 
##' \file{PRE_FATE_CLUSTERING_STEP_3_PFGtraitsValues.pdf} files. \cr
##' This \code{.csv} file can be used to build parameter files to run a 
##' \code{FATE} simulation (e.g. \code{\link{PRE_FATE.params_PFGsuccession}}).
##' 
##' @keywords functional group, traits
##' 
##' @seealso \code{\link{PRE_FATE.speciesClustering_step1}},
##' \code{\link{PRE_FATE.speciesClustering_step2}}
##' 
##' @examples
##' 
##' ## Load example data
##' data(DATASET_Bauges_PFG)
##' 
##' ## Species traits
##' tab.traits = DATASET_Bauges_PFG$dom.traits
##' str(tab.traits)
##' 
##' ## Determinant species
##' sp.DETERM = DATASET_Bauges_PFG$dom.determ
##' str(sp.DETERM)
##' 
##' ## Merge traits and PFG informations
##' mat.traits = merge(sp.DETERM[, c("species", "PFG")]
##'                    , tab.traits
##'                    , by = "species", all.x = TRUE)
##' 
##' 
##' ## Compute traits per PFG : no specific graphic ----------------------------------------------
##' sp.PFG = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
##' names(sp.PFG)
##' str(sp.PFG$tab)
##' names(sp.PFG$plot)
##' plot(sp.PFG$plot$DISPERSAL)
##' plot(sp.PFG$plot$LIGHT)
##' plot(sp.PFG$plot$MOISTURE)
##' 
##' 
##' ## Compute traits per PFG : with one specific graphic ----------------------------------------
##' colnames(mat.traits) = c("species", "PFG", "GROUP", "DISPERSAL"
##'                          , "light", "NITROGEN", "MOISTURE", "height")
##' sp.PFG = PRE_FATE.speciesClustering_step3(mat.traits = mat.traits)
##' names(sp.PFG)
##' str(sp.PFG$tab)
##' names(sp.PFG$plot)
##' plot(sp.PFG$plot$height_light)
##' 
##' 
##' @export
##' 
##' @importFrom utils write.csv
##' @importFrom graphics plot
##' @importFrom stats median
##' 
##' @importFrom foreach foreach %do%
##' @importFrom reshape2 melt
##' 
##' @importFrom ggplot2 ggplot aes_string 
##' geom_boxplot geom_point geom_segment
##' scale_y_continuous scale_y_log10 scale_color_manual scale_fill_manual 
##' facet_wrap labs theme element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##'
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step3 = function(mat.traits
){
  
  #############################################################################
  
  ## CHECK parameter mat.traits
  if (.testParam_notDf(mat.traits))
  {
    .stopMessage_beDataframe("mat.traits")
  } else
  {
    if (nrow(mat.traits) < 2 || ncol(mat.traits) <= 2 )
    {
      stop(paste0("Wrong dimension(s) of data!\n `mat.traits` does not have the "
                  , "appropriate number of rows (>=2, at least 2 species) "
                  , "or columns (>=3, at least 1 trait)"))
    } else if (sum(colnames(mat.traits) == "species") == 0 ||
               sum(colnames(mat.traits) == "PFG") == 0)
    {
      .stopMessage_columnNames("mat.traits", c("species", "PFG", "(trait1)", "(trait2)", "..."))
    } 
    ## Test species values
    mat.traits$species = as.character(mat.traits$species)
    .testParam_samevalues.m("mat.traits$species", mat.traits$species)
    .testParam_notChar.m("mat.traits$species", mat.traits$species)
    ## Test PFG values
    mat.traits$PFG = as.character(mat.traits$PFG)
    .testParam_NAvalues.m("mat.traits$PFG", mat.traits$PFG)
    .testParam_notChar.m("mat.traits$PFG", mat.traits$PFG)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesClustering_step3 : PFG TRAIT VALUES")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  ## Get information about which traits are given
  names_traits = colnames(mat.traits)[which(!(colnames(mat.traits) %in% c("species","PFG")))]
  names_traits.factor = c("maturity", "longevity", "height", "light", "soil_tolerance"
                          , "soil_tol_min", "soil_contrib", "soil_tol_max")
  for(i.trait in names_traits.factor)
  {
    isPresent = i.trait %in% colnames(mat.traits)
    assign(x = paste0("isThere.", i.trait), value = isPresent)
    if (!isPresent) {
      if (!(i.trait == "soil_tol_min" && "soil_tolerance" %in% names_traits) &&
          !(i.trait == "soil_tol_max" && "soil_tolerance" %in% names_traits)) {
        names_traits.factor = names_traits.factor[-which(names_traits.factor == i.trait)]
      }
    }
  }
  if (length(which(names_traits %in% names_traits.factor)) > 0)
  {
    names_traits.factor = c(names_traits.factor
                            , names_traits[-which(names_traits %in% names_traits.factor)])
  } else
  {
    names_traits.factor = c(names_traits.factor, names_traits)
  }
  
  
  ## Get information about which traits are factor / character
  ind.factor = sapply(names_traits, function(x) {
    is.factor(mat.traits[, x]) || is.character(mat.traits[, x]) })
  ind.factor = names(ind.factor)[which(ind.factor == TRUE)]
  
  
  #############################################################################
  
  if (isThere.longevity && isThere.maturity)
  {
    ## Identify missing longevity values
    tab.longevity = rowSums(table(mat.traits$PFG, mat.traits$longevity))
    tab.longevity.pfg = names(tab.longevity)[which(tab.longevity == 0)]
    if (length(tab.longevity.pfg) > 0)
    {
      ind.pfg.longevity = which(mat.traits$PFG %in% tab.longevity.pfg)
    }
    
    ## Identify missing maturity values
    tab.maturity = rowSums(table(mat.traits$PFG, mat.traits$maturity))
    tab.maturity.pfg = names(tab.maturity)[which(tab.maturity == 0)]
    if (length(tab.maturity.pfg) > 0)
    {
      ind.pfg.maturity = which(mat.traits$PFG %in% tab.maturity.pfg)
    }
    
    ## Set new values for longevity
    if (length(tab.longevity.pfg) > 0 && length(ind.pfg.longevity) > 0)
    {
      mat.traits$longevity[ind.pfg.longevity] = mat.traits$maturity[ind.pfg.longevity] * 2
    }
    ## Set new values for maturity
    if (length(tab.maturity.pfg) > 0 && length(ind.pfg.maturity) > 0)
    {
      mat.traits$maturity[ind.pfg.maturity] = mat.traits$longevity[ind.pfg.maturity] / 2
    }
    ## Set new values for both
    if (length(tab.longevity.pfg) > 0 && length(tab.maturity.pfg) > 0 &&
        length(intersect(ind.pfg.longevity, ind.pfg.maturity)) > 0)
    {
      ind.pfg.longevity.maturity = intersect(ind.pfg.longevity, ind.pfg.maturity)
      mat.traits$longevity[ind.pfg.longevity.maturity] = 5
      mat.traits$maturity[ind.pfg.longevity.maturity] = 2
      warning(paste0("No trait values are available for `longevity` and `maturity` for the PFG "
                     , paste0(mat.traits$PFG[ind.pfg.longevity.maturity], collapse = ", ")
                     , ".\n Default values have been set to 5 (`longevity`) and 2 (`maturity`), but YOU BETTER CHECK THAT !"
      ))
    }
  }
  
  
  if (isThere.soil_contrib && isThere.soil_tolerance)
  {
    ## Calculate soil_tol_min and soil_tol_max values
    mat.traits$soil_tol_min = as.numeric(mat.traits$soil_contrib) - 
      as.numeric(mat.traits$soil_tolerance)
    mat.traits$soil_tol_max = as.numeric(mat.traits$soil_contrib) + 
      as.numeric(mat.traits$soil_tolerance)
    mat.traits = mat.traits[, -which(colnames(mat.traits) == "soil_tolerance")]
    ind.factor = ind.factor[which(ind.factor != "soil_tolerance")]
  }
  
  
  ## CALCULATE MEDIAN TRAIT VALUE PER PFG -------------------------------------
  mat.traits.pfg = split(mat.traits, mat.traits$PFG)
  mat.traits.pfg = foreach(tab = mat.traits.pfg, .combine = "rbind") %do%
  {
    res.pfg = data.frame(PFG = unique(tab$PFG)
                         , no.species = length(unique(tab$species))
                         , stringsAsFactors = FALSE)
    
    tab.val = tab[ ,-which(colnames(tab) %in% c("species", "PFG")), drop = FALSE]
    res.val = foreach(i = 1:ncol(tab.val), .combine = "cbind") %do%
    {
      val = tab.val[, i]
      if (is.factor(val) || is.character(val))
      {
        res = median(as.numeric(as.factor(val)), na.rm = TRUE)
        res = levels(as.factor(val))[res]
      } else
      {
        res = mean(as.numeric(val), na.rm = TRUE)
        if (colnames(tab.val)[i] %in% c("soil_contrib", "soil_tol_min", "soil_tol_max"))
        {
          res = round(res, 2)
        } else
        {
          res = round(res)
        }
      }
      res = data.frame(res, stringsAsFactors = FALSE)
      colnames(res) = colnames(tab.val)[i]
      return(res)
    }
    
    return(data.frame(res.pfg, res.val, stringsAsFactors = FALSE))
  }
  
  write.csv(mat.traits.pfg
            , file = "PRE_FATE_PFG_TABLE_traits.csv"
            , row.names = FALSE)
  
  message(paste0("\n The parameter file PRE_FATE_PFG_TABLE_traits.csv "
                 , "has been successfully created !\n"))
  
  
  #############################################################################
  
  cat("\n ---------- PRODUCING PLOT(S) \n")
  
  mat.traits.melt = mat.traits
  if (length(ind.factor) > 0){
    for (i in ind.factor) mat.traits.melt[, i] = as.numeric(as.factor(mat.traits.melt[, i]))
  }
  mat.traits.melt = melt(mat.traits.melt, id.vars = c("species", "PFG"))
  mat.traits.melt$variable = as.character(mat.traits.melt$variable)
  mat.traits.melt$variable = factor(mat.traits.melt$variable, names_traits.factor)
  
  mat.traits.pfg.melt = mat.traits.pfg
  if (length(ind.factor) > 0){
    for (i in ind.factor) {
      tmp = as.numeric(as.character(mat.traits.pfg.melt[, i]))
      mat.traits.pfg.melt[, i] = as.numeric(factor(tmp, levels(mat.traits[,i])))
    }
  }
  mat.traits.pfg.melt = melt(mat.traits.pfg.melt, id.vars = c("no.species", "PFG"))
  mat.traits.pfg.melt$variable = as.character(mat.traits.pfg.melt$variable)
  mat.traits.pfg.melt$variable = factor(mat.traits.pfg.melt$variable, names_traits.factor)
  
  #############################################################################
  
  pp.i = function(i.trait, i.title, i.color
                  , i.segment = NULL, i.facet = FALSE)
  {
    ind.keep.sp = which(mat.traits.melt$variable %in% i.trait)
    ind.keep.pfg = which(mat.traits.pfg.melt$variable %in% i.trait)

        
    pp.i = ggplot(mat.traits.melt[ind.keep.sp,]
                  , aes_string(x = "PFG", y = "value", fill = "variable")) +
      geom_boxplot(color = "grey60", na.rm = TRUE)
    
    if (!is.null(i.segment)) {
      pp.i = pp.i +
        geom_segment(data = mat.traits.pfg
                     , aes_string(x = "PFG"
                                  , xend = "PFG"
                                  , y = i.segment[1]
                                  , yend = i.segment[2])
                     , color = "#525252"
                     , lwd = 1
                     , inherit.aes = FALSE)
    }
    
    if (i.facet) {
      pp.i = pp.i + facet_wrap(~ variable, ncol = 1, scales = "free_y")
    }
    
    pp.i = pp.i +
      geom_point(data = mat.traits.pfg.melt[ind.keep.pfg, ]
                 , aes_string(x = "PFG"
                              , y = "value"
                              , color = "variable")
                 , size = 2
                 , inherit.aes = FALSE) +
      scale_color_manual("", values = i.color) +
      scale_fill_manual(guide = FALSE, values = rep("#FFFFFF", length(i.trait))) +
      labs(x = "", y = ""
           , title = paste0("STEP D : Computation of PFG traits values : ", i.title)
           , subtitle = paste0("PFG traits values are calculated as the average of "
                               , "the PFG determinant species traits values.\n"
                               , "If the trait is factorial or categorical, median value is taken.\n"
                               , "Light-grey boxplot represent determinant species values.\n"
                               , "Colored points represent the PFG calculated values.\n\n")) +
      .getGraphics_theme() +
      theme(axis.ticks.x = element_blank()
            , axis.text.x = element_text(angle = 90))
    
    return(pp.i)
  }
  
  pp_list = list()
  
  #############################################################################
  if (isThere.longevity && isThere.maturity)
  {
    cat("\n> Longevity and maturity...")
    pp_list$maturity_longevity = pp.i(i.trait = c("longevity", "maturity")
                                      , i.title = "longevity & maturity"
                                      , i.color = c("longevity" = "#377eb8", "maturity" = "#ff7f00")
                                      , i.segment = c("longevity", "maturity"))
    pp_list$maturity_longevity = suppressMessages(pp_list$maturity_longevity +
                                                    scale_y_log10())
    plot(pp_list$maturity_longevity)
    
    indSuppr = which(names_traits.factor %in% c("maturity", "longevity"))
    names_traits.factor = names_traits.factor[-indSuppr]
  }
  
  #############################################################################
  if (isThere.height && isThere.light)
  {
    cat("\n> Height and light...")
    pp_list$height_light = pp.i(i.trait = c("height", "light")
                                , i.title = "height & light"
                                , i.color = c("height" = "#238b45", "light" = "#1d91c0")
                                , i.facet = TRUE)
    pp_list$height_light = suppressMessages(pp_list$height_light + scale_y_log10())
    pp_list$height_light = pp_list$height_light + theme(strip.text = element_blank())
    plot(pp_list$height_light)
    
    indSuppr = which(names_traits.factor %in% c("height", "light"))
    names_traits.factor = names_traits.factor[-indSuppr]
  }
  
  #############################################################################
  if ((isThere.soil_contrib && isThere.soil_tolerance) ||
      (isThere.soil_contrib && isThere.soil_tol_min && isThere.soil_tol_max))
  {
    cat("\n> Soil contribution and tolerance...")
    pp_list$soil = pp.i(i.trait = c("soil_tol_min", "soil_contrib", "soil_tol_max")
                        , i.title = "soil contribution & tolerance"
                        , i.color = c("soil_tol_min" = "#ec7014"
                                      , "soil_contrib" = "#b15928"
                                      , "soil_tol_max" = "#cb181d")
                        , i.segment = c("soil_tol_min", "soil_tol_max"))
    plot(pp_list$soil)
    
    indSuppr = which(names_traits.factor %in% c("soil_tol_min", "soil_contrib"
                                                , "soil_tol_max", "soil_tolerance"))
    names_traits.factor = names_traits.factor[-indSuppr]
    
  }
  
  #############################################################################
  if (length(names_traits.factor) > 0)
  {
    for(i.trait in names_traits.factor)
    {
      cat(paste0("\n> ", i.trait, "..."))
      pp_list[[i.trait]] = pp.i(i.trait = i.trait
                                , i.title = i.trait
                                , i.color = "#02818a")
      plot(pp_list[[i.trait]])
      
    }
  }
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  if (length(pp_list) > 0)
  {
    pdf(file = "PRE_FATE_CLUSTERING_STEP_3_PFGtraitsValues.pdf"
        , width = 10, height = 8)
    for (pp in pp_list) if (!is.null(pp)) plot(pp)
    dev.off()
  }
  
  return(list(tab = mat.traits.pfg
              , plot = pp_list))
}

