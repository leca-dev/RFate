### HEADER #####################################################################
##' @title Computation of traits distances between species
##' 
##' @name PRE_FATE.speciesDistanceTraits
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a distance matrix between 
##' species, based on functional trait values. 
##'              
##' @param mat.traits a \code{data.frame} with at least 3 columns :
##' \describe{
##' \item{\code{species}}{the ID of each studied species}
##' \item{\code{GROUP}}{a factor variable containing grouping information to 
##' divide the species into data subsets (see 
##' \href{PRE_FATE.speciesDistance#details}{\code{Details}})}
##' \item{\code{...}}{one column for each functional trait}
##' }
##' @param opt.maxPercent.NA (\emph{optional}) default \code{0}. \cr Maximum 
##' percentage of missing values (\code{NA}) allowed for each trait (between 
##' \code{0} and \code{1})
##' @param opt.maxPercent.similarSpecies (\emph{optional}) default \code{0.25}. 
##' \cr Maximum percentage of similar species (same value) 
##' allowed for each trait (between \code{0} and \code{1})
##' @param opt.min.sd (\emph{optional}) default \code{0.5}. \cr Minimum 
##' standard deviation allowed for each trait (trait unit)
##' 
##' @details 
##' 
##' This function allows to obtain a \strong{distance matrix between species} 
##' (1 - Schoeners D), based on functional traits information :
##' 
##' \itemize{
##'   \item The \code{GROUP} column is required if species must be separated 
##'   to have one final distance matrix per \code{GROUP} value. \cr If the 
##'   column is missing, all species will be considered as part of a unique 
##'   dataset.
##'   \item The traits can be qualitative or quantitative, but previously 
##'   identified as such \cr (i.e. with the use of functions such as 
##'   \code{as.numeric}, \code{as.factor} and \code{ordered}).
##'   \item Functional distance matrix is calculated with Gower dissimilarity, 
##'   using the \code{\link[FD]{gowdis}} function.
##'   \item This function allows \code{NA} values. \cr However, too many 
##'   missing values lead to misleading results. Hence, 3 parameters allow the 
##'   user to play with the place given to missing values, and therefore the 
##'   selection of traits that will be used for the distance computation :
##'   \describe{
##'     \item{opt.maxPercent.NA}{traits with too many missing values are 
##'     removed}
##'     \item{opt.maxPercent \cr .similarSpecies}{traits with too many 
##'     similar values are removed}
##'     \item{opt.min.sd}{traits with too little variability are removed}
##'   }
##' }
##' 
##' 
##' @return A \code{matrix} containing functional distances between each pair 
##' of species, calculated as \code{1 - Schoeners D}.
##' 
##' @keywords functional traits, Gower distance
##' 
##' @seealso \code{\link[FD]{gowdis}}
##' 
##' @examples
##' 
##' ## Load example data
##' Champsaur_PFG = .loadData('Champsaur_PFG', 'RData')
##' 
##' ## Species traits
##' tab.traits = Champsaur_PFG$sp.traits
##' tab.traits = tab.traits[, c('species', 'GROUP', 'MATURITY', 'LONGEVITY'
##'                             , 'HEIGHT', 'DISPERSAL', 'LIGHT', 'NITROGEN')]
##' str(tab.traits)
##' 
##' ## Give warnings -------------------------------------------------------------
##' DIST.traits = PRE_FATE.speciesDistanceTraits(mat.traits = tab.traits)
##' str(DIST.traits)
##' 
##' ## Change parameters to allow more NAs (and change traits used) --------------
##' DIST.traits = PRE_FATE.speciesDistanceTraits(mat.traits = tab.traits
##'                                              , opt.maxPercent.NA = 0.05
##'                                              , opt.maxPercent.similarSpecies = 0.3
##'                                              , opt.min.sd = 0.3)
##' str(DIST.traits)
##' 
##' \dontrun{
##' require(foreach); require(ggplot2); require(ggdendro)
##' pp = foreach(x = names(DIST.traits)) %do%
##'   {
##'     hc = hclust(as.dist(DIST.traits[[x]]))
##'     pp = ggdendrogram(hc, rotate = TRUE) +
##'       labs(title = paste0('Hierarchical clustering based on species distance '
##'                           , ifelse(length(names(DIST.traits)) > 1
##'                                    , paste0('(group ', x, ')')
##'                                    , '')))
##'     return(pp)
##'   }
##' plot(pp[[1]])
##' plot(pp[[2]])
##' plot(pp[[3]])
##' }
##' 
##' 
##' @export
##' 
##' @importFrom stats as.dist na.exclude var
##' @importFrom FD gowdis
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesDistanceTraits = function(mat.traits
                                          , opt.maxPercent.NA = 0
                                          , opt.maxPercent.similarSpecies = 0.25
                                          , opt.min.sd = 0.3
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
                  , "or columns (>=3, at least 2 traits)"))
    } else if (sum(colnames(mat.traits) == "species") == 0)
    {
      .stopMessage_columnNames("mat.traits", c("species", "(GROUP)", "(trait1)", "(trait2)", "..."))
    } else if (sum(colnames(mat.traits) == "GROUP") == 0)
    {
      warning(paste0("`mat.traits` does not contain any column with `GROUP` information\n"
                     , "Data will be considered as one unique dataset."))
      mat.traits$GROUP = "AllSpecies"
    }
    mat.traits$species = as.character(mat.traits$species)
    mat.traits$GROUP = as.character(mat.traits$GROUP)
    .testParam_samevalues.m("mat.traits$species", mat.traits$species)
    .testParam_notChar.m("mat.traits$GROUP", mat.traits$GROUP)
  }
  ## CHECK parameter opt
  .testParam_notBetween.m("opt.maxPercent.NA", opt.maxPercent.NA, 0, 1)
  .testParam_notBetween.m("opt.maxPercent.similarSpecies", opt.maxPercent.similarSpecies, 0, 1)
  .testParam_notBetween.m("opt.min.sd", opt.min.sd, 0, 1)
  
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesDistanceTraits")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ### PREPARATION OF DATA
  #############################################################################
  
  cat("\n ---------- INFORMATION : AVAILABLE \n")
  
  ## TRAITS -------------------------------------------------------------------
  mat.traits = as.data.frame(mat.traits)
  rownames(mat.traits) = mat.traits$species
  names_species.traits = sort(unique(mat.traits$species))
  names_traits = colnames(mat.traits)[which(!(colnames(mat.traits) %in% c("species","GROUP")))]
  names_groups = sort(unique(mat.traits$GROUP))
  
  cat("\n> FOR TRAITS ")
  cat("\n  Number of species : ", length(names_species.traits))
  cat("\n  Groups : ", paste0(names_groups, collapse = ", "))
  cat("\n  Measured traits : ", paste0(names_traits, collapse = ", "))
  cat("\n")
  
  ## Remove species with no traits
  no_NA_values = apply(as.matrix(mat.traits[, names_traits, drop = FALSE])
                       , 1, function(x) sum(is.na(x)))
  ind_NA_values = which(no_NA_values >= length(names_traits) - 1)
  if (length(ind_NA_values) > 0)
  {
    warning(paste0("Missing data!\n `mat.traits` contains some species with no trait values : "
                   , paste0(mat.traits$species[ind_NA_values], collapse = ", ")
                   , "\nThese species will not be taken into account ! \n\n"
    ))
    mat.traits = mat.traits[-ind_NA_values, , drop = FALSE]
    names_species.traits = sort(unique(mat.traits$species))
    names_groups = sort(unique(mat.traits$GROUP))
    if (nrow(mat.traits) <= 1)
    {
      stop("Wrong dimension(s) of data!\n `mat.traits` does not have the appropriate number of rows (>=2)")
    }
  }
  
  ## Remove groups with only one species
  no_sp_group = table(mat.traits$GROUP)
  ind_1_sp = names(no_sp_group)[which(no_sp_group == 1)]
  ind_1_sp = which(mat.traits$GROUP == ind_1_sp)
  if (length(ind_1_sp) > 0)
  {
    warning(paste0("Missing data!\n `mat.traits` contains some groups with only one species : "
                   , paste0(mat.traits$GROUP[ind_1_sp], collapse = ", ")
                   , "\nThese species and groups will not be taken into account ! \n\n"
    ))
    mat.traits = mat.traits[-ind_1_sp, , drop = FALSE]
    names_species.traits = sort(unique(mat.traits$species))
    names_groups = sort(unique(mat.traits$GROUP))
    if (nrow(mat.traits) <= 1)
    {
      stop("Wrong dimension(s) of data!\n `mat.traits` does not have the appropriate number of rows (>=2)")
    }
  }
  
  ## SPLIT INFORMATION by species type
  species.split = split(mat.traits$species, f = mat.traits$GROUP)
  
  
  #############################################################################
  ### CALCULATE TRAITS DISTANCES
  #############################################################################
  
  ## Check for percentage of NA -----------------------------------------------
  tab = mat.traits[, names_traits, drop = FALSE]
  tab = split(tab, mat.traits$GROUP)
  tab_eval.1 = sapply(tab, function(x) {
    apply(x, 2, function(y) sum(is.na(y)))
  })
  tab_eval.1 = as.data.frame(tab_eval.1)
  tab_eval.1$trait = rownames(tab_eval.1)
  tab_eval.1 = melt(tab_eval.1, id.vars = "trait")
  colnames(tab_eval.1) = c("TRAIT", "GROUP", "number.NA")
  tab_eval.1$number.NA = tab_eval.1$number.NA / nrow(mat.traits)
  
  ## Apply Gower distance for each trait and calculate : ----------------------
  ##  - percentage of 0 (= similar species)
  ##  - standard deviation (variability of distances)
  tab_eval.2 = foreach(tr = names_traits, .combine = "rbind") %do%
    {
      mat.traits.split = split(mat.traits[, tr, drop = FALSE], f = mat.traits$GROUP)
      mat.species.gower.split = lapply(mat.traits.split, gowdis)
      res = foreach(x = names(mat.species.gower.split), .combine = "rbind") %do%
        {
          mat = as.matrix(mat.species.gower.split[[x]])
          if (nrow(mat) > 1)
          {
            mat[upper.tri(mat, diag = TRUE)] = NA
            mat = as.vector(mat)
            std.dev = sqrt(var(na.exclude(mat)))
            percent.0 = ifelse(length(which(!is.na(mat))) > 0
                               , length(which(mat == 0)) / length(which(!is.na(mat)))
                               , NA)
            return(data.frame(GROUP = x, TRAIT = tr, std.dev, percent.0, stringsAsFactors = FALSE))
          } else
          {
            return(data.frame(GROUP = x, TRAIT = tr, std.dev = 0, percent.0 = 0, stringsAsFactors = FALSE))
          }
        }
      return(res)
    }
  
  ## CHOOSE which traits to keep by species type ------------------------------
  traits_toKeep = merge(tab_eval.1, tab_eval.2, by = c("GROUP", "TRAIT"))
  traits_toKeep$toKeep1 = (traits_toKeep$number.NA <= opt.maxPercent.NA)
  traits_toKeep$toKeep2 = (traits_toKeep$percent.0 < opt.maxPercent.similarSpecies)
  traits_toKeep$toKeep3 = (traits_toKeep$std.dev > opt.min.sd)
  traits_toKeep$toKeep = ifelse(traits_toKeep$toKeep1 == FALSE
                                , FALSE
                                , ifelse(traits_toKeep$toKeep2 == TRUE
                                         , TRUE
                                         , ifelse(traits_toKeep$toKeep3 == TRUE, TRUE, FALSE
                                         )))
  
  if (length(which(traits_toKeep$toKeep == FALSE)) == nrow(traits_toKeep))
  {
    eval.message = sapply(unique(traits_toKeep$GROUP), function(gr) {
      tab = traits_toKeep[which(traits_toKeep$GROUP == gr), ]
      paste0("In group "
             , gr
             , " : \n"
             , paste0(" >> "
                      , substr(tab$TRAIT, 1, 10)
                      , "\t\t\t"
                      , round(tab$number.NA, 4) * 100
                      , "\t"
                      , round(tab$percent.0, 4) * 100
                      , "\t"
                      , round(tab$std.de, 4) * 100
                      , collapse = "\n")
             , "\n")
    })
    eval.message = paste0(eval.message, collapse = "")
    stop(paste0("Missing data!\n `mat.traits` contains traits with too many "
                , "missing values or not enough variation between species. \n"
                , "Please check. \n\n"
                , "Columns below represent for each trait :\n"
                , " - the percentage of missing values \n"
                , " - the percentage of similar species \n"
                , " - the standard deviation of pairwise distances \n\n"
                , eval.message))
    
  } else if (length(which(traits_toKeep$toKeep == FALSE)) > 0)
  {
    tab.notKeep = traits_toKeep[which(traits_toKeep$toKeep == FALSE), ]
    eval.message = sapply(unique(tab.notKeep$GROUP), function(gr) {
      tab = tab.notKeep[which(tab.notKeep$GROUP == gr), ]
      paste0("In group "
             , gr
             , " : \n"
             , paste0(" >> "
                      , substr(tab$TRAIT, 1, 10)
                      , "\t\t\t"
                      , round(tab$number.NA, 4) * 100
                      , "\t"
                      , round(tab$percent.0, 4) * 100
                      , "\t"
                      , round(tab$std.de, 4) * 100
                      , collapse = "\n")
             , "\n")
    })
    eval.message = paste0(eval.message, collapse = "")
    warning(paste0("Missing data!\n `mat.traits` contains some traits with too many "
                   , "missing values or not enough variation between species. \n"
                   , "These traits will not be taken into account ! \n\n"
                   , "Columns below represent for each trait :\n"
                   , " - the percentage of missing values \n"
                   , " - the percentage of similar species \n"
                   , " - the standard deviation of pairwise distances \n\n"
                   , eval.message))
  }
  
  ## SPLIT INFORMATION by species type
  cat("\n ---------- INFORMATION : USED \n")
  cat("\n  Traits used to calculate functional distances : \n")
  mat.traits.split = split(mat.traits[, names_traits, drop = FALSE], f = mat.traits$GROUP)
  toRemove = vector()
  for (gp in 1:length(mat.traits.split))
  {
    tmp = traits_toKeep[which(traits_toKeep$GROUP == names(mat.traits.split)[gp]), ]
    tmp = tmp$TRAIT[which(tmp$toKeep == TRUE)]
    if (length(tmp) > 0)
    {
      mat.traits.split[[gp]] = mat.traits.split[[gp]][, tmp, drop = FALSE]
      cat("\n> Group", names(mat.traits.split)[gp], ":", as.character(tmp)) 
    } else
    {
      toRemove = c(toRemove, gp)
    }
  }
  if (length(toRemove) > 0)
  {
    mat.traits.split = mat.traits.split[-toRemove]
  }
  names_groups = names(mat.traits.split)
  cat("\n")
  
  ## GOWER DISSIMILARITY FOR MIXED VARIABLES
  mat.species.gower.split = lapply(mat.traits.split, gowdis)
  
  for (gp in 1:length(mat.species.gower.split))
  {
    if (length(which(is.na(as.matrix(mat.species.gower.split[[gp]])))) > 0)
    {
      ## remove NA values
      mat.species.gower.split[[gp]] = as.matrix(mat.species.gower.split[[gp]])
      nn = apply(mat.species.gower.split[[gp]], 2, function(x) length(which(is.na(x))))
      nn = which(nn == 0)
      mat.species.gower.split[[gp]] = mat.species.gower.split[[gp]][nn, nn]
      mat.species.gower.split[[gp]] = as.dist(mat.species.gower.split[[gp]])
    }
  }
  species.split = lapply(mat.species.gower.split, function(x) colnames(as.matrix(x)))
  
  cat("\n  Number of species : ", length(unlist(species.split)))
  cat("\n  Groups : ", paste0(names_groups, collapse = ", "))
  cat("\n  Number of species in each group : ", sapply(species.split, length))
  cat("\n  Number of NA values due to `gowdis` function : "
      , nrow(mat.traits) - sum(sapply(species.split, length)))
  cat("\n")
  
  
  #############################################################################
  cat("\n> Done!\n")
  
  ## SAVE results
  if(length(names_groups) == 1)
  {
    mat.FUNCTIONAL = as.matrix(mat.species.gower.split[[1]])
  } else
  {
    mat.FUNCTIONAL = lapply(mat.species.gower.split, as.matrix)
  }
  
  return(mat.FUNCTIONAL)
}

