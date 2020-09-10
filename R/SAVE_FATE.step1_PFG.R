### HEADER #####################################################################
##' @title Save data to reproduce building of Plant Functional Groups
##'
##' @name SAVE_FATE.step1_PFG
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to gather all data and parameters 
##' used to build a set of Plant Functional Groups.
##' 
##' @param name.dataset a \code{string} corresponding to the name to give to 
##' archive folder
##' @param mat.observations a \code{data.frame} with at least 3 columns : \cr
##' \code{sites}, \code{species}, \code{abund} (\emph{and optionally, 
##' \code{habitat}}) (see \code{\link{PRE_FATE.selectDominant}})
##' @param rules.selectDominant (\emph{optional}) default \code{NA}. \cr A 
##' \code{vector} containing all the parameter values given to the 
##' \code{\link{PRE_FATE.selectDominant}} function, if used 
##' (\code{doRuleA}, \code{rule.A1}, \code{rule.A2_quantile}, \code{doRuleB}, 
##' \code{rule.B1_percentage}, \code{rule.B1_number}, \code{rule.B2}, 
##' \code{doRuleC}).
##' @param mat.traits a \code{data.frame} with at least 3 columns :
##' \code{species}, \code{GROUP}, \code{...} (one column for each functional 
##' trait) \cr (see \code{\link{PRE_FATE.speciesDistance}})
##' @param mat.overlap (\emph{optional}) default \code{NA}. \cr 
##' Otherwise, two options :
##' \itemize{
##'   \item a \code{data.frame} with 2 columns : \code{species}, \code{raster}
##'   \item a dissimilarity structure representing the niche overlap between 
##'   each pair of species. \cr It can be a \code{dist} object, a 
##'   \code{niolap} object, or simply a \code{matrix}.
##' }
##' (see \code{\link{PRE_FATE.speciesDistance}})
##' @param rules.speciesDistance (\emph{optional}) default \code{NA}. \cr A 
##' \code{vector} containing all the parameter values given to the 
##' \code{\link{PRE_FATE.speciesDistance}} function, if used \cr (
##' \code{opt.maxPercent.NA}, \code{opt.maxPercent.similarSpecies}, 
##' \code{opt.min.sd}).
##' @param mat.species.DIST a \code{dist} object, or a \code{list} of 
##' \code{dist} objects (one for each \code{GROUP} value), corresponding to the 
##' distance between each pair of species. \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesDistance}} function.
##' @param clust.evaluation (\emph{optional}) default \code{NA}. \cr A 
##' \code{data.frame} with 4 columns : \cr
##' \code{GROUP}, \code{no.clusters}, \code{variable}, \code{value}. \cr Such an 
##' object can be obtained with the 
##' \code{\link{PRE_FATE.speciesClustering_step1}} function.
##' @param no.clusters an \code{integer}, or a \code{vector} of \code{integer} 
##' (one for each \code{GROUP} value), with the number of clusters to be kept 
##' (see \code{\link{PRE_FATE.speciesClustering_step2}})
##' @param determ.all a \code{data.frame} with 6 or 10 columns : \cr
##' \code{PFG}, \code{GROUP}, \code{ID.cluster}, \code{species}, 
##' \code{ID.species}, \code{DETERMINANT} \cr (\emph{and optionally, 
##' \code{sp.mean.dist}, \code{allSp.mean}, \code{allSp.min}, 
##' \code{allSp.max}}). \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesClustering_step2}} function.
##' @param mat.traits.PFG a \code{data.frame} with at least 3 columns :
##' \code{PFG}, \code{no.species}, \code{...} (one column for each functional 
##' trait, computed as the \code{mean} (for numeric traits) or the \code{median} 
##' (for categorical traits) of the values of the determinant species of this 
##' PFG). \cr Such an object can be obtained with the 
##' \code{\link{PRE_FATE.speciesClustering_step3}} function.
##' 
##' @return A \code{list} containing all the elements given to the function and 
##' checked :
##' 
##' \describe{
##'   \item{name.dataset}{name of the dataset}
##'   \item{mat.observations}{(see \code{\link{PRE_FATE.selectDominant}}) \cr
##'   \describe{
##'     \item{\code{sites}}{name of sampling site}
##'     \item{\code{(x, y)}}{coordinates of sampling site}
##'     \item{\code{species}}{name of the concerned species}
##'     \item{\code{abund}}{abundance of the concerned species}
##'     \item{\code{(habitat)}}{habitat of sampling site \cr \cr}
##'   }
##'   }
##'   \item{rules.selectDominant}{a \code{vector} containing values for the 
##'   parameters \code{doRuleA}, \code{rule.A1}, \code{rule.A2_quantile}, 
##'   \code{doRuleB}, \code{rule.B1_percentage}, \code{rule.B1_number}, 
##'   \code{rule.B2}, \code{doRuleC} (see \code{\link{PRE_FATE.selectDominant}})}
##'   \item{mat.traits}{(see \code{\link{PRE_FATE.speciesDistance}}) \cr
##'   \describe{
##'     \item{\code{species}}{name of the concerned species}
##'     \item{\code{GROUP}}{name of the concerned data subset}
##'     \item{\code{...}}{one column for each functional trait \cr \cr}
##'   }
##'   }
##'   \item{mat.overlap}{a \code{dist} object corresponding to the distance 
##'   between each pair of species in terms of niche overlap (see 
##'   \code{\link{PRE_FATE.speciesDistance}})}
##'   \item{rules.speciesDistance}{a \code{vector} containing values for the 
##'   parameters \code{opt.maxPercent.NA}, \code{opt.maxPercent.similarSpecies}, 
##'   \code{opt.min.sd} (see \code{\link{PRE_FATE.speciesDistance}})}
##'   \item{mat.species.DIST}{a \code{dist} object corresponding to the distance 
##'   between each pair of species, or a \code{list} of \code{dist} objects, one 
##'   for each \code{GROUP} value (see \code{\link{PRE_FATE.speciesDistance}})}
##'   \item{clust.evaluation}{(see \code{\link{PRE_FATE.speciesClustering_step1}}) \cr
##'   \describe{
##'     \item{\code{GROUP}}{name of data subset}
##'     \item{\code{no.clusters}}{number of clusters used for the clustering}
##'     \item{\code{variable}}{evaluation metrics' name}
##'     \item{\code{value}}{value of evaluation metric \cr \cr}
##'   }
##'   }
##'   \item{no.clusters}{number of clusters to be kept for each data subset}
##'   \item{determ.all}{(see \code{\link{PRE_FATE.speciesClustering_step2}}) \cr
##'   \describe{
##'     \item{\code{PFG}}{ID of the plant functional group 
##'     (\code{GROUP} + \code{ID.cluster})}
##'     \item{\code{GROUP}}{name of data subset}
##'     \item{\code{ID.cluster}}{cluster number}
##'     \item{\code{species}}{name of species}
##'     \item{\code{ID.species}}{species number in each PFG}
##'     \item{\code{DETERMINANT}}{\code{TRUE} if determinant species, \code{FALSE} 
##'     otherwise}
##'     \item{\code{(sp.mean.dist)}}{species mean distance to other species of 
##'     the same PFG}
##'     \item{\code{(allSp.mean)}}{\eqn{mean(\text{sp.mean.dist})} within the PFG}
##'     \item{\code{(allSp.min)}}{\eqn{mean(\text{sp.mean.dist}) - 1.64 * 
##'     sd(\text{sp.mean.dist})} within the PFG}
##'     \item{\code{(allSp.max)}}{\eqn{mean(\text{sp.mean.dist}) + 1.64 * 
##'     sd(\text{sp.mean.dist})} within the PFG}
##'   }
##'   }
##'   \item{mat.traits.PFG}{(see \code{\link{PRE_FATE.speciesClustering_step3}}) \cr
##'   \describe{
##'     \item{\code{PFG}}{name of the concerned functional group}
##'     \item{\code{no.species}}{number of species in the concerned PFG}
##'     \item{\code{...}}{one column for each functional trait \cr \cr}
##'   }
##'   }
##' }
##' 
##' The information is written in \file{FATE_dataset_[name.dataset]_step1_PFG.RData} file.
##' 
##' 
##' @seealso \code{\link{PRE_FATE.abundBraunBlanquet}}, 
##' \code{\link{PRE_FATE.selectDominant}}, 
##' \code{\link{PRE_FATE.speciesDistance}}, 
##' \code{\link{PRE_FATE.speciesClustering_step1}}, 
##' \code{\link{PRE_FATE.speciesClustering_step2}}, 
##' \code{\link{PRE_FATE.speciesClustering_step3}}
##' 
##' @examples
##' 
##' ## Load example data
##' 
##' @export
##' 
## END OF HEADER ###############################################################


SAVE_FATE.step1_PFG = function(name.dataset
                               , mat.observations
                               , rules.selectDominant = c("doRuleA" = NA
                                                          , "rule.A1" = NA
                                                          , "rule.A2_quantile" = NA
                                                          , "doRuleB" = NA
                                                          , "rule.B1_percentage" = NA
                                                          , "rule.B1_number" = NA
                                                          , "rule.B2" = NA
                                                          , "doRuleC" = NA)
                               , mat.traits
                               , mat.overlap = NA
                               , rules.speciesDistance = c("opt.maxPercent.NA" = NA
                                                           , "opt.maxPercent.similarSpecies" = NA
                                                           , "opt.min.sd" = NA)
                               , mat.species.DIST
                               , clust.evaluation = NA
                               , no.clusters
                               , determ.all
                               , mat.traits.PFG
){
  
  #############################################################################
  
  .testParam_notChar.m("name.dataset", name.dataset)
  ## CHECK parameter mat.observations
  if (.testParam_notDf(mat.observations))
  {
    .stopMessage_beDataframe("mat.observations")
  } else
  {
    mat.observations = as.data.frame(mat.observations)
    if (nrow(mat.observations) == 0 || !(ncol(mat.observations) %in% c(3, 4, 5, 6)))
    {
      .stopMessage_numRowCol("mat.observations", c("sites", "(x)", "(y)", "species", "abund", "(habitat)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.observations))
                          , "3" = .testParam_notColnames(mat.observations, c("sites", "species", "abund"))
                          , "4" = .testParam_notColnames(mat.observations, c("sites", "species", "abund", "habitat"))
                          , "5" = .testParam_notColnames(mat.observations, c("sites", "x", "y", "species", "abund"))
                          , "6" = .testParam_notColnames(mat.observations, c("sites", "x", "y", "species", "abund", "habitat"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.observations", c("sites", "(x)", "(y)", "species", "abund", "(habitat)"))
      }
    }
    mat.observations$sites = as.character(mat.observations$sites)
    if (sum(colnames(mat.observations) == "x") == 1)
    {
      .testParam_notNum.m("mat.observations$x", mat.observations$x)
    }
    if (sum(colnames(mat.observations) == "y") == 1)
    {
      .testParam_notNum.m("mat.observations$y", mat.observations$y)
    }
    mat.observations$species = as.character(mat.observations$species)
    .testParam_notNum.m("mat.observations$abund", mat.observations$abund)
    if (sum(colnames(mat.observations) == "habitat") == 0)
    {
      mat.observations$habitat = "all"
    }
    mat.observations$habitat = as.character(mat.observations$habitat)
  }
  ## CHECK parameter rules.selectDominant
  names.rules.selectDominant = c("doRuleA", "rule.A1", "rule.A2_quantile", "doRuleB", "rule.B1_percentage"
                                 , "rule.B1_number", "rule.B2", "doRuleC")
  .testParam_notInValues.m(names(rules.selectDominant), names.rules.selectDominant)
  if (length(rules.selectDominant) != 8)
  {
    for (name.i in names.rules.selectDominant)
    {
      if (!(name.i %in% names(rules.selectDominant)))
      {
        rules.selectDominant[name.i] = NA
      }
    }
  }
  rules.selectDominant = rules.selectDominant[names.rules.selectDominant]
  if (length(which(is.na(rules.selectDominant))) < 8)
  {
    .testParam_notInValues.m("rules.selectDominant['doRuleA']", rules.selectDominant['doRuleA'], c(0, 1))
    .testParam_notInValues.m("rules.selectDominant['doRuleB']", rules.selectDominant['doRuleB'], c(0, 1))
    .testParam_notInValues.m("rules.selectDominant['doRuleC']", rules.selectDominant['doRuleC'], c(0, 1))
    ## CHECK parameter doRuleA / doRuleC
    if (rules.selectDominant['doRuleA'] || rules.selectDominant['doRuleC'])
    {
      .testParam_notNum.m("rules.selectDominant['rule.A1']", rules.selectDominant['rule.A1'])
      .testParam_notBetween.m("rules.selectDominant['rule.A2_quantile']", rules.selectDominant['rule.A2_quantile'], 0, 1)
    }
    ## CHECK parameter doRuleB
    if (rules.selectDominant['doRuleB'])
    {
      .testParam_notNum.m("rules.selectDominant['rule.B1_number']", rules.selectDominant['rule.B1_number'])
      .testParam_notBetween.m("rules.selectDominant['rule.B1_percentage']", rules.selectDominant['rule.B1_percentage'], 0, 1)
      .testParam_notBetween.m("rules.selectDominant['rule.B2']", rules.selectDominant['rule.B2'], 0, 1)
    }
  }
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
  }
  ## CHECK parameter mat.overlap
  if(missing(mat.overlap) || is.null(mat.overlap))
  {
    stop(paste0("Wrong type of data!\n `mat.overlap` must be either "
                , "a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"))
  } else
  {
    if (!.testParam_notInClass(mat.overlap, c("dist", "niolap")))
    {
      mat.overlap = as.matrix(mat.overlap)
    } else if (is.matrix(mat.overlap))
    {
      if (ncol(mat.overlap) != nrow(mat.overlap))
      {
        stop(paste0("Wrong dimension(s) of data!\n `mat.overlap` does not have the same number of rows ("
                    ,nrow(mat.overlap)
                    ,") and columns ("
                    ,ncol(mat.overlap)
                    ,")"))
      }
    } else if (is.data.frame(mat.overlap))
    {
      if (nrow(mat.overlap) < 2 || ncol(mat.overlap) != 2 )
      {
        stop(paste0("Wrong dimension(s) of data!\n `mat.overlap` does not have the "
                    , "appropriate number of rows (>=2, at least 2 species) "
                    , "or columns (species, raster)"))
      } else if (.testParam_notColnames(mat.overlap, c("species", "raster")))
      {
        .stopMessage_columnNames("mat.overlap", c("species", "raster"))
      }
      mat.overlap$species = as.character(mat.overlap$species)
      mat.overlap$raster = as.character(mat.overlap$raster)
      .testParam_samevalues.m("mat.overlap$species", mat.overlap$species)
      if (sum(file.exists(mat.overlap$raster)) < nrow(mat.overlap))
      {
        stop("Wrong data given!\n `mat.overlap$raster` must contain file names which exist")
      }
      if (sum(extension(mat.overlap$raster) %in% c(".tif", ".img", ".asc")) == nrow(mat.overlap))
      {
        raster.list = lapply(mat.overlap$raster, function(x) as(raster(x), "SpatialGridDataFrame"))
        overlap.mat = as.matrix(niche.overlap(raster.list))
        rownames(overlap.mat) = colnames(overlap.mat) = mat.overlap$species
        mat.overlap = overlap.mat
      } else
      {
        stop(paste0("Wrong data given!\n `mat.overlap$raster` must contain "
                    , "file names with appropriate extension (`.tif`, `.img`, `.asc`)"))
      }
    } else
    {
      stop(paste0("Wrong type of data!\n `mat.overlap` must be either "
                  , "a data.frame or a dissimilarity object (`dist`, `niolap`, `matrix`)"))
    }
  }
  ## CHECK parameter rules.speciesDistance
  names.rules.speciesDistance = c("opt.maxPercent.NA", "opt.maxPercent.similarSpecies", "opt.min.sd")
  .testParam_notInValues.m(names(rules.speciesDistance), names.rules.speciesDistance)
  if (length(rules.speciesDistance) != 3)
  {
    for (name.i in names.rules.speciesDistance)
    {
      if (!(name.i %in% names(rules.speciesDistance)))
      {
        rules.speciesDistance[name.i] = NA
      }
    }
  }
  rules.speciesDistance = rules.speciesDistance[names.rules.speciesDistance]
  if (length(which(is.na(rules.speciesDistance))) < 3)
  {
    .testParam_notBetween.m("rules.speciesDistance['opt.maxPercent.NA']"
                            , rules.speciesDistance['opt.maxPercent.NA'], 0, 1)
    .testParam_notBetween.m("rules.speciesDistance['opt.maxPercent.similarSpecies']"
                            , rules.speciesDistance['opt.maxPercent.similarSpecies'], 0, 1)
    .testParam_notBetween.m("rules.speciesDistance['opt.min.sd']", rules.speciesDistance['opt.min.sd'], 0, 1)
  }
  ## Check existence of parameters
  if (missing(mat.species.DIST) || is.null(mat.species.DIST))
  {
    stop("No data given!\n (missing `mat.species.DIST` information)")
  }
  ## CHECK parameter mat.species.DIST
  if(class(mat.species.DIST) == "list") ##is.list(mat.species.DIST))
  {
    if (length(mat.species.DIST) > 0)
    {
      for (i in 1:length(mat.species.DIST))
      {
        if (class(mat.species.DIST[[i]]) %in% c("dist", "niolap"))
        {
          mat.species.DIST[[i]] = as.matrix(mat.species.DIST[[i]])
        } else if (class(mat.species.DIST[[i]]) == "matrix") #is.matrix(mat.species.DIST[[i]]))
        {
          if (ncol(mat.species.DIST[[i]]) != nrow(mat.species.DIST[[i]]))
          {
            stop(paste0("Wrong dimension(s) of data!\n `mat.species.DIST[[",
                        i,
                        "]]` does not have the same number of rows (",
                        nrow(mat.species.DIST[[i]]),
                        ") and columns (",
                        ncol(mat.species.DIST[[i]]),
                        ")"
            ))
          }
        } else {
          stop(paste0("Wrong type of data!\n `mat.species.DIST[["
                      , i
                      , "]]` must be a dissimilarity object "
                      , "(`dist`, `niolap`, `matrix`)"))
        }
      }
    } else
    {
      stop("Wrong dimension(s) of data!\n `mat.species.DIST` must be of length > 0")
    }
    if(!is.null(names(mat.species.DIST)))
    {
      group_names = names(mat.species.DIST)
    } else {
      group_names = paste0("GROUP", 1:length(mat.species.DIST))
    }
  } else
  {
    if (class(mat.species.DIST) %in% c("dist", "niolap"))
    {
      mat.species.DIST = as.matrix(mat.species.DIST)
    } else if (is.matrix(mat.species.DIST))
    {
      if (ncol(mat.species.DIST) != nrow(mat.species.DIST))
      {
        stop(paste0("Wrong dimension(s) of data!\n `mat.species.DIST` "
                    , "does not have the same number of rows (",
                    nrow(mat.species.DIST),
                    ") and columns (",
                    ncol(mat.species.DIST),
                    ")"
        ))
      }
    } else
    {
      stop(paste0("Wrong type of data!\n `mat.species.DIST` must be a "
                  , "dissimilarity object (`dist`, `niolap`, `matrix`) "
                  , "or a list of dissimilarity objects"))
    }
    mat.species.DIST = list(mat.species.DIST)
    group_names = paste0("GROUP", 1:length(mat.species.DIST))
  }
  ## CHECK parameter clust.evaluation 	
  if (.testParam_notDf(clust.evaluation))
  {
    .stopMessage_beDataframe("clust.evaluation")
  } else
  {
    clust.evaluation = as.data.frame(clust.evaluation)
    if (nrow(clust.evaluation) == 0 || ncol(clust.evaluation) != 4)
    {
      .stopMessage_numRowCol("clust.evaluation", c("GROUP", "no.clusters", "variable", "value"))
    } else
    {
      if (.testParam_notColnames(clust.evaluation, c("GROUP", "no.clusters", "variable", "value")))
      {
        .stopMessage_columnNames("clust.evaluation", c("GROUP", "no.clusters", "variable", "value"))
      }
    }
  }
  ## CHECK parameter no.clusters
  if (.testParam_notNum(no.clusters))
  {
    stop("No data given!\n (missing `no.clusters` information)")
  } else
  {
    if (class(mat.species.DIST) == "list" && length(no.clusters) != length(mat.species.DIST))
    {
      stop("Wrong type of data!\n `no.clusters` must have the same length than `mat.species.DIST`")
    } else if (length(no.clusters) != 1)
    {
      stop("Wrong type of data!\n `no.clusters` must have the same length than `mat.species.DIST`")
    }
  }
  ## CHECK parameter determ.all 	
  if (.testParam_notDf(determ.all))
  {
    .stopMessage_beDataframe("determ.all")
  } else
  {
    determ.all = as.data.frame(determ.all)
    if (nrow(determ.all) == 0 || !(ncol(determ.all) %in% c(6, 10)))
    {
      .stopMessage_numRowCol("determ.all", c("PFG", "GROUP", "ID.cluster", "species", "ID.species", "DETERMINANT"
                                             , "(sp.mean.dist)", "(allSp.mean)", "(allSp.min)", "(allSp.max)"))
    } else
    {
      notCorrect = switch(as.character(ncol(determ.all))
                          , "6" = .testParam_notColnames(determ.all, c("PFG", "GROUP", "ID.cluster", "species", "ID.species", "DETERMINANT"))
                          , "10" = .testParam_notColnames(determ.all, c("PFG", "GROUP", "ID.cluster", "species", "ID.species", "DETERMINANT"
                                                                        , "sp.mean.dist", "allSp.mean", "allSp.min", "allSp.max"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("determ.all", c("PFG", "GROUP", "ID.cluster", "species", "ID.species", "DETERMINANT"
                                                 , "(sp.mean.dist)", "(allSp.mean)", "(allSp.min)", "(allSp.max)"))
      }
    }
  }
  ## CHECK parameter mat.traits.PFG
  if (.testParam_notDf(mat.traits.PFG))
  {
    .stopMessage_beDataframe("mat.traits.PFG")
  } else
  {
    if (nrow(mat.traits.PFG) < 2 || ncol(mat.traits.PFG) <= 2 )
    {
      stop(paste0("Wrong dimension(s) of data!\n `mat.traits.PFG` does not have the "
                  , "appropriate number of rows (>=2, at least 2 species) "
                  , "or columns (>=3, at least 1 trait)"))
    } else if (sum(colnames(mat.traits.PFG) == "PFG") == 0 ||
               sum(colnames(mat.traits.PFG) == "no.species") == 0)
    {
      .stopMessage_columnNames("mat.traits.PFG", c("PFG", "no.species", "(trait1)", "(trait2)", "..."))
    }
    mat.traits.PFG$PFG = as.character(mat.traits.PFG$PFG)
    .testParam_samevalues.m("mat.traits.PFG$PFG", mat.traits.PFG$PFG)
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # SAVE_FATE.step1_PFG")
  cat("\n #------------------------------------------------------------# \n")
  
  # {
  #   cat("\n ---------- INFORMATION : SAMPLING \n")
  #   cat("\n  Number of releves : ", MO.no_releves)
  #   cat("\n  Number of sites : ", MO.no_sites)
  #   cat("\n  Number of species : ", MO.no_species)
  #   cat("\n")
  # }
  
  #############################################################################
  
  results = list(name.dataset
                 , mat.observations
                 , rules.selectDominant
                 , mat.traits
                 , mat.overlap
                 , rules.speciesDistance
                 , mat.species.DIST
                 , clust.evaluation
                 , no.clusters
                 , determ.all
                 , mat.traits.PFG)
  
  name.dataset = paste0("FATE_dataset_", name.dataset, "_step1_PFG")
  assign(name.dataset, results)
  save(name.dataset, file = paste0(name.dataset, ".RData"))
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  return(results)
}

