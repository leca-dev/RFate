### HEADER #####################################################################
##' @title Combine several dissimilarity distance matrices
##' 
##' @name PRE_FATE.speciesDistanceCombine
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a distance matrix between 
##' species, combining several dissimilarity distance matrices. 
##'              
##' @param list.mat.dist a \code{list} of matrices containing dissimilarity 
##' distance values between each pair of species.
##' @param opt.min.noMat (\emph{optional}) default \code{length(list.mat.dist)}. \cr 
##' An \code{integer} corresponding to the minimal number of distance matrices for 
##' which each species should have values
##' @param opt.normal (\emph{optional}) default \code{TRUE}. \cr 
##' If \code{TRUE}, all given distance matrices will be normalized 
##' (see \href{PRE_FATE.speciesDistanceCombine#details}{\code{Details}})
##' @param opt.weights (\emph{optional}) default \code{NULL}. \cr 
##' A \code{vector} of \code{double} (between \code{0} and \code{1}) 
##' corresponding to the weights for each distance matrix provided in 
##' \code{list.mat.dist}. They must sum up to \code{1}.
##' 
##' @details 
##' 
##' This function allows to obtain a \strong{distance matrix between species}, 
##' based on several dissimilarity distance matrices \strong{combined} 
##' according to the following formula :
##' 
##' \deqn{\text{mat.DIST} = \Sigma (\text{wei.i} * \text{mat.DIST}_{i})}
##' 
##' If \code{opt.normal = TRUE}, two \emph{normalization} steps are applied 
##' to each distance matrix before combining them :
##' 
##' \enumerate{
##'   \item a \strong{non-paranormal (npn)} transformation 
##'   (\code{\link[huge]{huge.npn}} function) to obtain Gaussian distributions 
##'   for all dissimilarity matrices used
##'   \item  a \strong{range} normalization to bring the values back between 
##'   \code{0} and \code{1} :
##'   
##'   \deqn{\text{mat.DIST}_{i} = \frac{\text{mat.DIST}_{i} - 
##'   min(\text{mat.DIST}_{i})}{max(\text{mat.DIST}_{i}) - 
##'   min(\text{mat.DIST}_{i})}}
##' }
##' 
##' 
##' @return A \code{matrix} containing the weighted (or not) combination of 
##' the different transformed (or not) distance matrices given. \cr \cr
##' 
##' The information for the combination of all distances is written in 
##' \file{PRE_FATE_DOMINANT_speciesDistance.csv} file.
##'
##' @keywords functional distance
##' 
##' @seealso \code{\link[huge]{huge.npn}}
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
##' ## Species niche overlap (dissimilarity distances)
##' DIST.overlap = Champsaur_PFG$mat.overlap
##' DIST.overlap[1:5, 1:5]
##' 
##' ## Species functional distances (dissimilarity)
##' DIST.traits = PRE_FATE.speciesDistanceTraits(mat.traits = tab.traits
##'                                              , opt.maxPercent.NA = 0.05
##'                                              , opt.maxPercent.similarSpecies = 0.3
##'                                              , opt.min.sd = 0.3)
##' DIST.traits$Chamaephyte[1:5, 1:5]
##' 
##' ## Combine distances ---------------------------------------------------------
##' list.DIST = list(DIST.overlap, DIST.traits$Chamaephyte)
##' sp.DIST.n = PRE_FATE.speciesDistanceCombine(list.mat.dist = list.DIST
##'                                             , opt.weights = c(0.2, 0.8))
##' sp.DIST.un = PRE_FATE.speciesDistanceCombine(list.mat.dist = list.DIST
##'                                              , opt.norm = FALSE
##'                                              , opt.weights = c(0.2, 0.8))
##' str(sp.DIST.n)
##' 
##' 
##' 
##' \dontrun{
##' require(corrplot)
##' list.DIST = list(DIST.overlap, DIST.traits$Chamaephyte
##'                  , sp.DIST.un, sp.DIST.n)
##' names(list.DIST) = c('overlap', 'traits', 'un-normed', 'normed')
##' 
##' par(mfrow = c(2, 2))
##' for (li in 1:length(list.DIST))
##' {
##'   tmp = list.DIST[[li]]
##'   tmp = tmp[colnames(sp.DIST.n), colnames(sp.DIST.n)]
##'   corrplot(tmp, method = 'shade'
##'            , type = 'lower', cl.lim = c(0, 1)
##'            , is.corr = FALSE, title = names(list.DIST)[li])
##' }
##' 
##' require(foreach); require(ggplot2); require(ggdendro)
##' hc = hclust(as.dist(sp.DIST.n))
##' pp = ggdendrogram(hc, rotate = TRUE) +
##'   labs(title = 'Hierarchical clustering based on species distances')
##' plot(pp)
##' }
##' 
##' 
##' @export
##' 
##' @importFrom utils combn
##' @importFrom huge huge.npn
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesDistanceCombine = function(list.mat.dist
                                           , opt.min.noMat = length(list.mat.dist)
                                           , opt.normal = TRUE
                                           , opt.weights = NULL
){
  
  #############################################################################
  
  ## Check existence of parameters
  if (missing(list.mat.dist) || is.null(list.mat.dist))
  {
    stop("No data given!\n (missing `list.mat.dist` information)")
  }
  ## CHECK parameter list.mat.dist
  if(!.testParam_notInClass(list.mat.dist, "list"))
  {
    if (length(list.mat.dist) > 0)
    {
      for (i in 1:length(list.mat.dist))
      {
        if (!.testParam_notInClass(list.mat.dist[[i]], c("dist", "niolap", "matrix"), FALSE))
        {
          list.mat.dist[[i]] = as.matrix(list.mat.dist[[i]])
          if (ncol(list.mat.dist[[i]]) != nrow(list.mat.dist[[i]]))
          {
            stop(paste0("Wrong dimension(s) of data!\n `list.mat.dist[[",
                        i,
                        "]]` does not have the same number of rows (",
                        nrow(list.mat.dist[[i]]),
                        ") and columns (",
                        ncol(list.mat.dist[[i]]),
                        ")"
            ))
          }
        } else {
          stop(paste0("Wrong type of data!\n `list.mat.dist[["
                      , i
                      , "]]` must be a dissimilarity object "
                      , "(`dist`, `niolap`, `matrix`)"))
        }
      }
    } else
    {
      stop("Wrong dimension(s) of data!\n `list.mat.dist` must be of length > 0")
    }
    if(!is.null(names(list.mat.dist)))
    {
      names_mat = names(list.mat.dist)
    } else {
      names_mat = paste0("DIST", 1:length(list.mat.dist))
    }
  } else
  {
    stop(paste0("Wrong type of data!\n `list.mat.dist` must be a list of "
                , "dissimilarity objects (`dist`, `niolap`, `matrix`)"))
  }
  ## CHECK parameter opt
  if (!.testParam_notDef(opt.weights))
  {
    if (length(opt.weights) != length(list.mat.dist))
    {
      stop(paste0("Wrong type of data!\n `opt.weights` must contain "
                  , length(list.mat.dist), " values summing up to 1"))
    }
    .testParam_notBetween.m("opt.weights", opt.weights, 0, 1)
    if (sum(opt.weights) != 1)
    {
      stop(paste0("Wrong type of data!\n `opt.weights` must contain "
                  , length(list.mat.dist), " values summing up to 1"))
    }
  } else
  {
    opt.weights = rep(1, length(list.mat.dist))
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesDistanceCombine")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ### PREPARATION OF DATA
  #############################################################################
  
  {
    cat("\n ---------- INFORMATION : AVAILABLE \n")
    
    no_mat = length(list.mat.dist)
    no_species = sapply(list.mat.dist, nrow)
    names_species = lapply(list.mat.dist, rownames)
    
    ## Check for correspondence :
    cat("\n  Number of distance matrices : ", no_mat)
    cat("\n  Number of species for each distance matrix : ", no_species)
    
    cat("\n  Comparison of matrix' dimensions : \n")
    combi = combn(1:no_mat, 2)
    
    inti_all = table(unlist(names_species))
    inti_all = names(inti_all)[which(inti_all >= opt.min.noMat)]
    cat("\n  Number of species with values for at least", opt.min.noMat, " matrices : ", length(inti_all))
    cat("\n")
    # inti_all = unique(unlist(names_species))
    # for(x in 1:ncol(combi)){
    #   inti = intersect(names_species[[combi[1, x]]], names_species[[combi[2, x]]])
    #   inti_all = intersect(inti_all, inti)
    #   cat("\n> Matrices ", names_mat[combi[1, x]], "-", names_mat[combi[2, x]], ": "
    #       , length(inti), "species in common")
    # }
    # cat("\n  Number of species with values for all matrices : ", length(inti_all))
    # cat("\n")
    
    names_species.toKeep = inti_all
  }
  
  # Keep only species present in all distance matrices
  mat.dist = lapply(list.mat.dist, function(x) {
    x[which(rownames(x) %in% names_species.toKeep), which(colnames(x) %in% names_species.toKeep)]
  })
  
  
  #############################################################################
  ### COMBINE ALL DISTANCE MATRICES
  #############################################################################
  
  if (opt.normal)
  {
    mat.dist = lapply(mat.dist, function(mat) {
      x = mat[lower.tri(mat, diag = FALSE)]
      x = matrix(x, ncol = 1)
      ## Non-paranormal transformation
      y = huge.npn(x)
      ## Normalization by maximum
      z = (y - min(y)) / (max(y) - min(y))
      ## Result
      res = mat
      res[lower.tri(res, diag = FALSE)] = z
      return(as.matrix(as.dist(res)))
    })
  }
  
  ## Fill missing species whith NA if there is any 
  mat.dist = lapply(mat.dist, function(x) {
    if (ncol(x) < length(names_species.toKeep)) {
      tmp.col = colnames(x)
      tmp.row = rownames(x)
      miss = setdiff(names_species.toKeep, colnames(x))
      x = cbind(x, matrix(rep(NA, length(miss) * nrow(x)), ncol = length(miss)))
      x = rbind(x, matrix(rep(NA, length(miss) * ncol(x)), nrow = length(miss)))
      colnames(x) = c(tmp.col, miss)
      rownames(x) = c(tmp.row, miss)
    }
    return(x)
  })
  
  ## Multiply each distance matrix by its weight
  mat.species.DIST = lapply(1:no_mat, function(x) {
    tmp = as.matrix(mat.dist[[x]])
    wei = opt.weights[x]
    return(wei * tmp) 
  })
  
  ## Calculate each pairwise weight (not taking into account dist with NA)
  mat.wei = lapply(mat.species.DIST, function(x) ifelse(is.na(x[]), 0, 1))
  mat.wei = lapply(1:length(opt.weights), function(x) opt.weights[x] * mat.wei[[x]])
  mat.wei = Reduce('+', mat.wei)
  
  ## Summing weighted distance matrices
  ## and divide by weights pairwise corrected
  res = Reduce('+', lapply(mat.species.DIST, function(x) replace(x, is.na(x), 0)))
  res = res*NA^!Reduce(`+`, lapply(mat.species.DIST, function(x) !is.na(x)))
  mat.species.DIST = res / mat.wei
  
  
  #############################################################################
  cat("\n> Done!\n")
  
  ## SAVE results
  toSave = as.matrix(mat.species.DIST)
  rownames(toSave) = colnames(toSave)
  write.csv(toSave
            , file = paste0("PRE_FATE_DOMINANT_speciesDistance.csv")
            , row.names = TRUE)
  message(paste0("\n The output file \n"
                 , " > PRE_FATE_DOMINANT_speciesDistance.csv \n"
                 , "has been successfully created !\n"))
  
  return(mat.species.DIST)
}

