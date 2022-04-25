### HEADER #####################################################################
##' @title Computation of niche overlap distances between species 
##' 
##' @name PRE_FATE.speciesDistanceOverlap
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create a distance matrix between 
##' species, based on co-occurrence of species. 
##'              
##' @param mat.overlap.option a \code{string} corresponding to the way to 
##' calculate the distance between species based on niche overlap (either 
##' \code{PCA} or \code{raster}, see 
##' \href{PRE_FATE.speciesDistance#details}{\code{Details}})
##' @param mat.overlap.object two options, depending on the value of 
##' \code{mat.overlap.option} :
##' \itemize{
##'   \item (\code{PCA} option) a \code{list} with 2 elements :
##'   \describe{
##'     \item{\code{tab.dom.PA}}{a \code{matrix} or \code{data.frame} with 
##'     sites in rows and species in columns, containing either \code{NA}, 
##'     \code{0} or \code{1} (see \code{\link{PRE_FATE.selectDominant}})}
##'     \item{\code{tab.env}}{a \code{matrix} or \code{data.frame} with 
##'     sites in rows and environmental variables in columns}
##'   }
##'   \item (\code{raster} option) a \code{data.frame} with 2 columns :
##'   \describe{
##'     \item{\code{species}}{the ID of each studied species}
##'     \item{\code{raster}}{path to raster file with species distribution}
##'   }
##' }
##' 
##' @details 
##' 
##' This function allows to obtain a \strong{distance matrix between species} 
##' (1 - Schoeners D), based on niche overlap information :
##' 
##' \itemize{
##'   \item If \code{PCA} option is selected, the degree of niche overlap will 
##'   be computed using the \code{\link[ecospat]{ecospat.niche.overlap}}. 
##'   \item If \code{raster} option is selected, the degree of niche overlap will 
##'   be computed using the \code{\link[phyloclim]{niche.overlap}}. \cr \cr \cr
##' }
##' 
##' 
##' @return A \code{matrix} containing overlap distances between each pair 
##' of species, calculated as \code{1 - Schoeners D}.
##'  
##' @keywords niche overlap, Schoeners D
##' 
##' @seealso \code{\link[ecospat]{ecospat.niche.overlap}}, 
##' \code{\link[phyloclim]{niche.overlap}}
##' 
##' @examples
##' 
##' ## Load example data
##' Champsaur_PFG = .loadData('Champsaur_PFG', 'RData')
##' 
##' ## Prepare sites x species table
##' ## Add absences in community sites
##' sites = Champsaur_PFG$sp.observations
##' tab.dom.PA = Champsaur_PFG$tab.dom.PA
##' for (si in sites$sites[which(sites$TYPE == "COMMUNITY")])
##' {
##'   ind = which(rownames(tab.dom.PA) == si)
##'   tab.dom.PA[ind, which(is.na(tab.dom.PA[ind, ]))] = 0
##' }
##' 
##' ## Prepare environmental table
##' tab.env = Champsaur_PFG$tab.env
##' 
##' ## Calculate niche overlap distances -----------------------------------------
##' list.over = list(tab.dom.PA[, 1:10], tab.env)
##' DIST.overlap = PRE_FATE.speciesDistanceOverlap(mat.overlap.option = "PCA"
##'                                                , mat.overlap.object = list.over)
##' (DIST.overlap[1:5, 1:5])
##' 
##' @export
##' 
##' @importFrom stats as.dist
##' @importFrom methods as
##' @importFrom ade4 dudi.hillsmith suprow
##' @importFrom raster raster extension
##' @importFrom phyloclim niche.overlap
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesDistanceOverlap = function(mat.overlap.option, mat.overlap.object
){
  
  #############################################################################
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesDistanceOverlap")
  cat("\n #------------------------------------------------------------# \n\n")
  
  
  ## CHECK parameter mat.overlap
  .testParam_notInValues.m("mat.overlap.option", mat.overlap.option, c("PCA", "raster"))
  if (mat.overlap.option == "PCA")
  {
    if(!.testParam_notInClass(mat.overlap.object, "list"))
    {
      mat.overlap = mat.overlap.object
      if (length(mat.overlap) == 2 &&
          !.testParam_notInClass(mat.overlap[[1]], c("matrix", "data.frame"), FALSE) &&
          !.testParam_notInClass(mat.overlap[[2]], c("matrix", "data.frame"), FALSE))
      {
        tab.dom.PA = mat.overlap[[1]]
        tab.env = mat.overlap[[2]]
        ind_rownames = sort(unique(intersect(rownames(tab.dom.PA), rownames(tab.env))))
        tab.dom.PA = tab.dom.PA[ind_rownames, ]
        tab.env = tab.env[ind_rownames, ]
        if (nrow(tab.dom.PA) == 0 || nrow(tab.env) == 0){
          stop(paste0("Wrong type of data!\n `tab.dom.PA` and `tab.env` "
                      , "must have matching rownames. Please check."))
        }
        
        ## Calculate PCA for all environment
        pca.env = dudi.hillsmith(tab.env, scannf = F, nf = 2)
        scores.env = pca.env$li
        
        ## Calculate overlap matrix
        PROGRESS = txtProgressBar(min = 0, max = ncol(tab.dom.PA), style = 3)
        grid.list = foreach(ii = 1:ncol(tab.dom.PA)) %do%
          {
            setTxtProgressBar(pb = PROGRESS, value = ii)
            si.01 = rownames(tab.dom.PA)[which(!is.na(tab.dom.PA[, ii]))]
            si.1 = rownames(tab.dom.PA)[which(tab.dom.PA[, ii] > 0)]
            if (length(si.1) > 5)
            {
              ind.01 = which(rownames(tab.env) %in% si.01)
              ind.1 = which(rownames(tab.env) %in% si.1)
              scores.sp1.01 = suprow(pca.env, tab.env[ind.01, ])$li
              scores.sp1.1 = suprow(pca.env, tab.env[ind.1, ])$li
              grid.clim.sp1 = ecospat.grid.clim.dyn(glob = scores.env
                                                    , glob1 = scores.sp1.01
                                                    , sp = scores.sp1.1
                                                    , R = 100, th.sp = 0)
              return(grid.clim.sp1)
            } else { return(NULL) }
          }
        close(PROGRESS)
        
        n.sel = ncol(tab.dom.PA)
        mat.overlap = matrix(NA, nrow = n.sel, ncol = n.sel
                             , dimnames = list(colnames(tab.dom.PA), colnames(tab.dom.PA)))
        PROGRESS = txtProgressBar(min = 0, max = n.sel, style = 3)
        for (ii in 1:(n.sel-1))
        {
          setTxtProgressBar(pb = PROGRESS, value = ii)          
          if (!is.null(grid.list[[ii]]))
          {
            for(jj in (ii+1):n.sel)
            {
              if (!is.null(grid.list[[jj]]))
              {
                res = ecospat.niche.overlap(grid.list[[ii]], grid.list[[jj]], cor = TRUE)$D
                mat.overlap[ii, jj] = res
              }
            }
          }
        }
        close(PROGRESS)
        
        mat.overlap[lower.tri(mat.overlap, diag = FALSE)] = t(mat.overlap)[lower.tri(mat.overlap, diag = FALSE)]
        diag(mat.overlap) = 1
      } else
      {
        stop(paste0("Wrong type of data!\n `mat.overlap.object` must be a list "
                    , "containing 2 data.frame or matrix elements"))
      }
    } else
    {
      stop(paste0("Wrong type of data!\n `mat.overlap.object` must be a list "
                  , "containing 2 data.frame or matrix elements"))
    }
  } else if (mat.overlap.option == "raster")
  {
    if (is.data.frame(mat.overlap.object))
    {
      mat.overlap = mat.overlap.object
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
        overlap.mat = t(niche.overlap(raster.list))
        overlap.mat = as.matrix(as.dist(overlap.mat))
        diag(overlap.mat) = 1
        rownames(overlap.mat) = colnames(overlap.mat) = mat.overlap$species
        mat.overlap = overlap.mat
      } else
      {
        stop(paste0("Wrong data given!\n `mat.overlap$raster` must contain "
                    , "file names with appropriate extension (`.tif`, `.img`, `.asc`)"))
      }
    } else
    {
      stop(paste0("Wrong type of data!\n `mat.overlap.object` must be a data.frame"))
    }
  }
  
  ## Remove species with no overlap
  no_NA_values = apply(mat.overlap, 2, function(x) sum(is.na(x)))
  ind_NA_values = which(no_NA_values >= nrow(mat.overlap) - 1)
  if (length(ind_NA_values) > 0)
  {
    warning(paste0("Missing data!\n `mat.overlap` contains some species with no overlap values : "
                   , paste0(colnames(mat.overlap)[ind_NA_values], collapse = ", ")
                   , "\nThese species will not be taken into account ! \n\n"
    ))
    mat.overlap = mat.overlap[-ind_NA_values, -ind_NA_values]
    # names_species.overlap = sort(unique(colnames(mat.overlap)))
    # if (nrow(mat.overlap) <= 1)
    # {
    #   stop("Wrong dimension(s) of data!\n `mat.overlap` does not have the appropriate number of rows (>=2)")
    # }
  }
  
  ## Transform into dissimilarity distances (instead of similarity)
  mat.OVERLAP = (1 - mat.overlap)
  
  cat("\n> Done!\n")
  
  return(mat.OVERLAP)
}

