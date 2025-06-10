### HEADER #####################################################################
##' @title From phyloclim package 0.9.5 : niche.overlap and di.enm functions
##' 
##' @name niche.overlap
##' @aliases di.pno
##' @aliases di.enm
##' 
##' @description This function quantifies the degree of niche overlap using 
##' the statistics D and I (as proposed by Warren et al., 2008) based on 
##' Schoeners D (Schoener, 1968) and Hellinger Distances (van der Vaart, 1998). 
##' Niche overlap can be calculated either from the probability surfaces of 
##' entire ecological niche models (Warren et al., 2008) or from single 
##' dimensions of the climatic niches (Evans et al., 2009).
##' 
##' @usage
##' di.pno(x, y)
##' di.enm(rn, x, y)
##' niche.overlap(x)
##' 
##' @param x Might take one of the follwing forms: (1) a data frame containing 
##' predicted niche occupancy (PNO) profiles, e.g., as derived by 
##' \code{\link[phyloclim]{pno}}; (2) a vector of filenames referencing to 
##' probability surfaces saved in ASCII-format with an ArcGIS-compatible header; 
##' (3) a list containing probability surfaces stored as objects of class 
##' \code{\link[sp]{SpatialGrid}}
##' 
##' @keywords phyloclim, internal
##' 
##' @seealso \code{\link[phyloclim]{niche.overlap}}
##' 
##' @importFrom methods slot
##' @importFrom sp read.asciigrid
##' 
##' @export
##' 
## END OF HEADER ###############################################################

di.pno <- function(x, y)
{
  
  # normalize probability surfaces
  # ------------------------------
  xSUM <- sum(x, na.rm = TRUE)
  x <- x / xSUM
  ySUM <- sum(y, na.rm = TRUE)
  y <- y / ySUM
  
  # Schoeners D (Schoener, 1968; Warren, Glor & Turelli, 2008)
  # ----------------------------------------------------------
  D <- 1 - 0.5 * sum(abs(x - y), na.rm = TRUE)
  
  # Hellingers Distance:
  # ---------------------
  H <- sqrt(sum((sqrt(x) - sqrt(y))^2, na.rm = TRUE))
  # I <- 1 - 0.5 * H -> error in Warren, Glor and Turelli 
  # (2008, Evolution 62:2868-2883)
  I <- 1 - H^2 * 0.5 # <- corrected I
  
  # both statistics range betweeen 0 (no overlap) and 1 (niches are identical)
  
  c(D = D, I = I)	
}

di.enm <- function(rn, x, y)
{
  
  if (!inherits(x, "Spatial")){
    if (!missing(rn)) {
      x <- gsub("_", paste("_", rn, "_", sep = ""), x)
    }
    x <- read.asciigrid(x)
  }
  
  if (!inherits(y, "Spatial")){
    if (!missing(rn)) {
      y <- gsub("_", paste("_", rn, "_", sep = ""), y)
    }
    y <- read.asciigrid(y)
  }
  
  xx <- slot(x, "data")
  yy <- slot(y, "data")
  
  # standardize probability surfaces
  # ------------------------------
  xSUM <- sum(xx, na.rm = TRUE)
  xx <- xx / xSUM
  ySUM <- sum(yy, na.rm = TRUE)
  yy <- yy / ySUM
  
  # Schoeners D (Schoener, 1968; Warren, Glor & Turelli, 2008)
  # ----------------------------------------------------------
  D <- 1 - 0.5 * sum(abs(xx - yy), na.rm = TRUE)
  
  # Hellingers Distance:
  # ---------------------
  H <- sqrt(sum((sqrt(xx) - sqrt(yy))^2, na.rm = TRUE))
  # I <- 1 - 0.5 * H -> error in Warren, Glor and Turelli 
  # (2008, Evolution 62:2868-2883)
  I <- 1 - H^2 * 0.5 # <- corrected I
  
  # both statistics range betweeen 0 (no overlap) and 1 (niches are identical)
  
  c(D = D, I = I)	
}

niche.overlap <- function(x)
{
  
  # CASE 1: x is a pno matrix
  # -------------------------
  if ( is.data.frame(x) ){
    x <- x[, -1]
    nspec <- ncol(x)
    DI <- matrix(nrow = nspec, ncol = nspec)
    rownames(DI) <- colnames(DI) <- names(x)
    for (i in 1:(nspec - 1)){
      for (j in (i + 1):nspec){
        dhi <- di.pno(x = x[, i], y = x[, j])
        DI[i, j] <- dhi["D"]
        DI[j, i] <- dhi["I"]
      }
    }
  }
  
  ## CASE 2: x is vector of filenames
  ## ------------------------------
  if ( class(x) == "character" ){
    nspec <- length(x)
    DI <- matrix(nrow = nspec, ncol = nspec)
    rownames(DI) <- colnames(DI) <- x
    for (i in 1:(nspec - 1)){
      X <- read.asciigrid(x[i])
      for (j in (i + 1):nspec){
        Y <- read.asciigrid(x[j])
        dhi <- di.enm(x = X, y = Y)
        DI[i, j] <- dhi[1]
        DI[j, i] <- dhi[2]
      }
    }
  }
  
  ## CASE 3: x is a list of 'SpatialGrid' objects
  ## --------------------------------------------
  if (inherits(x[[1]], "SpatialGrid")){
    nspec <- length(x)
    DI <- matrix(nrow = nspec, ncol = nspec)
    rownames(DI) <- colnames(DI) <- names(x)
    system.time(
      for (i in 1:(nspec - 1)){
        for (j in (i + 1):nspec){
          dhi <- di.enm(x = x[[i]], y = x[[j]])
          DI[i, j] <- dhi[1]
          DI[j, i] <- dhi[2]
        }
      }
    ) # system.time
  }
  class(DI) <- "niolap"
  DI
}
