### HEADER #####################################################################
##' @title From SPOT package 2.5.0 : designLHD and designLHDNorm functions
##' 
##' @name designLHD
##' @aliases designLHDNorm
##' 
##' @keywords SPOT, Latin Hypercube Sampling
##' 
##' @seealso \code{\link[SPOT]{designLHD}}
##' 
##' @export
##' 
## END OF HEADER ###############################################################

designLHDNorm = function (dim, size, calcMinDistance = FALSE, nested = NULL, inequalityConstraint = NULL) 
{
  step <- 1/size
  design <- replicate(dim, sample(0:(size - 1), size) * step + runif(size) * step)
  if (!is.null(inequalityConstraint)) {
    feasible <- apply(design, 1, inequalityConstraint) <= 0
    if (any(feasible)) 
      design <- design[feasible, , drop = FALSE]
    else design <- matrix(NA, 0, ncol(design))
    while (nrow(design) < size) {
      newP <- runif(dim)
      if (inequalityConstraint(newP) <= 0) {
        design <- rbind(design, as.numeric(newP))
      }
    }
  }
  des <- rbind(design, nested)
  if (calcMinDistance) 
    minDistance <- min(dist(des))
  else minDistance <- NA
  list(design = design, minDistance = minDistance)
}



designLHD = function (x = NULL, lower, upper, control = list()) 
{
  n <- length(lower)
  con <- list(size = 10, retries = 10, replicates = 1, inequalityConstraint = NULL, types = rep("numeric", n))
  con[names(control)] <- control
  control <- con
  if (!is.null(x)) {
    for (i in 1:length(lower)) {
      lowerBound <- lower[i]
      upperBound <- upper[i]
      x[, i] <- (x[, i] - lowerBound)/(upperBound - lowerBound)
    }
  }
  if (!is.null(control$inequalityConstraint)) {
    ineqConstraint <- control$inequalityConstraint
    force(ineqConstraint)
    force(lower)
    force(upper)
    ineqConstraint01 <- function(xx) {
      xx <- lower + xx * (upper - lower)
      ineqConstraint(xx)
    }
  } else {
    ineqConstraint01 <- NULL
  }
  best <- designLHDNorm(length(lower), control$size, calcMinDistance = control$retries > 1
                        , nested = x, inequalityConstraint = ineqConstraint01)
  if (control$retries > 1) {
    for (i in 1:(control$retries - 1)) {
      tmpDes <- designLHDNorm(length(lower), control$size, calcMinDistance = TRUE
                              , nested = x, inequalityConstraint = ineqConstraint01)
      if (tmpDes$minDistance > best$minDistance) 
        best <- tmpDes
    }
  }
  design <- rbind(x, best$design)
  for (i in 1:n) {
    lowerBound <- lower[i]
    upperBound <- upper[i]
    if (control$types[i] != "numeric") {
      lowerBound <- lowerBound - 0.5
      upperBound <- upperBound + 0.4999999999999
    }
    design[, i] <- lowerBound + design[, i] * (upperBound - lowerBound)
    if (control$types[i] != "numeric") 
      design[, i] <- floor(design[, i] + 0.5)
  }
  if (control$replicates > 1) {
    m <- nrow(design)
    design <- design[rep(1:m, control$replicates), ]
  }
  as.matrix(design)
}

