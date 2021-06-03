### HEADER #####################################################################
##' @title From SPOT package 2.5.0 : designLHD and designLHDNorm functions
##' 
##' @name designLHD
##' @aliases designLHDNorm
##' 
##' @param x optional matrix x, rows for points, columns for dimensions. This 
##' can contain one or more points which are part of the design, but specified 
##' by the user. These points are added to the design, and are taken into 
##' account when calculating the pair-wise distances. They do not count for 
##' the design size. E.g., if x has two rows, control$replicates is one and 
##' control$size is ten, the returned design will have 12 points (12 rows). 
##' The first two rows will be identical to x. Only the remaining ten rows are 
##' guaranteed to be a valid LHD.
##' @param lower vector with lower boundary of the design variables (in case 
##' of categorical parameters, please map the respective factor to a set of 
##' contiguous integers, e.g., with lower = 1 and upper = number of levels)
##' @param upper vector with upper boundary of the design variables (in case 
##' of categorical parameters, please map the respective factor to a set of 
##' contiguous integers, e.g., with lower = 1 and upper = number of levels)
##' @param control list of controls: see \code{\link[SPOT]{designLHD}}
##' 
##' @param dim number, dimension of the problem (will be no. of columns of 
##' the result matrix)
##' @param size number of points with that dimension needed. (will be no. 
##' of rows of the result matrix).
##' @param calcMinDistance Boolean to indicate whether a minimal distance 
##' should be calculated.
##' @param nested nested design to be considered during distance calculation.
##' @param inequalityConstraint inequality constraint function, smaller zero 
##' for infeasible points. Used to replace infeasible points with random 
##' points. Has to evaluate points in interval [0;1].
##' 
##' @keywords SPOT, Latin Hypercube Sampling
##' 
##' @seealso \code{\link[SPOT]{designLHD}}
##' 
##' @importFrom stats runif dist
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

