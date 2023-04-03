### HEADER #####################################################################
##' @title From betapart package 1.5.4 : beta.pair and betapart.core functions
##' 
##' @name beta.pair
##' @aliases betapart.core
##' 
##' @usage 
##' betapart.core(x)
##' beta.pair(x, index.family = "sorensen") 
##' 
##' @param x data frame, where rows are sites and columns are species. 
##' Alternatively x can be a betapart object derived from the betapart.core 
##' function 
##' @param index.family family of dissimilarity indices, partial match of 
##' "sorensen" or "jaccard".
##' 
##' @keywords betapart, internal
##' 
##' @seealso \code{\link[betapart]{beta.pair}}
##' 
##' @export
##' 
## END OF HEADER ###############################################################

betapart.core = function (x) 
{
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) 
    stop("The data in x is not numeric.", call. = TRUE)
  xvals <- unique(as.vector(x))
  if (any(!is.element(xvals, c(0, 1)))) 
    stop("The table contains values other than 0 and 1: data should be presence/absence.", call. = TRUE)
  shared <- x %*% t(x)
  not.shared <- abs(sweep(shared, 2, diag(shared)))
  sumSi <- sum(diag(shared))
  St <- sum(colSums(x) > 0)
  a <- sumSi - St
  sum.not.shared <- not.shared + t(not.shared)
  max.not.shared <- pmax(not.shared, t(not.shared))
  min.not.shared <- pmin(not.shared, t(not.shared))
  computations <- list(data = x, sumSi = sumSi, St = St, a = a, 
                       shared = shared, not.shared = not.shared, sum.not.shared = sum.not.shared, 
                       max.not.shared = max.not.shared, min.not.shared = min.not.shared)
  class(computations) <- "betapart"
  return(computations)
}



beta.pair = function (x, index.family = "sorensen") 
{
  index.family <- match.arg(index.family, c("jaccard", "sorensen"))
  if (!inherits(x, "betapart")) {
    x <- betapart.core(x)
  }
  switch(index.family, sorensen = {
    beta.sim <- x$min.not.shared/(x$min.not.shared + x$shared)
    beta.sne <- ((x$max.not.shared - x$min.not.shared)/((2 * x$shared) + x$sum.not.shared)) * (x$shared/(x$min.not.shared + x$shared))
    beta.sor <- x$sum.not.shared/(2 * x$shared + x$sum.not.shared)
    pairwise <- list(beta.sim = as.dist(beta.sim), beta.sne = as.dist(beta.sne), beta.sor = as.dist(beta.sor))
  }, jaccard = {
    beta.jtu <- (2 * x$min.not.shared)/((2 * x$min.not.shared) + x$shared)
    beta.jne <- ((x$max.not.shared - x$min.not.shared)/(x$shared + x$sum.not.shared)) * (x$shared/((2 * x$min.not.shared) + x$shared))
    beta.jac <- x$sum.not.shared/(x$shared + x$sum.not.shared)
    pairwise <- list(beta.jtu = as.dist(beta.jtu), beta.jne = as.dist(beta.jne), beta.jac = as.dist(beta.jac))
  })
  return(pairwise)
}
