### HEADER #####################################################################
##' @title Obtain ellipse coordinates from (PCO) X,Y and a factor value
##' 
##' @name .getELLIPSE
##' @aliases util.ellipse
##' @aliases util.ELLIPSE
##' 
##' @usage 
##' util.ellipse(mx, my, vx, cxy, vy, coeff)
##' util.ELLIPSE(x, y, z)
##' .getELLIPSE(xy, fac)
##' 
##' 
##' @param xy a \code{data.frame} or \code{matrix} with 2 columns corresponding 
##' to individuals coordinates, extracted from example from 
##' \code{\link[ade4]{dudi.pco}} analysis
##' @param fac a \code{vector} containing group labels for individuals (with 
##' \code{length(fac) = nrow(xy)})
##' @param x a \code{vector} corresponding to abscissa coordinates of 
##' individuals (column \code{1} of \code{xy})
##' @param y a \code{vector} corresponding to ordinate coordinates of 
##' individuals (column \code{2} of \code{xy})
##' @param z a \code{data.frame} with one column for each level represented in 
##' \code{fac} and \code{nrow(z) = length(fac) = nrow(xy)}. Values are 
##' corresponding to the relative representation of each level (for level 
##' \code{i} : \eqn{\frac{1}{N_i}})
##' @param mx \eqn{\Sigma x * \frac{z}{\Sigma z}}
##' @param my \eqn{\Sigma y * \frac{z}{\Sigma z}}
##' @param vx \eqn{\Sigma (x - mx) * (x - mx) * \frac{z}{\Sigma z}}
##' @param cxy \eqn{\Sigma (x - mx) * (y - my) * \frac{z}{\Sigma z}}
##' @param vy \eqn{\Sigma (y - my) * (y - my) * \frac{z}{\Sigma z}}
##' @param coeff default \code{1}
##' 
##' @importFrom stats model.matrix
##' 
## END OF HEADER ###############################################################

util.ellipse <- function(mx, my, vx, cxy, vy, coeff) {
  lig <- 100
  epsi <- 1e-10
  x <- 0
  y <- 0
  if (vx < 0) { vx <- 0 }
  if (vy < 0) { vy <- 0 }
  if (vx == 0 && vy == 0) { return(NULL) }
  delta <- (vx - vy) * (vx - vy) + 4 * cxy * cxy
  delta <- sqrt(delta)
  l1 <- (vx + vy + delta)/2
  l2 <- vx + vy - l1
  if (l1 < 0) { l1 <- 0 }
  if (l2 < 0) { l2 <- 0 }
  l1 <- sqrt(l1)
  l2 <- sqrt(l2)
  test <- 0
  if (vx == 0) {
    a0 <- 0
    b0 <- 1
    test <- 1
  }
  if ((vy == 0) && (test == 0)) {
    a0 <- 1
    b0 <- 0
    test <- 1
  }
  if (((abs(cxy)) < epsi) && (test == 0)) {
    if (vx > vy) {
      a0 <- 1
      b0 <- 0
    } else {
      a0 <- 0
      b0 <- 1
    }
    test <- 1
  }
  if (test == 0) {
    a0 <- 1
    b0 <- (l1 * l1 - vx)/cxy
    norm <- sqrt(a0 * a0 + b0 * b0)
    a0 <- a0/norm
    b0 <- b0/norm
  }
  a1 <- 2 * pi/lig
  c11 <- coeff * a0 * l1
  c12 <- (-coeff) * b0 * l2
  c21 <- coeff * b0 * l1
  c22 <- coeff * a0 * l2
  angle <- 0
  for (i in 1:lig) {
    cosinus <- cos(angle)
    sinus <- sin(angle)
    x[i] <- mx + c11 * cosinus + c12 * sinus
    y[i] <- my + c21 * cosinus + c22 * sinus
    angle <- angle + a1
  }
  return(list(x = x, y = y,
              seg1 = c(mx + c11, my + c21, 
                       mx - c11, my - c21),
              seg2 = c(mx + c12, my + c22, 
                       mx - c12, my - c22)))
}


util.ELLIPSE = function(x, y, z){
  z <- z/sum(z)
  m1 <- sum(x * z)
  m2 <- sum(y * z)
  v1 <- sum((x - m1) * (x - m1) * z)
  v2 <- sum((y - m2) * (y - m2) * z)
  cxy <- sum((x - m1) * (y - m2) * z)
  ell <- util.ellipse(m1, m2, v1, cxy, v2, 1)
  return(ell)
}

.getELLIPSE = function(xy, fac) {
  fac = factor(fac)
  dfdistri = as.data.frame(model.matrix( ~ fac - 1))
  dfdistri = t(t(dfdistri) / as.vector(table(fac)))
  
  coox = as.matrix(t(dfdistri)) %*% xy[, 1] # label
  cooy = as.matrix(t(dfdistri)) %*% xy[, 2] # label
  
  pfg = NULL
  DAT = foreach(fac.i = colnames(dfdistri), .combine = "rbind") %do%
  {
    ell = util.ELLIPSE(xy[, 1], xy[, 2], dfdistri[, fac.i])
    if(length(ell$x) > 0){
      dat = data.frame(x = ell$x
                       , y = ell$y
                       , xlabel = as.vector(coox[fac.i, 1])
                       , ylabel = as.vector(cooy[fac.i, 1])
                       , PFG = sub("fac", "", fac.i))
    } else {
      dat = data.frame(x = as.vector(coox[fac.i, 1])
                       , y = as.vector(cooy[fac.i, 1])
                       , xlabel = as.vector(coox[fac.i, 1])
                       , ylabel = as.vector(cooy[fac.i, 1])
                       , PFG = sub("fac", "", fac.i))
    }
    return(dat)
  }
  return(DAT)
}




