### HEADER #####################################################################
##' @title From ecospat package 3.2 : ecospat.kd, ecospat.grid.clim.dyn and 
##' ecospat.niche.overlap functions (and sp 1.4-5, adehabitatMA 0.3.14, 
##' adehabitatHR 0.4.19 packages
##' 
##' @name ecospat.niche.overlap
##' @aliases ecospat.grid.clim.dyn
##' @aliases ecospat.kd
##' 
##' @usage
##' ecospat.kd(x, ext, R = 100, th = 0, env.mask = c(), method = "adehabitat")
##' ecospat.grid.clim.dyn(glob, glob1, sp, R = 100, th.sp = 0, th.env = 0, 
##' geomask = NULL, kernel.method = "adehabitat", extend.extent = c(0, 0, 0, 0))
##' ecospat.niche.overlap(z1, z2, cor)
##' 
##' 
##' @param x two-column dataframe (or a vector)
##' @param ext c(xmin, xmax)
##' @param th quantile
##' @param env.mask mask
##' @param method kernel.method
##' 
##' 
##' @param glob A two-column dataframe (or a vector) of the environmental 
##' values (in column) for background pixels of the whole study area (in row).
##' @param glob1 A two-column dataframe (or a vector) of the environmental 
##' values (in column) for the background pixels of the species (in row).
##' @param sp A two-column dataframe (or a vector) of the environmental 
##' values (in column) for the occurrences of the species (in row).
##' @param R The resolution of the grid.
##' @param th.sp The quantile used to delimit a threshold to exclude low 
##' species density values.
##' @param th.env The quantile used to delimit a threshold to exclude low 
##' environmental density values of the study area.
##' @param geomask A geographical mask to delimit the background extent if the 
##' analysis takes place in the geographical space.It can be a SpatialPolygon 
##' or a raster object. Note that the CRS should be the same as the one used 
##' for the points.
##' @param kernel.method Method used to estimate the the kernel density. 
##' Currently, there are two methods: by default, it is the methode from 
##' 'adehabitat'. Method from the library 'ks' is also available.
##' @param extend.extent Vector with extention values of the window size 
##' (see details).
##' 
##' @param z1 Species 1 occurrence density grid created by ecospat.grid.clim.
##' @param z2 Species 2 occurrence density grid created by ecospat.grid.clim.
##' @param cor Correct the occurrence densities of each species by the 
##' prevalence of the environments in their range (TRUE = yes, FALSE = no).
##' 
##' 
##' @keywords ecospat, niche overlap
##' 
##' @seealso \code{\link[ecospat]{ecospat.grid.clim.dyn}},
##' \code{\link[ecospat]{ecospat.niche.overlap}}
##' 
##' @importFrom stats density
##' @importFrom raster coordinates extract mask cellStats as.matrix
##' @importFrom adehabitatMA ascgen
##' @importFrom adehabitatHR kernelUD
##' @importFrom sp SpatialPoints
##' 
##' @export
##' 
## END OF HEADER ###############################################################

ecospat.kd = function (x, ext, R = 100, th = 0, env.mask = c(), method = "adehabitat") 
{
  if (method == "adehabitat") {
    if (ncol(x) == 2) {
      xr <- data.frame(cbind((x[, 1] - ext[1])/abs(ext[2] - ext[1]), (x[, 2] - ext[3])/abs(ext[4] - ext[3])))
      mask <- ascgen(SpatialPoints(cbind((0:(R))/R, (0:(R)/R))), nrcol = R - 2, count = FALSE)
      x.dens <- kernelUD(SpatialPoints(xr[, 1:2]), h = "href", grid = mask, kern = "bivnorm")
      x.dens <- raster(xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4], matrix(x.dens$ud, nrow = R))
      if (!is.null(th)) {
        th.value <- quantile(extract(x.dens, x), th)
        x.dens[x.dens < th.value] <- 0
      }
      if (!is.null(env.mask)) {
        x.dens <- x.dens * env.mask
      }
    } else if (ncol(x) == 1) {
      xr <- seq(from = min(ext), to = max(ext), length.out = R)
      x.dens <- density(x[, 1], kernel = "gaussian", from = min(xr), to = max(xr), n = R, cut = 0)
      if (!is.null(env.mask)) {
        x.dens$y <- x.dens$y * env.mask
      }
      if (!is.null(th)) {
        xr <- sapply(x, findInterval, x.dens$x)
        th.value <- quantile(x.dens$y[xr], th)
        sprm <- which(x.dens$y < th.value)
        x.dens$y[sprm] <- 0
      }
    }
  }
  # if (method == "ks") {
  #   if (ncol(x) == 2) {
  #     x.dens <- ks::kde(x, xmin = ext[c(1, 3)], xmax = ext[c(2, 4)], gridsize = c(R, R))
  #     x.dens <- raster::flip(raster::t(raster::raster(x.dens$estimate)), direction = "y")
  #     raster::extent(x.dens) <- c(xmn = ext[1], xmx = ext[2], ymn = ext[3], ymx = ext[4])
  #     if (!is.null(th)) {
  #       th.value <- quantile(raster::extract(x.dens, x), th)
  #       x.dens[x.dens < th.value] <- 0
  #     }
  #     if (!is.null(env.mask)) {
  #       x.dens <- x.dens * env.mask
  #     }
  #   } else if (ncol(x) == 1) {
  #     x.dens <- ks::kde(x, xmin = min(ext), xmax = max(ext), gridsize = c(R, R))
  #     x.dens$y <- x.dens$estimate
  #     x.dens$x <- x.dens$eval.points
  #     if (!is.null(env.mask)) {
  #       x.dens$y <- x.dens$y * env.mask
  #     }
  #     if (!is.null(th)) {
  #       xr <- sapply(x, findInterval, x.dens$x)
  #       th.value <- quantile(x.dens$y[xr], th)
  #       sprm <- which(x.dens$y < th.value)
  #       x.dens$y[sprm] <- 0
  #     }
  #   }
  # }
  return(x.dens)
}


ecospat.grid.clim.dyn = function (glob, glob1, sp, R = 100, th.sp = 0, th.env = 0, geomask = NULL
                                  , kernel.method = "adehabitat", extend.extent = c(0, 0, 0, 0)) 
{
  if (is.null(kernel.method) | (kernel.method != "ks" & kernel.method != "adehabitat")) {
    stop("supply a kernel method ('adehabitat' or 'ks')")
  }
  glob <- as.matrix(glob)
  glob1 <- as.matrix(glob1)
  sp <- as.matrix(sp)
  l <- list()
  if (ncol(glob) > 2) {
    stop("cannot calculate overlap with more than two axes")
  }
  if (ncol(glob) == 1) {
    xmin <- min(glob[, 1]) + extend.extent[1]
    xmax <- max(glob[, 1]) + extend.extent[2]
    glob1.dens <- ecospat.kd(x = glob1, ext = c(xmin, xmax), method = kernel.method, th = 0)
    sp.dens <- ecospat.kd(x = sp, ext = c(xmin, xmax), method = kernel.method, th = 0, env.mask = glob1.dens$y > 0)
    x <- sp.dens$x
    y <- sp.dens$y
    z <- sp.dens$y * nrow(sp)/sum(sp.dens$y)
    Z <- glob1.dens$y * nrow(glob)/sum(glob1.dens$y)
    z.uncor <- z/max(z)
    z.cor <- z/Z
    z.cor[is.na(z.cor)] <- 0
    z.cor[z.cor == "Inf"] <- 0
    z.cor <- z.cor/max(z.cor)
  }
  if (ncol(glob) == 2) {
    xmin <- apply(glob, 2, min, na.rm = T)
    xmax <- apply(glob, 2, max, na.rm = T)
    ext <- c(xmin[1], xmax[1], xmin[2], xmax[2]) + extend.extent
    glob1.dens <- ecospat.kd(x = glob1, ext = ext, method = kernel.method, th = 0)
    if (!is.null(geomask)) {
      sp::proj4string(geomask) <- NA
      glob1.dens <- mask(glob1.dens, geomask, updatevalue = 0)
    }
    sp.dens <- ecospat.kd(x = sp, ext = ext, method = kernel.method, th = 0, env.mask = glob1.dens > 0)
    x <- seq(from = ext[1], to = ext[2], length.out = 100)
    y <- seq(from = ext[3], to = ext[4], length.out = 100)
    l$y <- y
    Z <- glob1.dens * nrow(glob1)/cellStats(glob1.dens, "sum")
    z <- sp.dens * nrow(sp)/cellStats(sp.dens, "sum")
    z.uncor <- z/cellStats(z, "max")
    z.cor <- z/Z
    z.cor[is.na(z.cor)] <- 0
    z.cor <- z.cor/cellStats(z.cor, "max")
  }
  w <- z.uncor
  w[w > 0] <- 1
  l$x <- x
  l$z <- z
  l$z.uncor <- z.uncor
  l$z.cor <- z.cor
  l$Z <- Z
  l$glob <- glob
  l$glob1 <- glob1
  l$sp <- sp
  l$w <- w
  return(l)
}


ecospat.niche.overlap = function (z1, z2, cor) 
{
  l <- list()
  if (cor == FALSE) {
    p1 <- as.matrix(z1$z.uncor)/sum(as.matrix(z1$z.uncor))
    p2 <- as.matrix(z2$z.uncor)/sum(as.matrix(z2$z.uncor))
  }
  if (cor == TRUE) {
    p1 <- as.matrix(z1$z.cor)/sum(as.matrix(z1$z.cor))
    p2 <- as.matrix(z2$z.cor)/sum(as.matrix(z2$z.cor))
  }
  D <- 1 - (0.5 * (sum(abs(p1 - p2))))
  H <- sqrt(sum((sqrt(p1) - sqrt(p2))^2))
  I <- 1 - (H^2)/2
  l$D <- D
  l$I <- I
  return(l)
}


