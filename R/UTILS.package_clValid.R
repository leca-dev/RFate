### HEADER #####################################################################
##' @title From clValid package O.7 : dunn function
##' 
##' @name dunn
##' 
##' @keywords clValid
##' 
##' @seealso \code{\link[clValid]{dunn}}
##' 
##' @export
##' 
## END OF HEADER ###############################################################

## Dunn index : ratio of the smallest distance between observations
## not in the same cluster to the largest intra-cluster distance.
## Value between zero and infinity, and should be maximized.
# mdunn = dunn(mat.species.DIST[[group]], c1) ## PB WITH R 4.0 and matrix class
## EXTRACTED from dunn function from clValid package
  
dunn = function (distance = NULL, clusters) #, Data = NULL, method = "euclidean") 
{
  # if (is.null(distance) & is.null(Data)) 
  #   stop("One of 'distance' or 'Data' is required")
  # if (is.null(distance)) 
  #   distance <- as.matrix(dist(Data, method = method))
  # if ("dist" %in% class(distance)) 
  #   distance <- as.matrix(distance)
  nc <- max(clusters)
  interClust <- matrix(NA, nc, nc)
  intraClust <- rep(NA, nc)
  for (i in 1:nc) {
    c1 <- which(clusters == i)
    for (j in i:nc) {
      if (j == i) 
        intraClust[i] <- max(distance[c1, c1])
      if (j > i) {
        c2 <- which(clusters == j)
        interClust[i, j] <- min(distance[c1, c2])
      }
    }
  }
  dunn <- min(interClust, na.rm = TRUE)/max(intraClust)
  return(dunn)
}
