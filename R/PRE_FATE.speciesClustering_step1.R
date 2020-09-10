### HEADER #####################################################################
##' @title Create clusters based on dissimilarity matrix
##' 
##' @name PRE_FATE.speciesClustering_step1
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create clusters of species based 
##' on a distance matrix between those species. Several metrics are computed 
##' to evaluate these clusters and a graphic is produced to help the user to 
##' choose the best number of clusters..
##'              
##' @param mat.species.DIST a \code{dist} object, or a \code{list} of 
##' \code{dist} objects (one for each \code{GROUP} value), corresponding to the 
##' dissimilarity distance between each pair of species. \cr Such an object can 
##' be obtained with the \code{\link{PRE_FATE.speciesDistance}} function.
##' 
##' 
##' @details 
##' 
##' This function allows to \strong{obtain dendrograms based on a dissimilarity 
##' distance matrix between species}.
##' 
##' As for the \code{\link{PRE_FATE.speciesDistance}} method, clustering can be 
##' run for data subsets, conditioning that \code{mat.species.DIST} is given as 
##' a \code{list} of \code{dist} objects (instead of a \code{dist} object alone).
##' \cr \cr
##' 
##' The process is as follows :
##' 
##' \describe{
##'   \item{\strong{1. Choice of the \cr optimal \cr clustering method}}{
##'   hierarchical clustering on the dissimilarity matrix is realized with the 
##'   \code{\link[stats]{hclust}}.
##'   \itemize{
##'     \item Several methods are available for the agglomeration : 
##'     \emph{complete}, \emph{ward.D}, \emph{ward.D2}, \emph{single}, 
##'     \emph{average (UPGMA)}, \emph{mcquitty (WPGMA)}, \emph{median (WPGMC)} 
##'     and \emph{centroid (UPGMC)}.
##'     \item \emph{Mouchet et al. (2008)} proposed a similarity measure between 
##'     the input distance and the one obtained with the clustering which must 
##'     be minimized to help finding the best clustering method :
##'     \deqn{ 1 - cor( \text{mat.species.DIST}, \text{clustering.DIST} ) ^ 2}
##'   }
##'   \strong{For each agglomeration method, this measure is calculated. The 
##'   method that minimizes it is kept and used for further analyses (see 
##'   \file{.pdf} output file). \cr \cr}
##'   }
##'   
##'   \item{\strong{2. Evaluation of the \cr clustering}}{once the hierarchical 
##'   clustering is done, the number of clusters to keep should be chosen. \cr 
##'   To do that, several metrics are computed :
##'   \itemize{
##'     \item{\emph{Dunn index (\code{mdunn}) : }}{ratio of the smallest 
##'     distance between observations not in the same cluster to the largest 
##'     intra-cluster distance. Value between \code{0} and \eqn{\infty}, and 
##'     should be maximized.}
##'     \item{\emph{Meila's Variation of Information index (\code{mVI}) : }}
##'     {measures the amount of information lost and gained in changing 
##'     between two clusterings. Should be minimized.}
##'     \item{\emph{Coefficient of determination (\code{R2}) : }}{value 
##'     between \code{0} and \code{1}. Should be maximized.}
##'     \item{\emph{Calinski and Harabasz index (\code{ch}) : }}{the higher 
##'     the value, the "better" is the solution.}
##'     \item{\emph{Corrected rand index (\code{Rand}) : }}{measures the 
##'     similarity between two data clusterings. Value between \code{0} and 
##'     \code{1}, with \code{0} indicating that the two data clusters do not 
##'     agree on any pair of points and \code{1} indicating that the data 
##'     clusters are exactly the same.}
##'     \item{\emph{Average silhouette width (\code{av.sil}) : }}{Observations 
##'     with a large \code{s(i)} (almost \code{1}) are very well clustered, a 
##'     small \code{s(i)} (around \code{0}) means that the observation lies 
##'     between two clusters, and observations with a negative \code{s(i)} are 
##'     probably placed in the wrong cluster. Should be maximized.}
##'   }
##'   \strong{A graphic is produced, giving the values of these metrics in 
##'   function of the number of clusters used. Number of clusters with 
##'   evaluation metrics' values among the 3 best are highlighted to help the 
##'   user to make his/her optimal choice (see \file{.pdf} output file).}
##'   }
##' }
##' 
##' \emph{\cr \cr
##' Mouchet M., Guilhaumon f., Villeger S., Mason N.W.H., Tomasini J.A. & 
##' Mouillot D., 2008. Towards a consensus for calculating dendrogam-based 
##' functional diversity indices. Oikos, 117, 794-800.}
##' 
##' @return A \code{list} containing one \code{list}, one \code{data.frame} with 
##' the following columns, and two \code{ggplot2} objects :
##' 
##' \describe{
##'   \item{clust.dendrograms}{a \code{list} with as many objects of 
##'   class \code{\link[stats]{hclust}} as data subsets}
##'   \item{clust.evaluation}{ \cr
##'   \describe{
##'     \item{\code{GROUP}}{name of data subset}
##'     \item{\code{no.clusters}}{number of clusters used for the clustering}
##'     \item{\code{variable}}{evaluation metrics' name}
##'     \item{\code{value}}{value of evaluation metric \cr \cr}
##'   }
##'   }
##'   \item{plot.clustMethod}{\code{ggplot2} object, representing the different 
##'   values of metrics to choose the clustering method}
##'   \item{plot.clustNo}{\code{ggplot2} object, representing the different 
##'   values of metrics to choose the number of clusters \cr \cr}
##' }
##' 
##' One \file{PRE_FATE_CLUSTERING_STEP1_numberOfClusters.pdf} file is created 
##' containing two types of graphics : 
##' \describe{
##'   \item{clusteringMethod}{to account for the chosen clustering method}
##'   \item{numberOfClusters}{for decision support, to help the user to choose 
##'   the adequate number of clusters to be given to the 
##'   \code{\link{PRE_FATE.speciesClustering_step2}} function}
##' }
##' 
##' 
##' @note \strong{The function does not return ONE dendrogram} (or as many as 
##' given dissimilarity structures) \strong{but a LIST with all tested numbers 
##' of clusters.} One final dendrogram can then be obtained using this result 
##' as a parameter in the \code{\link{PRE_FATE.speciesClustering_step2}} 
##' function.
##' 
##' @keywords hierarchical clustering, Dunn index, Meila's Variation of 
##' Information index, R2, Calinski and Harabasz index, Corrected rand index, 
##' Average silhouette width
##' 
##' @seealso \code{\link[stats]{hclust}},
##' \code{\link[stats]{cutree}},
##' \code{\link[fpc]{cluster.stats}},
##' \code{\link[clValid]{dunn}},
##' \code{\link{PRE_FATE.speciesDistance}},
##' \code{\link{PRE_FATE.speciesClustering_step2}}
##' 
##' @examples
##' 
##' ## Load example data
##' data(DATASET_Bauges_PFG)
##' 
##' ## Species dissimilarity distance (niche overlap + traits distance)
##' tab.dist = DATASET_Bauges_PFG$dom.dist_total
##' str(tab.dist)
##' as.matrix(tab.dist[[1]])[1:5, 1:5]
##' 
##' ## Build dendrograms -------------------------------------------------------------------------
##' sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = tab.dist)
##' names(sp.CLUST)
##' 
##' \dontrun{
##' require(foreach)
##' require(ggplot2)
##' require(ggdendro)
##' pp = foreach(x = names(sp.CLUST$clust.dendrograms)) %do%
##' {
##'   hc = sp.CLUST$clust.dendrograms[[x]]
##'   pp = ggdendrogram(hc, rotate = TRUE) +
##'     labs(title = paste0("Hierarchical clustering based on species distance "
##'                         , ifelse(length(names(sp.CLUST$clust.dendrograms)) > 1
##'                                  , paste0("(group ", x, ")")
##'                                  , "")))
##'   return(pp)
##' }
##' plot(pp[[1]])
##' plot(pp[[2]])
##' plot(pp[[3]])
##' }
##' 
##' str(sp.CLUST$clust.evaluation)
##' 
##' plot(sp.CLUST$plot.clustMethod)
##' plot(sp.CLUST$plot.clustNo)
##' 
##' 
##' @export
##' 
##' @importFrom grDevices colorRampPalette
##' @importFrom graphics plot
##' @importFrom stats cor as.dist hclust cophenetic cutree
##' @importFrom utils tail
##' 
##' @importFrom foreach foreach %do%
##' @importFrom reshape2 melt
##' @importFrom fpc cluster.stats
##' @importFrom clValid dunn
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string
##' geom_line geom_point geom_vline geom_label
##' scale_color_manual scale_linetype_discrete 
##' facet_grid labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step1 = function(mat.species.DIST)
{
  
  #############################################################################
  
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
    } else if (class(mat.species.DIST) == "matrix") #is.matrix(mat.species.DIST))
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
  no_NA_values = sapply(mat.species.DIST, function(mat) sum(is.na(mat)))
  if (length(which(no_NA_values > 0)) > 0)
  {
    stop(paste0("Missing data!\n `mat.species.DIST` contain NA values ("
                , paste0(no_NA_values, collapse = ", ")
                , "), clustering with `hclust` function might have "
                , "problems dealing with this data"))
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesClustering_step1")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ### CLUSTERING
  #############################################################################
  
  ## HOW TO CHOOSE the best clustering method (complete, ward, single, average) ?
  ## Measure of similarity between input distance (mat.species.DIST)
  ## and the one obtained with the clustering (clust.DIST)
  ## WHICH MUST BE MINIMIZED
  ## (Mouchet et al. 2008)
  
  avail.methods = c("complete", "ward.D", "ward.D2", "single",
                    "average", "mcquitty", "median", "centroid")
  clust.choice = foreach(clust.method = avail.methods) %do% {
    ## CALCULATE dendrograms from distance matrices
    clust.dendrograms = lapply(mat.species.DIST, function(x) {
      hclust(as.dist(x), method = clust.method)
    })
    ## CALCULATE THE DISTANCES corresponding to these dendrograms
    clust.DIST = lapply(clust.dendrograms, cophenetic)
    
    ## CALCULATE Mouchet measure
    clust.choice = sapply(1:length(clust.DIST), function(x){
      return(1 - (cor(as.dist(clust.DIST[[x]]), as.dist(mat.species.DIST[[x]])) *
                    cor(as.dist(clust.DIST[[x]]), as.dist(mat.species.DIST[[x]]))))
    })
    
    return(data.frame(clust.method = clust.method
                      , GROUP = group_names
                      , metric = clust.choice
                      , stringsAsFactors = FALSE))
  }
  clust.choice = do.call(rbind, clust.choice)
  
  ## Check for NA values
  if (length(group_names) == 1)
  {
    no_NA_values = length(which(is.na(clust.choice$metric)))
    no_NA_values = (no_NA_values == nrow(clust.choice))
  } else {
    no_NA_values = sapply(group_names, function(x) 
      length(which(is.na(clust.choice$metric[which(clust.choice$GROUP == x)]))))
    no_NA_values = (no_NA_values == sapply(group_names, function(x) 
      length(which(clust.choice$GROUP == x))))
    no_NA_values = (sum(no_NA_values) >= 1)
  }
  if (no_NA_values)
  {
    stop(paste0("All clustering methods (maybe for a specific group) "
                , "give NA values for Mouchet measure.\n"
                , "Please check if you have sufficient values to run `hclust` function"))
  }
  
  ## GRAPHICAL REPRESENTATION -------------------------------------------------
  pp1 = ggplot(clust.choice, aes_string(x = "GROUP"
                                        , y = "metric"
                                        , group = "clust.method"
                                        , lty = "clust.method")) +
    geom_line(lwd = 0.8) +
    geom_point() +
    geom_label(data = clust.choice[which(clust.choice$GROUP == tail(levels(clust.choice$GROUP), 1)), ]
               , aes_string(label = "clust.method")
               , hjust = -0.1) +
    scale_linetype_discrete(guide = F) +
    labs(x="", y = ""
         , title = "STEP A : Choice of clustering method"
         , subtitle = paste0("Similarity between input and clustering distances "
                             , "(must be minimized, Mouchet et al. 2008)\n"
                             , "depending on clustering method.")) +
    .getGraphics_theme() +
    theme(axis.ticks = element_blank()
          , axis.text.y = element_text(angle = 0))
  
  plot(pp1)
  
  ## CHOICE OF CLUSTERING METHOD ----------------------------------------------
  clust.method = sapply(split(clust.choice, clust.choice$GROUP), function(x){
    x$clust.method[which.min(x$metric)]
  })
  clust.method = names(which.max(table(clust.method)))
  
  ## CALCULATE dendrograms from distance matrices
  clust.dendrograms = lapply(mat.species.DIST, function(x) {
    hclust(as.dist(x), method = clust.method)
  })
  
  cat("\n  Clustering method : ", clust.method)
  cat("\n  Clustering evaluation...")
  cat("\n")
  
  #############################################################################
  ### EVALUATION OF CLUSTERING
  #############################################################################
  
  ## COMPUTATION OF SEVERAL INDICES TO EVALUATE THE 'QUALITY' OF CLUSTERING
  ## Calculated for each group, and varying the number of clusters
  
  min_no_species_in_group = sapply(mat.species.DIST, function(x) ncol(as.matrix(x)))
  min_no_species_in_group = sapply(min_no_species_in_group, function(x) min(x, 15))
  combi = foreach(group = 1:length(group_names), .combine = "rbind") %do% {
    expand.grid(no.clusters = 2:(min_no_species_in_group[group] - 1), GROUP = group)
  }
  
  clust.evaluation = foreach(group = combi$GROUP, no.clusters = combi$no.clusters) %do%
  {
    
    k1 = no.clusters
    k2 = no.clusters + 1
    c1 = cutree(clust.dendrograms[[group]], k = k1)
    c2 = cutree(clust.dendrograms[[group]], k = k2)
    stats = cluster.stats(mat.species.DIST[[group]], c1, c2)
    
    ## Dunn index : ratio of the smallest distance between observations
    ## not in the same cluster to the largest intra-cluster distance.
    ## Value between zero and infinity, and should be maximized.
    mdunn = dunn(mat.species.DIST[[group]], c1)
    
    ## Meila's VI index (Variation of Information) : measures the amount of 
    ## information lost and gained in changing between 2 clusterings.
    ## Should be minimized (?)
    mVI = stats$vi
    
    ## Value between zero and one. Should be maximized.
    R2 = stats$average.between / (stats$average.between + stats$average.within)
    
    ## Calinski and Harabasz index : 
    ## The higher the value, the "better" is the solution.
    ch = stats$ch
    
    ## Corrected rand index : measure of the similarity between two data clusterings.
    ## Value between 0 and 1, with 0 indicating that the two data clusters do not agree
    ## on any pair of points and 1 indicating that the data clusters are exactly the same.
    Rand = stats$corrected.rand
    
    ## Average silhouette width :
    ## Observations with a large s(i) (almost 1) are very well clustered,
    ## a small s(i) (around 0) means that the observation lies between two clusters,
    ## and observations with a negative s(i) are probably placed in the wrong cluster.
    ## Should be maximized.
    av.sil = stats$avg.silwidth
    
    return(data.frame(GROUP = group_names[group]
                      , no.clusters
                      , mdunn, mVI, R2, ch, Rand, av.sil
                      , stringsAsFactors = FALSE))
  }
  clust.evaluation = do.call(rbind, clust.evaluation)
  clust.evaluation = melt(clust.evaluation, id.vars = c("GROUP","no.clusters"))
  
  
  ## Find number of cluster which give optimal variable values ----------------
  combi = expand.grid(GROUP = group_names
                      , variable = unique(clust.evaluation$variable))
  
  clust.evaluation.optim = foreach(group = combi$GROUP
                                   , variable = combi$variable) %do%
  {
    tmp = clust.evaluation[which(clust.evaluation$GROUP == group &
                                   clust.evaluation$variable == variable),]
    if(variable == "mVI")
    {
      optim = unique(sort(tmp$value, decreasing = F))[1:3]
      ind.optim = which(tmp$value %in% optim)
    } else {
      optim = unique(sort(tmp$value, decreasing = T))[1:3]
      ind.optim = which(tmp$value %in% optim)
    }
    optim.clust = tmp$no.clusters[ind.optim]
    optim.val = tmp$value[ind.optim]
    return(data.frame(GROUP = group
                      , variable
                      , optim.clust
                      , optim.val
                      , stringsAsFactors = FALSE))
  }
  clust.evaluation.optim = do.call(rbind, clust.evaluation.optim)
  
  
  ## GRAPHICAL REPRESENTATION -------------------------------------------------
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  pp2 = ggplot(clust.evaluation, aes_string(x = "no.clusters", y = "value")) +
    facet_grid("variable ~ GROUP", scales = "free") +
    geom_line() +
    geom_point() +
    geom_vline(data = clust.evaluation.optim
               , aes_string(xintercept = "optim.clust", color = "group")
               , lwd = 4
               , alpha = 0.3) +
    scale_color_manual(guide = F, values = colRamp(length(group_names))) +
    labs(x = "", y = ""
         , title = "STEP B : Choice of number of clusters"
         , subtitle = paste0("Evolution of clustering evaluation variables with "
                             , "the number of clusters in each group.\n"
                             , "All values except that of mVI must be maximized "
                             , "(check function's help for more details about the measures).\n"
                             , "The number of clusters with values among the 3 best are highlighted.")) +
    .getGraphics_theme()
  
  plot(pp2)
  
  ## ----------------------------------------------------------------------
  pdf(file = "PRE_FATE_CLUSTERING_STEP_1_numberOfClusters.pdf"
      , width = 10, height = 8)
  plot(pp1)
  plot(pp2)
  dev.off()
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  return(list(clust.dendrograms = clust.dendrograms
              , clust.evaluation = clust.evaluation
              , plot.clustMethod = pp1
              , plot.clustNo = pp2))
  
}

