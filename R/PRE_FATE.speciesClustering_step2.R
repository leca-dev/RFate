### HEADER #####################################################################
##' @title Choose clusters and select determinant species
##' 
##' @name PRE_FATE.speciesClustering_step2
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to obtain functional groups by : 1) 
##' selecting the number of clusters to be kept from an object obtained with 
##' the \code{\link{PRE_FATE.speciesClustering_step1}} function ; 2) refining 
##' these groups by identifying determinant species in each of them.
##'              
##' @param clust.dendrograms a dendrogram, or a \code{list} of dendrograms (one 
##' for each \code{GROUP} value). \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesClustering_step1}} function.
##' @param no.clusters an \code{integer}, or a \code{vector} of \code{integer} 
##' (one for each \code{GROUP} value), with the number of clusters to be kept
##' @param mat.species.DIST a \code{dist} object, or a \code{list} of 
##' \code{dist} objects (one for each \code{GROUP} value), corresponding to the 
##' distance between each pair of species. \cr Such an object can be obtained 
##' with the \code{\link{PRE_FATE.speciesDistance}} function.
##' 
##' 
##' @details 
##' 
##' This function allows to obtain a classification of \strong{dominant} 
##' species into Plant Functional Groups (PFG), and the \strong{determinant} 
##' species based on these PFG. \cr \cr
##' 
##' \strong{What is the difference between \code{dominant} and
##' \code{determinant} species ?}
##' 
##' \itemize{
##'   \item{\strong{Dominant} species are species representative of an 
##'   environment or a studied area, in terms of number of releves or 
##'   abundance values. They can be found with the 
##'   \code{\link{PRE_FATE.selectDominant}} function of this package. These 
##'   dominant species are used to build PFG with the 
##'   \code{\link{PRE_FATE.speciesClustering_step1}} function.
##'   }
##'   \item{Once PFG are built, \strong{determinant} species are defined as 
##'   refined subsets of dominant species within each PFG. \cr The process is 
##'   detailed below :
##'   \itemize{
##'     \item each dominant species is assigned to a PFG
##'     \item within each PFG :
##'     \itemize{
##'       \item for each species, compute its mean distance to the other 
##'       species within the PFG (\code{sp.mean.dist})
##'       \item calculate the mean value of all these mean distances
##'       (\code{allSp.mean})
##'       \item calculate the deviation values around this mean value
##'       (\code{allSp.min} and \code{allSp.max})
##'       \item determinant species are the ones that are included between 
##'       those deviation values
##'     }
##'   }
##'   }
##' }
##' 
##' @return A \code{list} containing one \code{vector}, one \code{data.frame} 
##' with the following columns, and two \code{ggplot2} objects :
##' 
##' \describe{
##'   \item{determ.sp}{the names of all determinant species}
##'   \item{determ.all}{(\emph{determinant and non-determinant species})
##'   \describe{
##'     \item{\code{PFG}}{ID of the plant functional group 
##'     (\code{GROUP} + \code{ID.cluster})}
##'     \item{\code{GROUP}}{name of data subset}
##'     \item{\code{ID.cluster}}{cluster number}
##'     \item{\code{species}}{name of species}
##'     \item{\code{ID.species}}{species number in each PFG}
##'     \item{\code{sp.mean.dist}}{species mean distance to other species of 
##'     the same PFG}
##'     \item{\code{allSp.mean}}{\eqn{mean(\text{sp.mean.dist})} within the PFG}
##'     \item{\code{allSp.min}}{\eqn{mean(\text{sp.mean.dist}) - 1.64 * 
##'     sd(\text{sp.mean.dist})} within the PFG}
##'     \item{\code{allSp.max}}{\eqn{mean(\text{sp.mean.dist}) + 1.64 * 
##'     sd(\text{sp.mean.dist})} within the PFG}
##'     \item{\code{DETERMINANT}}{\code{TRUE} if determinant species, \code{FALSE} 
##'     otherwise}
##'   }
##'   }
##'   \item{plot.distance}{\code{ggplot2} object, representing the distribution 
##'   of mean distances between species for each functional group} 
##'   \item{plot.PCO}{\code{list} of \code{ggplot2} objects, representing the 
##'   PFG within the functional space \cr \cr}
##' }
##' 
##' One \file{PRE_FATE_CLUSTERING_STEP_2_distantSpecies_PCO.pdf} file is created 
##' containing two types of graphics : 
##' \describe{
##'   \item{distantSpecies}{to visualize in each PFG the distribution of mean 
##'   distance of each species to other species, and non-determinant species 
##'   which are outside the distribution}
##'   \item{PCO}{to visualize in each PFG the distribution of species, with and 
##'   without non-determinant species}
##' }
##' 
##' @keywords hierarchical clustering, Principal Component Ordination
##' 
##' @seealso \code{\link[stats]{cutree}},
##' \code{\link[ade4]{quasieuclid}},
##' \code{\link[ade4]{dudi.pco}},
##' \code{\link{PRE_FATE.speciesDistance}},
##' \code{\link{PRE_FATE.speciesClustering_step1}},
##' \code{\link{PRE_FATE.speciesClustering_step3}}
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
##' 
##' ## Build dendrograms -------------------------------------------------------------------------
##' sp.CLUST = PRE_FATE.speciesClustering_step1(mat.species.DIST = tab.dist)
##' names(sp.CLUST)
##' 
##' 
##' ## Number of clusters per group
##' plot(sp.CLUST$plot.clustNo)
##' no.clusters = c(5, 5, 9)
##' 
##' ## Find determinant species ------------------------------------------------------------------
##' sp.DETERM = PRE_FATE.speciesClustering_step2(clust.dendrograms = sp.CLUST$clust.dendrograms
##'                                              , no.clusters = no.clusters
##'                                              , mat.species.DIST = tab.dist)
##' names(sp.DETERM)
##' 
##' str(sp.DETERM$determ.sp)
##' str(sp.DETERM$determ.all)
##' 
##' ## Species names
##' sp.NAMES = DATASET_Bauges_PFG$sp.names
##' sp.NAMES$species = paste0("X", sp.NAMES$species)
##' determ = merge(sp.DETERM$determ.all, sp.NAMES, by = "species", all.x = TRUE)
##' str(determ)
##' 
##' plot(sp.DETERM$plot.distance)
##' plot(sp.DETERM$plot.PCO$Chamaephyte)
##' plot(sp.DETERM$plot.PCO$Herbaceous)
##' plot(sp.DETERM$plot.PCO$Phanerophyte)
##' 
##' 
##' @export
##' 
##' @importFrom grDevices colorRampPalette pdf dev.off
##' @importFrom stats as.dist cutree sd
##' @importFrom graphics plot
##' 
##' @importFrom ade4 quasieuclid dudi.pco inertia.dudi
##' 
##' @importFrom ggplot2 ggplot ggsave aes_string 
##' geom_point geom_hline geom_vline geom_errorbar geom_path
##' scale_color_discrete scale_color_manual scale_shape_manual scale_size_manual
##' facet_grid labs theme element_text element_blank
##' @importFrom ggthemes theme_fivethirtyeight
##' @importFrom ggrepel geom_label_repel
##' 
## END OF HEADER ###############################################################


PRE_FATE.speciesClustering_step2 = function(clust.dendrograms
                                            , no.clusters
                                            , mat.species.DIST
){
  
  #############################################################################
  
  ## CHECK parameter clust.dendrograms
  if (.testParam_notInClass(clust.dendrograms, c("list","hclust")))
  {
    stop("No data given!\n (missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  } else
  {
    if (class(clust.dendrograms) == "list" &&
        length(which(sapply(clust.dendrograms, class) == "hclust")) < length(clust.dendrograms))
    {
      stop("Wrong type of data!\n each element of `clust.dendrograms` must be of class `hclust`")
    }
    if (class(clust.dendrograms) == "hclust")
    {
      clust.dendrograms = list(GROUP1 = clust.dendrograms)
    }
    if(!is.null(names(clust.dendrograms)))
    {
      group_names = names(clust.dendrograms)
    } else
    {
      group_names = paste0("GROUP", 1:length(clust.dendrograms))
    }
  }
  ## CHECK parameter no.clusters
  if (.testParam_notNum(no.clusters))
  {
    stop("No data given!\n (missing `no.clusters` information)")
  } else
  {
    if (length(no.clusters) != length(clust.dendrograms))
    {
      stop("Wrong type of data!\n `no.clusters` must have the same length than `clust.dendrograms`")
    }
  }
  ## CHECK parameter mat.species.DIST
  if (.testParam_notInClass(mat.species.DIST, c("list","dist")))
    
  {
    stop("No data given!\n (missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  } else
  {
    if (class(mat.species.DIST) == "list" &&
        length(mat.species.DIST) != length(clust.dendrograms))
    {
      stop("Wrong type of data!\n `mat.species.DIST` must have the same length than `clust.dendrograms`")
    }
    if (class(mat.species.DIST) == "dist")
    {
      mat.species.DIST = list(GROUP1 = mat.species.DIST)
    }
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.speciesClustering_step2 : DETERMINANT SPECIES")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  ## DEFINITION OF CLUSTERED GROUPS
  #############################################################################
  
  ### CUT dendrogramS (or trees) RESULTING FROM hclust INTO SEVERAL GROUPS (no = k)
  ## Number of groups for each group has been chosen 
  ## according to the previous plot (CLUSTERING_STEP_1B)
  clust.groups = lapply(1:length(group_names), function(x) 
    cutree(clust.dendrograms[[x]], k = no.clusters[x]))
  clust.groups = lapply(1:length(group_names), function(x) {
    tmp.names = names(clust.groups[[x]])
    tmp = paste0(group_names[x], ".", clust.groups[[x]])
    names(tmp) = tmp.names
    return(tmp)
  })
  clust.groups = unlist(clust.groups)
  
  PFG_names = sort(unique(clust.groups))
  
  #############################################################################
  ## IDENTIFY DETERMINANT SPECIES
  #############################################################################

  ## DETERMINANT SPECIES are the ones whose mean distance to other species
  ## is in the distribution of mean distances of every species to other species
  
  determ = foreach(pfg = PFG_names) %do%
    {
      ## get the SPECIES names
      sp = names(clust.groups)[which(clust.groups == pfg)]
      
      if (length(sp) > 1)
      {
        ## get the GROUP information
        group = strsplit(pfg, "[.]")[[1]][1]
        ID.cluster = strsplit(pfg, "[.]")[[1]][2]
        ## get the DISTANCE information
        mat = as.matrix(mat.species.DIST[[group]])[sp, sp]
        
        ## compute for each species its mean distance to other species
        sp.mean.dist = apply(mat, 1, mean, na.rm = T)
        
        return(data.frame(PFG = pfg
                          , GROUP = group
                          , ID.cluster
                          , species = sp
                          , ID = 1:length(sp)
                          , sp.mean.dist
                          , allSp.mean = mean(sp.mean.dist)
                          , allSp.min = mean(sp.mean.dist) - 1.64 * sd(sp.mean.dist)
                          , allSp.max = mean(sp.mean.dist) + 1.64 * sd(sp.mean.dist)
                          , stringsAsFactors = FALSE))
      } else if (length(sp) == 1)
      {
        return(data.frame(PFG = pfg
                          , GROUP = group
                          , ID.cluster
                          , species = sp
                          , ID = 1
                          , sp.mean.dist = NA
                          , allSp.mean = NA
                          , allSp.min = NA
                          , allSp.max = NA
                          , stringsAsFactors = FALSE))
      }
    }
  determ = do.call(rbind, determ)
  rownames(determ) = determ$species
  if (length(determ) == 0 || is.null(determ))
  {
    stop("No determinant species have been selected. Please check your data")
  }
  determ$DETERMINANT = TRUE
  determ$DETERMINANT[which(determ$sp.mean.dist > determ$allSp.max)] = FALSE
  determ$DETERMINANT[which(determ$sp.mean.dist < determ$allSp.min)] = FALSE
  determ$DETERMINANT = factor(determ$DETERMINANT, c(TRUE, FALSE))
  
  ## SAVE DETERM
  ## CAT INFO ABOUT HOW MANY SPECIES ARE REMOVED
  ## CAT INFO ABOUT HOW MANY SPECIES in each PFG
  
  #############################################################################
  ## GRAPHICAL REPRESENTATIONS
  #############################################################################
  
  ## GRAPHICAL REPRESENTATION 1 -----------------------------------------------
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  colLev = levels(interaction(determ$DETERMINANT, determ$GROUP))
  
  pp3 = ggplot(determ, aes_string(x = "PFG", y = "sp.mean.dist"
                                  , color = interaction(determ$DETERMINANT, determ$GROUP)
                                  , shape = "DETERMINANT")) +
    scale_color_manual(guide = F, values = colRamp(length(colLev))) +
    scale_shape_manual(guide = F, values = c("0" = 20, "1" = 8)) +
    geom_errorbar(aes_string(ymin = "allSp.min", ymax = "allSp.max")
                  , color = "darkblue") +
    geom_point(position = "jitter") +
    geom_point(aes_string(y = "allSp.mean")
               , pch = 18
               , lwd = 5
               , color = "darkblue") +
    facet_grid("~ GROUP", scales = "free_x") +
    labs(x = "", y = "Mean distance to other species"
         , title = "STEP C : Removal of distant species"
         , subtitle = paste0("Only species whose mean distance to other species "
                             , "is included in the distribution\n"
                             , "of all PFG's species mean distances to other species are kept.\n"
                             , "Species indicated with * will be removed from PFGs.\n"
                             , "Non-represented PFG might be one-species-only.")) +
    .getGraphics_theme() +
    theme(axis.ticks.x = element_blank()
          , axis.text.x = element_text(angle = 90))
  
  plot(pp3)
  
  
  ## GRAPHICAL REPRESENTATION 2 -----------------------------------------------
  ## Compute Principal Coordinates Analysis (PCO) for the determinantes of 
  ## each group to see the position in "distance space" (overlap + traits) 
  ## between species and groups
  
  colRamp = colorRampPalette(c('#8e0152','#c51b7d','#de77ae','#7fbc41','#4d9221','#276419'))
  
  pp4_list = foreach(group = group_names) %do%
  {
    ## Transform results of clustering into distance matrix
    tmp = determ[which(determ$GROUP == group),]
    mat = mat.species.DIST[[group]]
    mat = quasieuclid(as.dist(mat))
    
    ## Transform distance matrix into PCO
    PCO = dudi.pco(mat, scannf = FALSE, nf = 3) ## PCO
    PCO.li = PCO$li
    PCO.li$det = ifelse(rownames(PCO.li) %in% tmp$sp[which(tmp$DETERMINANT == FALSE)], 0, 1)
    PCO.li$det = factor(PCO.li$det, c(0, 1))
    PCO.li$PFG = clust.groups[rownames(PCO.li)]
    
    ## GET inertia values
    inert = inertia.dudi(PCO)$tot.inertia
    inert = c(inert$`cum(%)`[1]
              , inert$`cum(%)`[2] - inert$`cum(%)`[1]
              , inert$`cum(%)`[3] - inert$`cum(%)`[2])
    
    ## GET ellipses
    PCO.li.ELL = .getELLIPSE(xy = PCO.li[, c("A1", "A2")], fac = PCO.li$PFG)
    PCO.li.ELL.det = .getELLIPSE(xy = PCO.li[which(PCO.li$det == 1), c("A1", "A2")]
                                 , fac = PCO.li$PFG[which(PCO.li$det == 1)])
    
    
    pp4 = ggplot(PCO.li, aes_string(x = "A1", y = "A2", color = "PFG")) +
      geom_hline(yintercept = 0, color = "grey30", lwd = 1) +
      geom_vline(xintercept = 0, color = "grey30", lwd = 1) +
      geom_point(aes_string(shape = "det", size = "det"), alpha = 0.5) +
      geom_path(data = PCO.li.ELL, aes_string(x = "x", y = "y"), lty = 2) +
      geom_path(data = PCO.li.ELL.det, aes_string(x = "x", y = "y")) +
      geom_label_repel(data = unique(PCO.li.ELL[, c("xlabel", "ylabel", "PFG")])
                       , aes_string(x = "xlabel", y = "ylabel", label = "PFG")) +
      scale_shape_manual(guide = F, values = c("0" = 8, "1" = 20)) +
      scale_size_manual(guide = F, values = c("0" = 3, "1" = 1)) +
      scale_color_discrete(guide = F) +
      labs(x = paste0("\n1st axis = ", round(inert[1], 1), "% of inertia")
           , y = paste0("2nd axis = ", round(inert[2], 1), "% of inertia\n")
           , title = paste0("STEP C : Removal of distant species : group ", group)
           , subtitle = paste0("Only species whose mean distance to other species "
                               , "is included in the distribution\n"
                               , "of all PFG's species mean distances to other species are kept.\n"
                               , "Species indicated with * will be removed from PFGs.\n"
                               , "Inertia ellipse are represented, with (dashed) and "
                               , "without (solid) non-determinant species.")) +
      .getGraphics_theme() +
      theme(axis.title = element_text(inherit.blank = FALSE))
    
    plot(pp4)
    return(pp4)
  }
  names(pp4_list) = group_names
  
  
  #############################################################################
  
  cat("\n> Done!\n")
  
  pdf(file = "PRE_FATE_CLUSTERING_STEP_2_distantSpecies_PCO.pdf"
      , width = 10, height = 8)
  plot(pp3)
  for(group in group_names)
  {
    plot(pp4_list[[group]])
  }
  dev.off()
  
  return(list(determ.sp = determ$species[which(determ$DETERMINANT == TRUE)]
              , determ.all = determ
              , plot.distance = pp3
              , plot.PCO = pp4_list))
}

