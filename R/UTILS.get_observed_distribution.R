### HEADER #####################################################################
##'
##' @title Compute distribution of relative abundance over observed relevés
##'
##' @name get.observed.distribution
##'
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to compute distribution, per PFG/strata/habitat,
##' of relative abundance, from observed data.
##' 
##' @param name.simulation simulation folder name.
##' @param releves.PFG a data frame with abundance (column named abund) at each site
##' and for each PFG and strata.
##' @param releves.sites a data frame with coordinates and a description of the habitat associated with 
##' the dominant species of each site in the studied map.
##' @param hab.obs a raster map of the extended studied map in the simulation, with same projection 
##' & resolution than simulation mask.
##' @param studied.habitat default \code{NULL}. If \code{NULL}, the function will
##' take into account of habitats define in the \code{hab.obs} map. Otherwise, please specify 
##' in a 2 columns data frame the habitats (2nd column) and the ID (1st column) for each of them which will be taken 
##' into account for the validation.
##' @param PFG.considered_PFG.compo a character vector of the list of PFG considered
##' in the validation.
##' @param strata.considered_PFG.compo a character vector of the list of precise 
##' strata considered in the validation.
##' @param habitat.considered_PFG.compo a character vector of the list of habitat(s)
##' considered in the validation.
##' @param perStrata \code{Logical}. All strata together (FALSE) or per strata (TRUE).
##' @param sim.version name of the simulation we want to validate (it works with
##' only one \code{sim.version}).
##' 
##' @details
##' 
##' The function takes the \code{releves.PFG} and \code{releves.sites} files and 
##' aggregate coverage per PFG. Then, the code get habitat information from also
##' the \code{hab.obs} map, keep only interesting habitat, strata and PFG, and
##' transform the data into relative metrics. Finally, the script computes distribution
##' per PFG, and if require per strata/habitat (else all strata/habitat will be considered together).
##' 
##' @return 
##' 
##' 2 files are created in
##' \describe{
##'   \item{\file{VALIDATION/PFG_COMPOSITION/sim.version} :
##'   1 .csv file which contain the observed relevés transformed into relative metrics.
##'   1 .csv file which contain the final output with the distribution per PFG, strata and habitat.
##'   
##' @export
##' 
##' @importFrom dplyr select filter group_by mutate %>% rename
##' @importFrom raster compareCRS res crs levels
##' @importFrom stats aggregate
##' @importFrom sf st_transform st_crop
##' @importFrom utils write.csv
##' @importFrom data.table setDT
##' 
### END OF HEADER ##############################################################

get.observed.distribution<-function(releves.PFG
                                    , releves.sites
                                    , hab.obs
                                    , studied.habitat = NULL
                                    , PFG.considered_PFG.compo
                                    , strata.considered_PFG.compo
                                    , habitat.considered_PFG.compo
                                    , perStrata){
  
  cat("\n ---------- GET OBSERVED DISTRIBUTION \n")
  
  # composition.mask = NULL
  
  #1. Aggregate coverage per PFG
  #########################################
  
  #transformation into coverage percentage
  if(!is.numeric(releves.PFG$abund)) # Braun-Blanquet abundance
  {
    releves.PFG <- filter(releves.PFG,is.element(abund,c(NA, "NA", 0, "+", "r", 1:5)))
    releves.PFG$coverage = PRE_FATE.abundBraunBlanquet(releves.PFG$abund)/100
  } else if (is.numeric(releves.PFG$abund) & max(releves.PFG$abund) == 1) # presence-absence data
  {
    releves.PFG$coverage = releves.PFG$abund
  } else if (is.numeric(releves.PFG$abund)) # absolute abundance
  {
    releves.PFG$coverage = releves.PFG$abund
  }
  
  if(perStrata == T){
    mat.PFG.agg <- aggregate(coverage ~ site + PFG + strata, data = releves.PFG, FUN = "sum")
  }else if(perStrata == F){
    mat.PFG.agg <- aggregate(coverage ~ site + PFG, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata <- "A" #"A" is for "all".
  }
  
  #2. Get habitat information
  ###################################
  
  #get sites coordinates
  mat.PFG.agg = merge(releves.sites, mat.PFG.agg, by = "site")
  
  #get habitat code and name
  mat.PFG.agg$code.habitat = raster::extract(x = hab.obs, y = mat.PFG.agg[, c("x", "y")])
  mat.PFG.agg = mat.PFG.agg[which(!is.na(mat.PFG.agg$code.habitat)), ]
  if (nrow(mat.PFG.agg) == 0) {
    stop("Code habitat vector is empty. Please verify values of your hab.obs map")
  }
  
  #correspondance habitat code/habitat name
  if (!is.null(studied.habitat) & nrow(studied.habitat) > 0 & ncol(studied.habitat) == 2)
  { # cas où pas de levels dans la carte d'habitat et utilisation d'un vecteur d'habitat
    colnames(obs.habitat) = c("ID", "habitat")
    table.habitat.releve = studied.habitat
    mat.PFG.agg = merge(mat.PFG.agg, table.habitat.releve[, c("ID", "habitat")], by.x = "code.habitat", by.y = "ID")
    print(cat("habitat classes used in the RF algo: ", unique(mat.PFG.agg$habitat), "\n", sep = "\t"))
  } else if (names(raster::levels(hab.obs)[[1]]) == c("ID", "habitat", "colour") & nrow(raster::levels(hab.obs)[[1]]) > 0 & is.null(studied.habitat))
  { # cas où on utilise les levels définis dans la carte
    table.habitat.releve = levels(hab.obs)[[1]]
    mat.PFG.agg = merge(mat.PFG.agg, table.habitat.releve[, c("ID", "habitat")], by.x = "code.habitat", by.y = "ID")
    mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$habitat %in% studied.habitat$habitat), ]
    print(cat("habitat classes used in the RF algo: ", unique(mat.PFG.agg$habitat), "\n", sep = "\t"))
  } else
  {
    stop("Habitat definition in hab.obs map is not correct")
  } 
  
  # #(optional) keep only releves data in a specific area
  # if(!is.null(composition.mask)){
  #   
  #   if(compareCRS(mat.PFG.agg,composition.mask)==F){ #as this stage it is not a problem to transform crs(mat.PFG.agg) since we have no more merge to do (we have already extracted habitat info from the map)
  #     mat.PFG.agg<-st_transform(x=mat.PFG.agg,crs=crs(composition.mask))
  #   }
  #   
  #   mat.PFG.agg<-st_crop(x=mat.PFG.agg,y=composition.mask)
  #   print("'releve' map has been cropped to match 'external.training.mask'.")
  # }
  
  
  # 3. Keep only releve on interesting habitat, strata and PFG
  ##################################################################"
  
  mat.PFG.agg <- as.data.frame(mat.PFG.agg)
  mat.PFG.agg <- dplyr::select(mat.PFG.agg,c(site,PFG,strata,coverage,habitat))
  
  mat.PFG.agg <- filter(
    mat.PFG.agg,
    is.element(PFG, PFG.considered_PFG.compo) &
      is.element(strata, strata.considered_PFG.compo) &
      is.element(habitat, habitat.considered_PFG.compo)
  )
  
  
  #4.Transform into a relative metrics (here relative.metric is relative coverage)
  ###################################################################################
  
  #important to do it only here, because if we filter some PFG, it changes the value of the relative metric
  #careful: if several strata are selected, the computation is made for each strata separately
  mat.PFG.agg <- as.data.frame(mat.PFG.agg %>% group_by(site, strata) %>% mutate(relative.metric = round(prop.table(coverage), digits = 2)))
  mat.PFG.agg$relative.metric[is.na(mat.PFG.agg$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  mat.PFG.agg$coverage <- NULL
  
  print("releve data have been transformed into a relative metric")
  
  
  # 5. Save data
  #####################
  write.csv(mat.PFG.agg, paste0(output.path, "/obs.releves.prepared.csv"), row.names = F)
  
  
  # 6. Compute distribution per PFG, and if require per strata/habitat (else all strata/habitat will be considered together)
  ####################################
  
  distribution <- setDT(mat.PFG.agg)[, quantile(relative.metric), by = c("PFG", "habitat", "strata")]
  distribution <- rename(distribution, "quantile" = "V1")
  distribution <- data.frame(distribution, rank = seq(0, 4, 1)) #to be able to sort on quantile
  
  # 7. Add the missing PFG*habitat*strata
  #final distribution is the distribution once the missing combination have been added. For these combination, all quantiles are set to 0
  
  observed.distribution <- expand.grid(
    PFG = PFG.considered_PFG.compo,                              
    habitat = habitat.considered_PFG.compo,
    strata = strata.considered_PFG.compo
  )
  
  null.quantile <- data.frame(rank = seq(0, 4, 1)) #to have 5 rows per PFG*strata*habitat
  observed.distribution <- merge(observed.distribution, null.quantile, all = TRUE)
  
  observed.distribution <- merge(observed.distribution, distribution, by = c("PFG", "habitat", "strata", "rank"), all.x = TRUE) # "all.x=T" to keep the unobserved combination
  
  observed.distribution$quantile[is.na(observed.distribution$quantile)] <- 0
  
  # 8. Order the table to be able to have output in the right format
  observed.distribution <- setDT(observed.distribution)
  observed.distribution <- observed.distribution[order(habitat, strata, PFG, rank)]
  
  observed.distribution <- rename(observed.distribution, "observed.quantile" = "quantile")
  
  
  # 9. Save results
  ##########################################
  write.csv(observed.distribution, paste0(output.path, "/observed.distribution.csv"), row.names = F)
  
  # 8. Return
  ####################
  
  return(observed.distribution)
  
}