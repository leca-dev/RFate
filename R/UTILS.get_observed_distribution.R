 
#################################################################

get_observed_distribution <- function(releves.PFG
                                    , hab.obs.compo = NULL
                                    , studied.habitat
                                    , PFG.considered_PFG.compo
                                    , strata.considered_PFG.compo
                                    , habitat.considered_PFG.compo
                                    , perStrata = FALSE
                                    , output.path){

  
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
  }else
  {
    stop("Abund data in releves.PFG must be Braun-Blanquet abundance, presences absence or absolute abundance values.")
  }
  
  if (perStrata == TRUE & !is.null(hab.obs.compo)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + strata, data = releves.PFG, FUN = "sum")
  } else if (perStrata == FALSE & !is.null(hab.obs.compo)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata = "A"
  } else if (perStrata == TRUE & is.null(hab.obs.compo)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + strata + code.habitat, data = releves.PFG, FUN = "sum")
  } else if (perStrata == FALSE & is.null(hab.obs.compo)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + code.habitat, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata = "A"
  }
  
  #2. Get habitat information
  ###################################
  
  #get habitat code and name
  coord = releves.PFG %>% group_by(site) %>% filter(!duplicated(site))
  if(is.null(hab.obs.compo))
  {
    mat.PFG.agg = merge(mat.PFG.agg, coord[,c("site","x","y","code.habitat")], by = "site")
  }
  if(!is.null(hab.obs.compo))
  {
    mat.PFG.agg = merge(mat.PFG.agg, coord[,c("site","x","y")], by = "site")
    mat.PFG.agg$code.habitat = extract(x = hab.obs.compo, y = mat.PFG.agg[,c("x", "y")])
    mat.PFG.agg = mat.PFG.agg[which(!is.na(mat.PFG.agg$code.habitat)), ]
    if (nrow(mat.PFG.agg) == 0) {
      stop("Code habitat vector is empty. Please verify values of your hab.obs map")
    }
  }
  
  #correspondence habitat code/habitat name
  table.habitat.releve = studied.habitat
  mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$code.habitat %in% studied.habitat$ID), ] # filter non interesting habitat + NA
  mat.PFG.agg = merge(mat.PFG.agg, table.habitat.releve[, c("ID", "habitat")], by.x = "code.habitat", by.y = "ID")
  
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
  
  cat("\n > Releve data have been transformed into a relative metric \n")
  
  
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
