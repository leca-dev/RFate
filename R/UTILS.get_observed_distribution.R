####################################################################
##' @importFrom dplyr select filter group_by mutate %>% rename
##' @importFrom raster compareCRS res crs levels
##' @importFrom stats aggregate
##' @importFrom sf st_transform st_crop
##' @importFrom utils write.csv
##' @importFrom data.table setDT
####################################################################

get_observed_distribution <- function(mat.PFG.agg
                                      , hab.obs.compo = NULL
                                      , studied.habitat
                                      , list.PFG
                                      , list.strata
                                      # , perStrata = FALSE
                                      , output.path)
{
  
  # # 3. Keep only releve on interesting habitat, strata and PFG
  # ##################################################################"
  # mat.PFG.agg <- as.data.frame(mat.PFG.agg[, c("site", "code.habitat", "strata", "PFG", "abund")])
  # mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$code.habitat %in% studied.habitat$code.habitat &
  #                                   mat.PFG.agg$strata %in% list.strata &
  #                                   mat.PFG.agg$PFG %in% list.PFG), ]
  # 
  # 
  # #4.Transform into a relative metrics (here relative.metric is relative coverage)
  # ###################################################################################
  # 
  # #important to do it only here, because if we filter some PFG, it changes the value of the relative metric
  # #careful: if several strata are selected, the computation is made for each strata separately
  # 
  # ## Compute relative abundance per site and strata (might be all if not provided)
  # mat.PFG.agg = as.data.frame(
  #   mat.PFG.agg %>% group_by(site, strata) %>% 
  #     mutate(relative.metric = round(prop.table(abund), digits = 2))
  # )
  # 
  # ## Remove NA and abund column
  # if (length(which(is.na(mat.PFG.agg$relative.metric)))) {
  #   mat.PFG.agg$relative.metric[which(is.na(mat.PFG.agg$relative.metric))] = 0
  # }
  # mat.PFG.agg$abund = NULL
  # cat("\n > Releves data have been transformed into a relative metric")
  # write.csv(mat.PFG.agg, paste0(output.path, "/obs.releves.prepared.csv"), row.names = F)
  
  
  # 6. Compute distribution per PFG, and if require per strata/habitat (else all strata/habitat will be considered together)
  ####################################
  
  distrib = split(mat.PFG.agg, list(mat.PFG.agg$PFG, mat.PFG.agg$code.habitat, mat.PFG.agg$strata), drop = TRUE)
  distrib = foreach(tmp = distrib, .combine = "rbind") %do%
    {
      qt = quantile(tmp$relative.metric, probs = seq(0, 1, 0.25))
      return(data.frame(PFG = unique(tmp$PFG)
                       , code.habitat = unique(tmp$code.habitat)
                       , strata = unique(tmp$strata)
                       , quantile.perc = seq(0, 1, 0.25)
                       , quantile.obs = as.vector(qt)))
    }
  
  # 7. Add the missing PFG*habitat*strata
  #final distribution is the distribution once the missing combination have been added. For these combination, all quantiles are set to 0
  
  all.distrib <- expand.grid(PFG = list.PFG,                              
                             code.habitat = studied.habitat$code.habitat,
                             strata = list.strata, 
                             quantile.perc = seq(0, 1, 0.25),
                             stringsAsFactors = FALSE)
  all.distrib <- merge(all.distrib, distrib, by = c("PFG", "code.habitat", "strata", "quantile.perc"), all.x = TRUE)
  all.distrib$quantile.obs[is.na(all.distrib$quantile.obs)] <- 0
  all.distrib = all.distrib[order(all.distrib$code.habitat, all.distrib$strata, all.distrib$PFG, all.distrib$quantile.perc), ]
  
  write.csv(all.distrib, paste0(output.path, "/all.distrib.csv"), row.names = F)
  
  return(all.distrib)
}
