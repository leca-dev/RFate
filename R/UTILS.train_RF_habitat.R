################################################################
##' @importFrom dplyr %>% group_by
##' @importFrom stats aggregate
##' @importFrom reshape2 dcast
##' @importFrom randomForest randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom readr write_rds
##' @importFrom utils read.csv write.csv
#################################################################


train_RF_habitat = function(mat.PFG.agg
                            , hab.obs.RF = NULL
                            , external.training.mask = NULL
                            , studied.habitat
                            , RF.param
                            , output.path
                            # , perStrata = FALSE
                            , seed)
{
  
  # ## Get site and habitat informations
  # infos = unique(releves.PFG[, which(colnames(releves.PFG) %in% c("site", "x", "y", "code.habitat"))])
  # if (!is.null(hab.obs.RF)) {
  #   infos$code.habitat = extract(x = hab.obs.RF, y = infos[, c("x", "y")])
  #   infos = infos[which(!is.na(infos$code.habitat)), ]
  #   if (nrow(infos) == 0) {
  #     stop("Code habitat vector is empty. Please verify values of your hab.obs.RF map")
  #   }
  # }
  # infos = merge(infos, studied.habitat, by = "code.habitat")
  # ## Keep only releves in specific area
  # if (!is.null(external.training.mask)) {
  #   val.inMask = extract(x = external.training.mask, y = infos[, c("x", "y")])
  #   infos = infos[which(!is.na(val.inMask)), ]
  #   cat("\n 'releve' map has been cropped to match 'external.training.mask'. \n")
  # }
  # infos = infos[, c("site", "x", "y", "code.habitat", "habitat")]
  # 
  # ## KEEP only selected sites
  # releves.PFG = releves.PFG[which(releves.PFG$site %in% infos$site), ]
  # if (nrow(releves.PFG) == 0) {
  #   stop("PROBLEM") ## TODO
  # }
  
  
  # 1. Compute relative abundance metric
  ######################################
  
  # ## Compute sum of abundance per PFG (and per strata and per code.habitat if provided)
  # if (!is.null(hab.obs.RF)) {
  #   if (perStrata == TRUE) {
  #     mat.PFG.agg = aggregate(abund ~ site + PFG + strata, data = releves.PFG, FUN = "sum")
  #   } else {
  #     mat.PFG.agg = aggregate(abund ~ site + PFG, data = releves.PFG, FUN = "sum")
  #     mat.PFG.agg$strata = "all"
  #   }
  # } else {
  #   if (perStrata == TRUE) {
  #     mat.PFG.agg = aggregate(abund ~ site + PFG + strata + code.habitat, data = releves.PFG, FUN = "sum")
  #   } else {
  #     mat.PFG.agg = aggregate(abund ~ site + PFG + code.habitat, data = releves.PFG, FUN = "sum")
  #     mat.PFG.agg$strata = "all"
  #   }
  # }

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
  # 
  # 
  # # 2. Cast the df
  # ################
  # 
  # #transfo into factor to be sure to create all the combination when doing "dcast"
  # mat.PFG.agg$PFG = as.factor(mat.PFG.agg$PFG)
  # mat.PFG.agg$strata = as.factor(mat.PFG.agg$strata)
  # mat.PFG.agg = reshape2::dcast(mat.PFG.agg, site ~ PFG + strata, value.var = "relative.metric", fill = 0, drop = FALSE)
  # mat.PFG.agg = merge(infos, mat.PFG.agg, by = "site")
  # 
  # 
  # # 4. Save data
  # ##############
  # 
  # write.csv(mat.PFG.agg,paste0(output.path,"/HABITAT/obs.releves.prepared.csv"),row.names = FALSE)
  
  # 5. Small adjustment in data structure
  #######################################
  
  perc = sapply(unique(mat.PFG.agg$habitat), function(x) {
    ind = which(mat.PFG.agg$habitat == x)
    return(100 * length(ind) / nrow(mat.PFG.agg))
  })
  names(perc) = unique(mat.PFG.agg$habitat)
  if (length(which(perc <= 1)) > 0) {
    mat.PFG.agg = mat.PFG.agg[-which(mat.PFG.agg$habitat %in% names(perc)[which(perc <= 1)]),]
    studied.habitat = studied.habitat[-which(studied.habitat$habitat %in% names(perc)[which(perc <= 1)]),]
    cat("\n > (", paste0(names(perc)[which(perc <= 1)], collapse = " / ")
        , ") represent 1% or less of the habitats in the whole area, there will be deleted for the next steps. \n")
  }
  # mat.PFG.agg$habitat = as.character(mat.PFG.agg$habitat)
  # studied.habitat$habitat = as.character(studied.habitat$habitat)
  mat.PFG.agg$habitat = as.factor(mat.PFG.agg$habitat)
  

    
  # 6.Random forest
  #################
  
  #separate the database into a training and a test part
  cat("\n > Separate the database into a training and a test part \n")
  set.seed(seed)
  
  freq = table(mat.PFG.agg$code.habitat) / nrow(mat.PFG.agg)
  no.hab = sample(names(freq), size = 0.4 * nrow(mat.PFG.agg), prob = freq, replace = TRUE)
  no.hab = table(no.hab)
  if (length(no.hab) != length(freq)) {
    stop("PROBLEM") ## TODO
  }
  training.site = foreach(hab = 1:length(no.hab), .combine = "c") %do%
    {
      sample(mat.PFG.agg$site[which(mat.PFG.agg$code.habitat == names(no.hab)[hab])]
             , size = no.hab[hab], replace = FALSE)
    }
  # training.site = sample(mat.PFG.agg$site, size = RF.param$share.training * length(mat.PFG.agg$site), replace = FALSE)
  
  
  tab.train = mat.PFG.agg[which(mat.PFG.agg$site %in% training.site), ]
  tab.test = mat.PFG.agg[-which(mat.PFG.agg$site %in% training.site), ]
  
  cat("\n Training part of the data :")
  print(table(tab.train$habitat))
  cat("\n Testing part of the data :")
  print(table(tab.test$habitat))
  
  #train the model (with correction for imbalances in sampling)
  #run optimization algo (careful : optimization over OOB...)
  mtry.perf = tuneRF(x = tab.train[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                     y = tab.train$habitat,
                     strata = tab.train$habitat,
                     sampsize = nrow(tab.train),
                     ntreeTry = RF.param$ntree,
                     stepFactor = 2,
                     improve = 0.05,
                     doBest = FALSE,
                     plot = FALSE,
                     trace = FALSE)
  #select mtry
  mtry.perf = as.data.frame(mtry.perf)
  mtry = mtry.perf$mtry[which.min(mtry.perf$OOBError)]  #the lowest n achieving minimum OOB
  
  #run real model
  model = randomForest(x = tab.train[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                       y = tab.train$habitat,
                       xtest = tab.test[, -which(colnames(tab.train) %in% c("site", "x", "y", "code.habitat", "habitat"))],
                       ytest = tab.test$habitat,
                       strata = tab.train$habitat,
                       sampsize = nrow(tab.train),
                       ntree = RF.param$ntree,
                       mtry = mtry,
                       norm.votes = TRUE,
                       keep.forest = TRUE)
  
  #analyse model performance
  conf.train = confusionMatrix(data = model$predicted, reference = tab.train$habitat)
  conf.test = confusionMatrix(data = model$test$predicted, reference = tab.test$habitat)
  
  synthesis = foreach(ii = c("train", "test"), .combine = "rbind") %do%
    {
      tab = get(paste0("conf.", ii))
      synth = data.frame(dataset = ii
                         , habitat = colnames(tab$table)
                         , sensitivity = tab$byClass[, 1]
                         , specificity = tab$byClass[, 2]
                         , weight = colSums(tab$table) / sum(colSums(tab$table)))
      #warning: prevalence is the weight of predicted habitat, not of observed habitat
      synth$TSS = round(synth$sensitivity + synth$specificity - 1, digits = 2)
      return(synth)
    }
  aggregate.TSS.training = round(sum(synthesis$weight[which(synthesis$dataset == "train")] * 
                                       synthesis$TSS[which(synthesis$dataset == "train")]), digits = 2)
  aggregate.TSS.testing = round(sum(synthesis$weight[which(synthesis$dataset == "test")] * 
                                      synthesis$TSS[which(synthesis$dataset == "test")]), digits = 2)
  
  
  # 7. Save and return output
  ###########################
  
  path.save = paste0(output.path, "/HABITAT")
  # write_rds(model, paste0(path.save, "/RF.model.rds"), compress = "none")
  write.csv(synthesis, paste0(path.save, "/RF_perf.per.hab.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.training, paste0(path.save, "/RF_aggregate.TSS_training.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.testing, paste0(path.save, "/RF_aggregate.TSS_testing.csv"), row.names = FALSE)
  
  return(list(RF = model, habitat = studied.habitat))
}

