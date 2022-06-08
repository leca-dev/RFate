### HEADER #####################################################################
##'
##' @title Create a random forest algorithm trained on observed vegetation data
##' 
##' @name train_RF_habitat
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to produce a random forest model
##' trained on observed PFG abundance and a map of observed habitat.
##' 
##' @param releves.PFG a \code{data.frame} with at least 5 columns : \cr
##' \code{site}, \code{x}, \code{y}, \code{abund}, \code{PFG}
##' \cr (\emph{and optionally, \code{strata}}, \code{code.habitat})
##' \cr (see \href{train_RF_habitat#details}{\code{Details}})
##' @param hab.obs.RF (optional) default \code{NULL].
##' \cr If habitat ID is not provided in \code{releves.PFG}, a \code{raster} map of the observed habitat in the studied area.
##' @param external.training.mask (optional) default \code{NULL}. 
##' \cr a \code{raster} map for keeping releves data only in a specific area.
##' @param studied.habitat a \code{data.frame} with 2 columns : \cr
##' \code{ID} ,\code{habitat}
##' \cr (see \href{train_RF_habitat#details}{\code{Details}})
##' @param RF.param a \code{list} of 2 parameters to fit a random forest model : \cr
##' \code{share.training} defines the size of the training part of the data base. \cr
##' \code{ntree} is the number of trees build by the algorithm, it allows to reduce the prediction error.
##' @param output.path access path to the folder where output files will be created.
##' @param perStrata (\code{Logical}) default \code{FALSE}. 
##' \cr If \code{TRUE}, the PFG abundance must be defined
##' by strata in each site. If \code{FALSE}, PFG abundance must be defined for all strata.
##' 
##' @details 
##' 
##' This function transforms PFG abundance in relative abundance,
##' gets habitat information from an habitat data frame previously defined and a habitat map, 
##' keep releves on interesting habitat(s) and then builds a random forest model. Finally, 
##' the function analyzes the model performance with computation of confusion matrix and TSS between
##' the training and testing sample.
##' 
##' @return 
##' 
##' 2 prepared observed releves files are created before the building of the random
##' forest model in a folder previously defined. \cr
##' 5 more files are created at the end of the script to save the RF model and
##' the performance analyzes (confusion matrix and TSS) for the training and 
##' testing parts.
##' 
##' @export 
##' 
##' @importFrom dplyr filter %>% group_by select
##' @importFrom stats aggregate
##' @importFrom reshape2 dcast
##' @importFrom data.table setDT
##' @importFrom raster extract compareCRS levels crs
##' @importFrom sf st_transform st_crop
##' @importFrom randomForest randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom readr write_rds
##' @importFrom utils read.csv write.csv
##' @importFrom stringr str_split
##' 
### END OF HEADER ##############################################################


train_RF_habitat = function(releves.PFG
                            , hab.obs.RF = NULL
                            , external.training.mask = NULL
                            , studied.habitat
                            , RF.param
                            , output.path
                            , perStrata = FALSE
                            , seed)
{
  
  #############################################################################
  
  ## CHECK studied.habitat data frame
  if (.testParam_notDf(studied.habitat))
  {
    .stopMessage_beDataframe(studied.habitat)
  } else
  {
    studied.habitat = as.data.frame(studied.habitat)
    if (nrow(studied.habitat) == 0 || ncol(studied.habitat) != 2)
    {
      .stopMessage_numRowCol("studied.habitat", c("ID", "habitat"))
    }
    if (!is.numeric(studied.habitat$ID))
    {
      stop("Habitat ID in studied.habitat are not in the right format. Please make sure you have numeric values")
    }
    if (!is.character(studied.habitat$habitat))
    {
      stop("Habitat name in studied.habitat are not in the right format. Please make sure you have character values")
    }
  }
  ## CHECK parameter releves.PFG
  if(perStrata == TRUE & !is.null(hab.obs.RF))
  {
    if (.testParam_notDf(releves.PFG))
    {
      .stopMessage_beDataframe("releves.PFG")
    } else
    {
      releves.PFG = as.data.frame(releves.PFG)
      if (nrow(releves.PFG) == 0 || ncol(releves.PFG) != 6)
      {
        .stopMessage_numRowCol("releves.PFG", c("site", "PFG", "strata", "abund", "x", "y"))
      }
      if (!is.numeric(releves.PFG$site))
      {
        stop("Sites in releves.PFG are not in the right format. Please make sure you have numeric values")
      }
      if (!is.character(releves.PFG$strata) & !is.numeric(releves.PFG$strata))
      {
        stop("strata definition in releves.PFG is not in the right format. Please make sure you have a character or numeric values")
      }
    }
  }else if(perStrata == FALSE & !is.null(hab.obs.RF))
  {
    if (.testParam_notDf(releves.PFG))
    {
      .stopMessage_beDataframe("releves.PFG")
    } else
    {
      releves.PFG = as.data.frame(releves.PFG)
      if (nrow(releves.PFG) == 0 || ncol(releves.PFG) != 5)
      {
        .stopMessage_numRowCol("releves.PFG", c("site", "PFG", "abund", "x", "y"))
      }
      if (!is.numeric(releves.PFG$site))
      {
        stop("Sites in releves.PFG are not in the right format. Please make sure you have numeric values")
      }
    }
  }else if(perStrata == TRUE & is.null(hab.obs.RF))
  {
    if (.testParam_notDf(releves.PFG))
    {
      .stopMessage_beDataframe("releves.PFG")
    } else
    {
      releves.PFG = as.data.frame(releves.PFG)
      if (nrow(releves.PFG) == 0 || ncol(releves.PFG) != 7)
      {
        .stopMessage_numRowCol("releves.PFG", c("site", "PFG", "strata", "abund", "x", "y", "code.habitat"))
      }
      if (!is.numeric(releves.PFG$site))
      {
        stop("Sites in releves.PFG are not in the right format. Please make sure you have numeric values")
      }
      if (!is.character(releves.PFG$strata) & !is.numeric(releves.PFG$strata))
      {
        stop("strata definition in releves.PFG is not in the right format. Please make sure you have character or numeric values")
      }
      if (!is.numeric(releves.PFG$code.habitat))
      {
        stop("Code.habitat values in releves.PFG are note in the right format. Please make sure you have numeric values")
      }
    }
  }else if(perStrata == FALSE & is.null(hab.obs.RF))
  {
    if (.testParam_notDf(releves.PFG))
    {
      .stopMessage_beDataframe("releves.PFG")
    } else
    {
      releves.PFG = as.data.frame(releves.PFG)
      if (nrow(releves.PFG) == 0 || ncol(releves.PFG) != 6)
      {
        .stopMessage_numRowCol("releves.PFG", c("site", "PFG", "abund", "x", "y", "code.habitat"))
      }
      if (!is.numeric(releves.PFG$site))
      {
        stop("Sites in releves.PFG are not in the right format. Please make sure you have numeric values")
      }
      if (!is.numeric(releves.PFG$code.habitat))
      {
        stop("Code.habitat values in releves.PFG are note in the right format. Please make sure you have numeric values")
      }
    }
  }
  
  # 1. Compute relative abundance metric
  ######################################
  
  #transformation into coverage percentage
  if(!is.numeric(releves.PFG$abund)) # Braun-Blanquet abundance ## Not sure that this should be kept
  {
    releves.PFG <- filter(releves.PFG,is.element(abund,c(NA, "NA", 0, "+", "r", 1:5)))
    releves.PFG$coverage = PRE_FATE.abundBraunBlanquet(releves.PFG$abund)/100
  } else if (is.numeric(releves.PFG$abund) & max(releves.PFG$abund) == 1) #presence-absence data
  {
    releves.PFG$coverage = releves.PFG$abund
  } else if (is.numeric(releves.PFG$abund)) #absolute abundance
  {
    releves.PFG$coverage = releves.PFG$abund
  }else
  {
    stop("Abund data in releves.PFG must be Braun-Blanquet abundance, presences absence or absolute abundance values.")
  }
  
  if (perStrata == TRUE & !is.null(hab.obs.RF)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + strata, data = releves.PFG, FUN = "sum")
  } else if (perStrata == FALSE & !is.null(hab.obs.RF)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata = "A"
  } else if (perStrata == TRUE & is.null(hab.obs.RF)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + strata + code.habitat, data = releves.PFG, FUN = "sum")
  } else if (perStrata == FALSE & is.null(hab.obs.RF)) {
    mat.PFG.agg = aggregate(coverage ~ site + PFG + code.habitat, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata = "A"
  }
  
  #transformation into a relative metric (here relative.metric is relative coverage)
  mat.PFG.agg =
    as.data.frame(
      mat.PFG.agg %>% group_by(site, strata) %>% mutate(relative.metric = round(prop.table(coverage), digits = 2))
    ) #rel is proportion of total pct_cov, not percentage
  mat.PFG.agg$relative.metric[is.na(mat.PFG.agg$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  mat.PFG.agg$coverage = NULL
  
  cat("\n > Releves data have been transformed into a relative metric \n")
  
  # 2. Cast the df
  ################
  
  #transfo into factor to be sure to create all the combination when doing "dcast"
  mat.PFG.agg$PFG = as.factor(mat.PFG.agg$PFG)
  mat.PFG.agg$strata = as.factor(mat.PFG.agg$strata)
  mat.PFG.agg = reshape2::dcast(mat.PFG.agg, site ~ PFG + strata, value.var = "relative.metric", fill = 0, drop = FALSE)
  
  # 3. Get habitat information
  ############################
  
  #get habitat code and name
  coord = releves.PFG %>% group_by(site) %>% filter(!duplicated(site))
  mat.PFG.agg = merge(mat.PFG.agg, coord[,c("site","x","y")], by = "site")
  if(!is.null(hab.obs.RF))
  {
    mat.PFG.agg$code.habitat = extract(x = hab.obs.RF, y = mat.PFG.agg[,c("x", "y")])
    mat.PFG.agg = mat.PFG.agg[which(!is.na(mat.PFG.agg$code.habitat)), ]
    if (nrow(mat.PFG.agg) == 0) {
      stop("Code habitat vector is empty. Please verify values of your hab.obs.RF map")
    }
  }
  
  #correspondence habitat code/habitat name
  table.habitat.releve = studied.habitat
  mat.PFG.agg = mat.PFG.agg[which(mat.PFG.agg$code.habitat %in% studied.habitat$ID), ] # filter non interesting habitat + NA
  mat.PFG.agg = merge(mat.PFG.agg, table.habitat.releve[, c("ID", "habitat")], by.x = "code.habitat", by.y = "ID")
  
  #(optional) keep only releves data in a specific area
  if (!is.null(external.training.mask)) {
    val.inMask = extract(x = external.training.mask, y = mat.PFG.agg[, c("x", "y")])
    mat.PFG.agg = mat.PFG.agg[which(!is.na(val.inMask)), ]
    cat("\n 'releve' map has been cropped to match 'external.training.mask'. \n")
  }
  
  # 4. Save data
  ##############
  
  write.csv(mat.PFG.agg,paste0(output.path,"/HABITAT/obs.releves.prepared.csv"),row.names = FALSE)
  
  # 5. Small adjustment in data structure
  #######################################
  
  mat.PFG.agg = as.data.frame(mat.PFG.agg) #get rid of the spatial structure before entering the RF process
  mat.PFG.agg$habitat = as.factor(mat.PFG.agg$habitat)
  
  # 6.Random forest
  #################
  
  #separate the database into a training and a test part
  set.seed(seed)
  
  training.site = sample(mat.PFG.agg$site, size = RF.param$share.training * length(mat.PFG.agg$site), replace = FALSE)
  releves.training = mat.PFG.agg[which(mat.PFG.agg$site %in% training.site), ]
  releves.testing = mat.PFG.agg[-which(mat.PFG.agg$site %in% training.site), ]
  
  #train the model (with correction for imbalances in sampling)
  
  #run optimization algo (careful : optimization over OOB...)
  mtry.perf = tuneRF(x = dplyr::select(releves.training, -c(code.habitat, site, habitat, x, y)),
                     y = releves.training$habitat,
                     strata = releves.training$habitat,
                     sampsize = nrow(releves.training),
                     ntreeTry = RF.param$ntree,
                     stepFactor = 2,
                     improve = 0.05,
                     doBest = FALSE,
                     plot = FALSE,
                     trace = FALSE)
  mtry.perf = as.data.frame(mtry.perf)
  
  #select mtry
  mtry = mtry.perf$mtry[mtry.perf$OOBError == min(mtry.perf$OOBError)][1] #the lowest n achieving minimum OOB
  
  #run real model
  model = randomForest(x = dplyr::select(releves.training, -c(code.habitat, site, habitat, x, y)),
                       y = releves.training$habitat,
                       xtest = dplyr::select(releves.testing, -c(code.habitat, site, habitat, x, y)),
                       ytest = releves.testing$habitat,
                       strata = releves.training$habitat,
                       sampsize = nrow(releves.training),
                       ntree = RF.param$ntree,
                       mtry = mtry,
                       norm.votes = TRUE,
                       keep.forest = TRUE)
  
  #analyse model performance
  
  # Analysis on the training sample
  confusion.training = confusionMatrix(data = model$predicted, reference = releves.training$habitat)
  synthesis.training = data.frame(habitat = colnames(confusion.training$table)
                                  , sensitivity = confusion.training$byClass[, 1]
                                  , specificity = confusion.training$byClass[, 2]
                                  , weight = colSums(confusion.training$table) / sum(colSums(confusion.training$table)))
  #warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.training = synthesis.training %>% mutate(TSS = round(sensitivity + specificity - 1, digits = 2))
  aggregate.TSS.training = round(sum(synthesis.training$weight * synthesis.training$TSS), digits = 2)
  
  # Analysis on the testing sample
  confusion.testing = confusionMatrix(data = model$test$predicted, reference = releves.testing$habitat)
  synthesis.testing = data.frame(habitat = colnames(confusion.testing$table)
                                 , sensitivity = confusion.testing$byClass[, 1]
                                 , specificity = confusion.testing$byClass[, 2]
                                 , weight = colSums(confusion.testing$table) / sum(colSums(confusion.testing$table)))
  #warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.testing = synthesis.testing %>% mutate(TSS = round(sensitivity + specificity - 1, digits = 2))
  aggregate.TSS.testing = round(sum(synthesis.testing$weight * synthesis.testing$TSS), digits = 2)
  
  
  # 7. Save and return output
  ###########################
  
  path.save = paste0(output.path, "/HABITAT")
  
  write_rds(model, paste0(path.save, "/RF.model.rds"), compress = "none")
  write.csv(synthesis.training, paste0(path.save, "/RF_perf.per.hab_training.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.training, paste0(path.save, "/RF_aggregate.TSS_training.csv"), row.names = FALSE)
  write.csv(synthesis.testing, paste0(path.save, "/RF_perf.per.hab_testing.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.testing, paste0(path.save, "/RF_aggregate.TSS_testing.csv"), row.names = FALSE)
  
  return(model)
}

