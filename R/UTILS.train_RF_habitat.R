### HEADER #####################################################################
##'
##' @title Create a random forest algorithm trained on CBNA data.
##' 
##' @name train.RF.habitat
##' 
##' @author Matthieu Combaud, Maxime Delprat
##' 
##' @description This script is designed to produce a random forest model
##' trained on observed PFG abundance, sites releves and a map of observed
##' habitat.
##' 
##' @param releves.PFG a data frame with Braund-Blanquet abundance at each site
##' and each PFG and strata.
##' @param releves.sites a data frame with coordinates and a description of
##' the habitat associated with the dominant species of each site in the 
##' studied map.
##' @param hab.obs a raster map of the observed habitat in the
##' extended studied area.
##' @param external.training.mask default \code{NULL}. (optional) Keep only
##' releves data in a specific area.
##' @param studied.habitat If \code{NULL}, the function will
##' take into account of all habitats in the hab.obs map. Otherwise, please specify 
##' in a vector the habitats that we take into account for the validation.
##' @param RF.param a list of 2 parameters for random forest model :
##' share.training defines the size of the trainig part of the data base.
##' ntree is the number of trees build by the algorithm, it allows to reduce
##' the prediction error.
##' @param output.path access path to the for the folder where output files
##' will be created.
##' @param perStrata \code{Logical}. If TRUE, the PFG abundance is defined
##' by strata in each site. If FALSE, PFG abundance is defined for all strata.
##' @param sim.version name of the simulation we want to validate.
##' 
##' @details 
##' 
##' This function transform PFG Braund-Blanquet abundance in relative abundance,
##' get habitat information from the releves map, keep only relees on interesting
##' habitat and then builds de random forest model. Finally, the function analyzes
##' the model performance with computation of confusion matrix and TSS for
##' the traning and testing sample.
##' 
##' @return 
##' 
##' 2 prepared CBNA releves files are created before the building of the random
##' forest model in a habitat validation folder.
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
##' @importFrom raster extract compareCRS levels
##' @importFrom sf st_transform st_crop st_write
##' @importFrom randomForest randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom readr write_rds
##' @importFrom utils read.csv write.csv
##' 
### END OF HEADER ##############################################################


train.RF.habitat<-function(releves.PFG
                           , releves.sites
                           , hab.obs
                           , external.training.mask = NULL
                           , studied.habitat
                           , RF.param
                           , output.path
                           , perStrata
                           , sim.version)
{
  
  cat("\n ---------- TRAIN A RANDOM FOREST MODEL ON OBSERVED DATA \n")
  
  #############################################################################
  
  ## CHECK parameter releves.PFG
  if (.testParam_notDf(releves.PFG))
  {
    .stopMessage_beDataframe("releves.PFG")
  } else
  {
    releves.PFG = as.data.frame(releves.PFG)
    if (nrow(releves.PFG) == 0 || ncol(releves.PFG) != 4)
    {
      .stopMessage_numRowCol("releves.PFG", c("sites", "PFG", "strata", "BB")) ## TODO : change colnames ?
    }
    ## TODO : condition on sites
    ## TODO : condition on strata
    ## TODO : condition on PFG
    .testParam_notInValues.m("releves.PFG$BB", releves.PFG$BB, c(NA, "NA", 0, "+", "r", 1:5))
  }
  ## CHECK parameter releves.sites
  if (.testParam_notDf(releves.sites))
  {
    .stopMessage_beDataframe("releves.sites")
  } else
  {
    releves.sites = as.data.frame(releves.sites)
    if (nrow(releves.sites) == 0 || ncol(releves.sites) != 3)
    {
      .stopMessage_numRowCol("releves.sites", c("sites", "x", "y")) ## TODO : change colnames ?
    }
    ## TODO : condition on site
  }
  
  
  #1. Compute relative abundance metric
  #########################################
  
  #transformation into coverage percentage
  ## TODO : Transform in real proportion (per site)
  releves.PFG$coverage<-PRE_FATE.abundBraunBlanquet(releves.PFG$BB)/100 #as a proportion, not a percentage
  
  if (perStrata == TRUE) {
    mat.PFG.agg <- aggregate(coverage ~ site + PFG + strata, data = releves.PFG, FUN = "sum")
  } else if (perStrata == FALSE) {
    mat.PFG.agg <- aggregate(coverage ~ site + PFG, data = releves.PFG, FUN = "sum")
    mat.PFG.agg$strata <- "A" #"A" is for "all". Important to have a single-letter code here (useful to check consistency between relevés strata and model strata)
  }
  
  #transformation into a relative metric (here relative.metric is relative coverage)
  mat.PFG.agg <-
    as.data.frame(
      mat.PFG.agg %>% group_by(site, strata) %>% mutate(relative.metric = round(prop.table(coverage), digits = 2))
    ) #rel is proportion of total pct_cov, not percentage
  mat.PFG.agg$relative.metric[is.na(mat.PFG.agg$relative.metric)] <- 0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  mat.PFG.agg$coverage<-NULL
  
  print("releve data have been transformed into a relative metric")
  
  #2. Cast the df
  #######################
  
  #transfo into factor to be sure to create all the combination when doing "dcast"
  mat.PFG.agg$PFG <- as.factor(mat.PFG.agg$PFG)
  mat.PFG.agg$strata <- as.factor(mat.PFG.agg$strata)
  mat.PFG.agg <- dcast(mat.PFG.agg, site ~ PFG + strata, value.var = "relative.metric", fill = 0, drop = FALSE)
  
  #3. Get habitat information
  ###################################
  
  #get sites coordinates
  mat.PFG.agg <- merge(releves.sites, mat.PFG.agg, by = "site") ## TODO : mettre tout directement dans releves.PFG ?
  
  #get habitat code and name
  mat.PFG.agg$code.habitat <- raster::extract(x = hab.obs, y = mat.PFG.agg[, c("x", "y")])
  mat.PFG.agg = mat.PFG.agg[which(!is.na(mat.PFG.agg$code.habitat)), ]
  if (nrow(mat.PFG.agg) == 0) {
    ## TODO : add stop message
  }
  
  #correspondance habitat code/habitat name
  ## ATTENTION ! il faut que la couche de noms du raster existe, et qu'elle s'appelle habitat...
  ## TODO : soit donner en paramètre un vecteur avec les noms d'habitat, soit les données dans releves.PFG...
  table.habitat.releve <- levels(hab.obs)[[1]]
  mat.PFG.agg<-merge(mat.PFG.agg, table.habitat.releve[, c("ID", "habitat")], by.x = "code.habitat", by.y = "ID") 
  
  #(optional) keep only releves data in a specific area
  if (!is.null(external.training.mask)) {
    # if (compareCRS(mat.PFG.agg, external.training.mask) == FALSE) {
    #   #as this stage it is not a problem to transform crs(mat.PFG.agg) since we have no more merge to do (we have already extracted habitat info from the map)
    #   mat.PFG.agg <- st_transform(x = mat.PFG.agg, crs = crs(external.training.mask))
    # }
    # mat.PFG.agg <- st_crop(x = mat.PFG.agg, y = external.training.mask)
    
    val.inMask = raster::extract(x = external.training.mask, y = mat.PFG.agg[, c("x", "y")])
    mat.PFG.agg = mat.PFG.agg[which(!is.na(val.inMask)), ]
    print("'releve' map has been cropped to match 'external.training.mask'.")
  }
  
  
  # 4. Keep only releve on interesting habitat
  ###################################################"
  
  if (!is.null(studied.habitat)) {
    mat.PFG.agg <- mat.PFG.agg[which(mat.PFG.agg$habitat %in% studied.habitat), ] #filter non interesting habitat + NA
    if (nrow(mat.PFG.agg) == 0) {
      ## TODO : add stop message
    }
  }
  print(cat("habitat classes used in the RF algo: ",unique(mat.PFG.agg$habitat),"\n",sep="\t"))
  
  # 5. Save data
  #####################
  
  # st_write(mat.PFG.agg,paste0(output.path,"/HABITAT/", sim.version, "/releve.PFG.habitat.shp"),overwrite=T,append=F)
  write.csv(mat.PFG.agg,paste0(output.path,"/HABITAT/", sim.version, "/CBNA.releves.prepared.csv"),row.names = FALSE)
  ## TODO : remove CBNA from file name
  
  # 6. Small adjustment in data structure
  ##########################################
  
  mat.PFG.agg<-as.data.frame(mat.PFG.agg) #get rid of the spatial structure before entering the RF process
  mat.PFG.agg$habitat<-as.factor(mat.PFG.agg$habitat)
  
  # 7.Random forest
  ######################################
  
  #separate the database into a training and a test part
  set.seed(123)
  
  training.site <- sample(mat.PFG.agg$site, size = RF.param$share.training * length(mat.PFG.agg$site), replace = FALSE)
  releves.training <- mat.PFG.agg[which(mat.PFG.agg$site %in% training.site), ]
  releves.testing <- mat.PFG.agg[-which(mat.PFG.agg$site %in% training.site), ]
  
  #train the model (with correction for imbalances in sampling)
  
  #run optimization algo (careful : optimization over OOB...)
  mtry.perf <- tuneRF(x = dplyr::select(releves.training, -c(code.habitat, site, habitat, geometry)),
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
  mtry <- mtry.perf$mtry[mtry.perf$OOBError == min(mtry.perf$OOBError)][1] #the lowest n achieving minimum OOB
  
  #run real model
  model <- randomForest(x = dplyr::select(releves.training, -c(code.habitat, site, habitat, geometry)),
                        y = releves.training$habitat,
                        xtest = dplyr::select(releves.testing, -c(code.habitat, site, habitat, geometry)),
                        ytest = releves.testing$habitat,
                        strata = releves.training$habitat,
                        sampsize = nrow(releves.training),
                        ntree = RF.param$ntree,
                        mtry = mtry,
                        norm.votes = TRUE,
                        keep.forest = TRUE)
  
  #analyse model performance
  
  # Analysis on the training sample
  confusion.training <- confusionMatrix(data = model$predicted, reference = releves.training$habitat)
  synthesis.training <- data.frame(habitat = colnames(confusion.training$table)
                                   , sensitivity = confusion.training$byClass[, 1]
                                   , specificity = confusion.training$byClass[, 2]
                                   , weight = colSums(confusion.training$table) / sum(colSums(confusion.training$table)))
  #warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.training <- synthesis.training %>% mutate(TSS = round(sensitivity + specificity - 1, digits = 2))
  aggregate.TSS.training <- round(sum(synthesis.training$weight * synthesis.training$TSS), digits = 2)
  
  # Analysis on the testing sample
  confusion.testing <- confusionMatrix(data = model$test$predicted, reference = releves.testing$habitat)
  synthesis.testing<-data.frame(habitat = colnames(confusion.testing$table)
                                , sensitivity = confusion.testing$byClass[, 1]
                                , specificity = confusion.testing$byClass[, 2]
                                , weight = colSums(confusion.testing$table) / sum(colSums(confusion.testing$table)))
  #warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.testing <- synthesis.testing %>% mutate(TSS = round(sensitivity + specificity - 1, digits = 2))
  aggregate.TSS.testing <- round(sum(synthesis.testing$weight * synthesis.testing$TSS), digits = 2)
  
  
  # 8. Save and return output
  #######################################"
  
  path.save = paste0(output.path, "/HABITAT/", sim.version)
  
  write_rds(model, paste0(path.save, "/RF.model.rds"), compress = "none")
  write.csv(synthesis.training, paste0(path.save, "/RF_perf.per.hab_training.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.training, paste0(path.save, "/RF_aggregate.TSS_training.csv"), row.names = FALSE)
  write.csv(synthesis.testing, paste0(path.save, "/RF_perf.per.hab_testing.csv"), row.names = FALSE)
  write.csv(aggregate.TSS.testing, paste0(path.save, "/RF_aggregate.TSS_testing.csv"), row.names = FALSE)
  
  return(model)
}

