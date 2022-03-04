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
##' @param perStrata a TRUE/FALSE vector. If TRUE, the PFG abundance is defined
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
##' @importFrom data.table dcast setDT
##' @importFrom raster extract aggregate compareCRS
##' @importFrom sf st_transform st_crop st_write
##' @importFrom randomForest randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom tidyverse write_rds
##' @importFrom utils read.csv
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
  
  #1. Compute relative abundance metric
  #########################################
  
  #identify sites with wrong BB values (ie values that cannot be converted by the PRE_FATE.abundBraunBlanquet function)
  releves.PFG<-filter(releves.PFG,is.element(BB,c(NA, "NA", 0, "+", "r", 1:5)))
  
  #transformation into coverage percentage
  releves.PFG$coverage<-PRE_FATE.abundBraunBlanquet(releves.PFG$BB)/100 #as a proportion, not a percentage
  
  if(perStrata==T){
    aggregated.releves.PFG<-aggregate(coverage~site+PFG+strata,data=releves.PFG,FUN="sum")
  }else if(perStrata==F){
    aggregated.releves.PFG<-aggregate(coverage~site+PFG,data=releves.PFG,FUN="sum")
    aggregated.releves.PFG$strata<-"A" #"A" is for "all". Important to have a single-letter code here (useful to check consistency between relevés strata and model strata)
  }
  
  #transformation into a relative metric (here relative.metric is relative coverage)
  aggregated.releves.PFG<-as.data.frame(aggregated.releves.PFG %>% group_by(site,strata) %>% mutate(relative.metric= round(prop.table(coverage),digits = 2))) #rel is proportion of total pct_cov, not percentage
  aggregated.releves.PFG$relative.metric[is.na(aggregated.releves.PFG$relative.metric)]<-0 #NA because abs==0 for some PFG, so put 0 instead of NA (maybe not necessary)
  aggregated.releves.PFG$coverage<-NULL
  
  print("releve data have been transformed into a relative metric")
  
  #2. Cast the df
  #######################
  
  #transfo into factor to be sure to create all the combination when doing "dcast"
  aggregated.releves.PFG$PFG<-as.factor(aggregated.releves.PFG$PFG)
  aggregated.releves.PFG$strata<-as.factor(aggregated.releves.PFG$strata)
  
  aggregated.releves.PFG<-dcast(setDT(aggregated.releves.PFG),site~PFG+strata,value.var=c("relative.metric"),fill=0,drop=F)
  
  #3. Get habitat information
  ###################################
  
  #get sites coordinates
  aggregated.releves.PFG<-merge(dplyr::select(releves.sites,c(site)),aggregated.releves.PFG,by="site")
  
  #get habitat code and name
  if(compareCRS(aggregated.releves.PFG,hab.obs)){
    aggregated.releves.PFG$code.habitat<-raster::extract(x=hab.obs,y=aggregated.releves.PFG)
  }else{
    aggregated.releves.PFG<-st_transform(x=aggregated.releves.PFG,crs=crs(hab.obs))
    aggregated.releves.PFG$code.habitat<-raster::extract(x=hab.obs,y=aggregated.releves.PFG)
  }
  
  #correspondance habitat code/habitat name
  table.habitat.releve<-levels(hab.obs)[[1]]
  
  aggregated.releves.PFG<-merge(aggregated.releves.PFG,dplyr::select(table.habitat.releve,c(ID,habitat)),by.x="code.habitat",by.y="ID") 
  
  #(optional) keep only releves data in a specific area
  if(!is.null(external.training.mask)){
    
    if(compareCRS(aggregated.releves.PFG,external.training.mask)==F){ #as this stage it is not a problem to transform crs(aggregated.releves.PFG) since we have no more merge to do (we have already extracted habitat info from the map)
      aggregated.releves.PFG<-st_transform(x=aggregated.releves.PFG,crs=crs(external.training.mask))
    }
    
    aggregated.releves.PFG<-st_crop(x=aggregated.releves.PFG,y=external.training.mask)
    print("'releve' map has been cropped to match 'external.training.mask'.")
  }
  
  
  # 4. Keep only releve on interesting habitat
  ###################################################"
  
  if (!is.null(studied.habitat)){
    aggregated.releves.PFG<-filter(aggregated.releves.PFG,is.element(habitat,studied.habitat)) #filter non interesting habitat + NA
    print(cat("habitat classes used in the RF algo: ",unique(aggregated.releves.PFG$habitat),"\n",sep="\t"))
  } else{
    print(cat("habitat classes used in the RF algo: ",unique(aggregated.releves.PFG$habitat),"\n",sep="\t"))
  }
  
  # 5. Save data
  #####################
  
  st_write(aggregated.releves.PFG,paste0(output.path,"/HABITAT/", sim.version, "/releve.PFG.habitat.shp"),overwrite=T,append=F)
  write.csv(aggregated.releves.PFG,paste0(output.path,"/HABITAT/", sim.version, "/CBNA.releves.prepared.csv"),row.names = F)
  
  # 6. Small adjustment in data structure
  ##########################################
  
  aggregated.releves.PFG<-as.data.frame(aggregated.releves.PFG) #get rid of the spatial structure before entering the RF process
  aggregated.releves.PFG$habitat<-as.factor(aggregated.releves.PFG$habitat)
  
  # 7.Random forest
  ######################################
  
  #separate the database into a training and a test part
  set.seed(123)
  
  training.site<-sample(aggregated.releves.PFG$site,size=RF.param$share.training*length(aggregated.releves.PFG$site),replace = F)
  releves.training<-filter(aggregated.releves.PFG,is.element(site,training.site))
  releves.testing<-filter(aggregated.releves.PFG,!is.element(site,training.site))
  
  #train the model (with correction for imbalances in sampling)
  
  #run optimization algo (careful : optimization over OOB...)
  mtry.perf<-as.data.frame(
    tuneRF(
      x=dplyr::select(releves.training,-c(code.habitat,site,habitat,geometry)),
      y=releves.training$habitat,
      strata=releves.training$habitat,
      sampsize=nrow(releves.training),
      ntreeTry=RF.param$ntree,
      stepFactor=2, improve=0.05,doBest=FALSE,plot=F,trace=F
    )
  )
  
  #select mtry
  mtry<-mtry.perf$mtry[mtry.perf$OOBError==min(mtry.perf$OOBError)][1] #the lowest n achieving minimum OOB
  
  #run real model
  model<- randomForest(
    x=dplyr::select(releves.training,-c(code.habitat,site,habitat,geometry)),
    y=releves.training$habitat,
    xtest=dplyr::select(releves.testing,-c(code.habitat,site,habitat,geometry)),
    ytest=releves.testing$habitat,
    strata=releves.training$habitat,
    sampsize=nrow(releves.training),
    ntree=RF.param$ntree,
    mtry=mtry,
    norm.votes=TRUE,
    keep.forest=TRUE
  )
  
  #analyse model performance
  
  # Analysis on the training sample
  
  confusion.training<-confusionMatrix(data=model$predicted,reference=releves.training$habitat)
  
  synthesis.training<-data.frame(habitat=colnames(confusion.training$table),sensitivity=confusion.training$byClass[,1],specificity=confusion.training$byClass[,2],weight=colSums(confusion.training$table)/sum(colSums(confusion.training$table))) #warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.training<-synthesis.training%>%mutate(TSS=round(sensitivity+specificity-1,digits=2))
  
  aggregate.TSS.training<-round(sum(synthesis.training$weight*synthesis.training$TSS),digits=2)
  
  # Analysis on the testing sample
  
  confusion.testing<-confusionMatrix(data=model$test$predicted,reference=releves.testing$habitat)
  
  synthesis.testing<-data.frame(habitat=colnames(confusion.testing$table),sensitivity=confusion.testing$byClass[,1],specificity=confusion.testing$byClass[,2],weight=colSums(confusion.testing$table)/sum(colSums(confusion.testing$table)))#warning: prevalence is the weight of predicted habitat, not of observed habitat
  synthesis.testing<-synthesis.testing%>%mutate(TSS=round(sensitivity+specificity-1,digits=2))
  
  aggregate.TSS.testing<-round(sum(synthesis.testing$weight*synthesis.testing$TSS),digits=2)
  
  
  # 8. Save and return output
  #######################################"
  
  write_rds(model,paste0(output.path,"/HABITAT/", sim.version, "/RF.model.rds"),compress="none")
  write.csv(synthesis.training,paste0(output.path,"/HABITAT/", sim.version, "/RF_perf.per.hab_training.csv"),row.names=F)
  write.csv(aggregate.TSS.training,paste0(output.path,"/HABITAT/", sim.version, "/RF_aggregate.TSS_training.csv"),row.names=F)
  write.csv(synthesis.testing,paste0(output.path,"/HABITAT/", sim.version, "/RF_perf.per.hab_testing.csv"),row.names=F)
  write.csv(aggregate.TSS.testing,paste0(output.path,"/HABITAT/", sim.version, "/RF_aggregate.TSS_testing.csv"),row.names=F)
  
  return(model)
  
}

