################################################################
##' @importFrom dplyr %>% group_by
##' @importFrom stats aggregate
##' @importFrom reshape2 dcast
##' @importFrom randomForest randomForest tuneRF
##' @importFrom caret confusionMatrix
##' @importFrom readr write_rds
##' @importFrom utils read.csv write.csv
#################################################################


train_RF_habitat = function(mat.cast
                            , hab.obs.RF = NULL
                            , external.training.mask = NULL
                            , mat.hab
                            , RF.param
                            , output.path
                            # , perStrata = FALSE
                            , seed)
{
  #separate the database into a training and a test part
  cat("\n > Separate the database into a training and a test part \n")
  set.seed(seed)
  
  mat.cast$habitat = as.factor(mat.cast$habitat)
  freq = table(mat.cast$code.habitat) / nrow(mat.cast)
  no.hab = sample(names(freq), size = 0.4 * nrow(mat.cast), prob = freq, replace = TRUE)
  no.hab = table(no.hab)
  if (length(no.hab) != length(freq)) {
    stop("PROBLEM") ## TODO
  }
  training.site = foreach(hab = 1:length(no.hab), .combine = "c") %do%
    {
      sample(mat.cast$site[which(mat.cast$code.habitat == names(no.hab)[hab])]
             , size = no.hab[hab], replace = FALSE)
    }
  # training.site = sample(mat.cast$site, size = RF.param$share.training * length(mat.cast$site), replace = FALSE)
  
  
  tab.train = mat.cast[which(mat.cast$site %in% training.site), ]
  tab.test = mat.cast[-which(mat.cast$site %in% training.site), ]
  
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
  aggregate.TSS.training = .valid_getModelPerf(dataset = "train"
                                               , mod.pred = model$predicted
                                               , mod.ref = tab.train$habitat)
  
  aggregate.TSS.testing = .valid_getModelPerf(dataset = "test"
                                               , mod.pred = model$test$predicted
                                               , mod.ref = tab.test$habitat)
  
  return(list(RF = model, habitat = mat.hab))
}

