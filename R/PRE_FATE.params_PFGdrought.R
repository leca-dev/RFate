### HEADER #####################################################################
##' @title Create \emph{DROUGHT} parameter files for a \code{FATE}
##' simulation
##' 
##' @name PRE_FATE.params_PFGdrought
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' response to drought disturbance parameters for each PFG (one file for each 
##' of them) used in the drought disturbance module of \code{FATE}.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param mat.PFG.dist (\emph{optional}) \cr 
##' a \code{data.frame} with 5 columns : \cr 
##' \code{PFG}, \code{type}, \code{maturity}, \code{longevity}, 
##' \code{age_above_150cm} (see 
##' \href{PRE_FATE.params_PFGdrought.html#details}{\code{Details}})
##' @param mat.PFG.tol a \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{nameDist},
##'   \item \code{PFG},
##'   \item (\emph{\code{responseStage}, \code{breakAge}, \code{resproutAge}}), 
##'   \item \code{responseStage}, \code{killedIndiv}, \code{resproutIndiv}  
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \href{PRE_FATE.params_PFGdrought.html#details}{\code{Details}})
##' @param mat.PFG.drought a \code{data.frame} with 4 or 6 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{threshold_moderate}, \code{threshold_severe},
##'   \item \code{counter_recovery}, \code{counter_sens}, \code{counter_cum}
##'   (\emph{or \code{strategy_drou}})
##' }
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/DROUGHT/} directory to store the results
##' 
##' 
##' @details
##' 
##' The \strong{drought disturbance module} is a specific case of the 
##' \code{disturbance module}. It also allows the user to simulate spatial 
##' perturbation(s) that will impact each PFG in terms of \emph{resprouting} and 
##' \emph{mortality} at different response stages, but with specific rules to 
##' determine when the PFG is affected (see 
##' \code{\link{PRE_FATE.params_globalParameters}}). \cr \cr
##' 
##' 
##' Several parameters, given within \code{mat.PFG.dist} or \code{mat.PFG.tol}, 
##' are required for each PFG in order to set up these responses. The 
##' explanations are the same than those that can be found in 
##' \code{\link{PRE_FATE.params_PFGdisturbance}} function. Therefore, 
##' \strong{only parameters whose values or descriptions change are detailed 
##' below :}
##' 
##' \describe{
##'   \item{nameDist}{a \code{string} to choose the concerned drought 
##'   disturbance : \cr \code{immediate} or \code{delayed} \cr \cr}
##'   
##'   \item{(\emph{strategy_tol})}{a \code{string} to choose the response to 
##'   drought strategy : \cr \code{herbs_cham_1}, \code{herbs_cham_2}, 
##'   \code{herbs_cham_3}, \code{trees_1}, \code{trees_2}, \code{trees_3} 
##'   \cr \cr}
##' }
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{FATES}{ = proportion of killed and resprouting individuals \cr
##'    = for each disturbance and for each response stage \cr \cr
##'   Two methods to define these tolerances are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using 
##'     \code{strategy_tol}) : \cr
##'       \itemize{
##'         \item the values give the percentage of killed or resprouting 
##'         individuals
##'         \item with \code{1, 2, 3, 4}: response classes
##'         \item with \code{K}: killed individuals, \code{R}: resprouting 
##'         individuals \cr \cr
##'       }
##'       \describe{
##'         \item{}{\strong{\code{| ___1___ | ___2___ | ___3___ | ___4___ |}}}
##'         \item{}{\strong{\code{| _K_ _R_ | _K_ _R_ | _K_ _R_ | _K_ _R_ |}}}
##'         \item{}{\code{________________IMMEDIATE________________}}
##'         \item{herbs_cham_1}{\code{| 10\% _0_ | _0_ _0_ | _0_ _0_ | _0_ _0_ |}}
##'         \item{herbs_cham_2}{\code{| 20\% _0_ | _0_ _0_ | _0_ _0_ | 10\% _0_ |}}
##'         \item{herbs_cham_3}{\code{| 40\% _0_ | 10\% _0_ | 10\% _0_ | 20\% _0_ |}}
##'         \item{trees_1}{\code{| 10\% _0_ | _0_ _0_ | _0_ 40\% | _0_ 40\% |}}
##'         \item{trees_2}{\code{| 20\% _0_ | _0_ 10\% | _0_ 50\% | 10\% 50\% |}}
##'         \item{trees_3}{\code{| 40\% _0_ | 10\% 40\% | 10\% 80\% | 20\% 80\% |}}
##'         \item{}{\code{_________________DELAYED_________________}}
##'         \item{herbs_cham_1}{\code{| _0_ _0_ | _0_ 10\% | _0_ 10\% | _0_ 10\% |}}
##'         \item{herbs_cham_2}{\code{| _0_ _0_ | _0_ 10\% | _0_ 10\% | _0_ 10\% |}}
##'         \item{herbs_cham_3}{\code{| _0_ _0_ | _0_ 10\% | _0_ 10\% | _0_ 10\% |}}
##'         \item{trees_1}{\code{| _0_ _0_ | _0_ 10\% | _0_ 40\% | _0_ 40\% |}}
##'         \item{trees_2}{\code{| 10\% _0_ | _0_ 40\% | _0_ 40\% | _0_ 40\% |}}
##'         \item{trees_3}{\code{| 20\% _0_ | 10\% 40\% | 10\% 50\% | 10\% 50\% |}}
##'       }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{responseStage}, 
##'       \code{killedIndiv} and \code{resproutIndiv} columns, if provided \cr
##'       The \code{PFG} column can contain either the life form (\code{H}, 
##'       \code{C} or \code{P}) or the PFG name. Both methods can be combined 
##'       (but are applied in the order given by the \code{PFG} column). \cr \cr
##'       }
##'   }
##'   }
##' }
##' 
##' 
##' Supplementary parameters related to drought, given within 
##' \code{mat.PFG.drought}, are required for each PFG :
##' 
##' \describe{
##'   \item{threshold_moderate}{a value corresponding to the threshold below 
##'   which the PFG will experience moderate drought (on the same scale than 
##'   \code{threshold_severe} and the map given with the \code{DROUGHT_MASK} 
##'   flag in \code{\link{PRE_FATE.params_globalParameters}})}
##'   \item{threshold_severe}{a value corresponding to the threshold below 
##'   which the PFG will experience severe drought (on the same scale than 
##'   \code{threshold_moderate} and the map given with the \code{DROUGHT_MASK} 
##'   flag in \code{\link{PRE_FATE.params_globalParameters}}). It should be 
##'   inferior or equal to \code{threshold_moderate}. \cr \cr}
##'   \item{counter_recovery}{an \code{integer} corresponding to the number of 
##'   years removed from the PFG counter of cumulated consecutive years of 
##'   drought events, during non-drought years}
##'   \item{counter_sens}{an \code{integer} corresponding to the number of 
##'   consecutive years of drought the PFG must experience before suffering 
##'   severe effects due to a severe drought (\emph{sensitivity to severe 
##'   drought})}
##'   \item{counter_cum}{an \code{integer} corresponding to the number of 
##'   consecutive years of drought the PFG must experience before any subsequent 
##'   drought event start having severe effects (\emph{cumulative drought 
##'   response}). It should be superior or equal to \code{counter_sens}.}
##'   \item{(\emph{strategy_drou})}{a \code{string} to choose the "counter" 
##'   strategy : \cr \code{herbs}, \code{chamaephytes}, \code{trees_shrubs} 
##'   \cr \cr}
##' }
##' 
##' These values will allow to define a set of characteristics for each PFG :
##' \describe{
##'   \item{sensitivity to values}{with the \strong{THRESHOLD_MODERATE} and 
##'   \strong{THRESHOLD_SEVERE} parameters}
##'   \item{sensitivity through time}{with the \strong{COUNTER_RECOVERY}, 
##'   \strong{COUNTER_SENS} and \strong{COUNTER_CUM} parameters}
##' }
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/DROUGHT/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{BREAK_AGE}{ages at which the PFG changes of response stage 
##'   \emph{(in years)}}
##'   \item{RESPR_AGE}{resprouting age table (in a single row) \cr
##'   This is a vector of \code{no.DIST (=2) * no.responseStages} numbers 
##'   corresponding \cr to the age at which the PFG can be rejuvenated 
##'   (younger than the actual one) :
##'   \itemize{
##'     \item at different response stages \emph{(\code{RS})}
##'     \item for each disturbance \emph{(\code{DI})}.
##'   }
##'   These parameters should be given in this order (e.g. with 3 response 
##'   stages) : \cr \code{DI1_RS1, DI1_RS2, DI1_RS3, DI2_RS1...} \emph{(in 
##'   years)}. 
##'   }
##'   \item{FATES}{disturbance response table (in a single row) \cr
##'   This is a vector of \code{no.DIST (=2) * no.responseStages * 2} numbers 
##'   corresponding \cr to the proportion of individuals :
##'   \itemize{
##'     \item that will be killed \emph{(\code{Ki})} or resprout 
##'     \emph{(\code{Re})}
##'     \item at different response stages \emph{(\code{RS})}
##'     \item for each disturbance \emph{(\code{DI})}.
##'   }
##'   These parameters should be given in this order (e.g. with 3 response 
##'   stages) : \cr \code{DI1_RS1_Ki, DI1_RS1_Re, DI1_RS2_Ki, DI1_RS2_Re, 
##'   DI1_RS3_Ki, DI1_RS3_Re, DI2_RS1_Ki...}
##'   \cr \emph{(from \code{0} to \code{10}, corresponding to 0 to 100\%)}. 
##'   }
##'   \item{PROP_KILLED}{proportion of propagules killed by each disturbance \cr
##'   \emph{(from \code{0} to \code{10}, corresponding to 0 to 100\%)}}
##'   \item{ACTIVATED_SEED}{proportion of seeds activated by each disturbance \cr
##'   \emph{(from \code{0} to \code{10}, corresponding to 0 to 100\%)} \cr \cr}
##'   \item{THRESHOLD_MOD}{threshold below which the PFG will experience 
##'   moderate drought \cr \emph{(same unit as that of the map given with the 
##'   \code{DROUGHT_MASK} flag in 
##'   \code{\link{PRE_FATE.params_globalParameters}})}}
##'   \item{THRESHOLD_SEV}{threshold below which the PFG will experience 
##'   severe drought \cr \emph{(same unit as that of the map given with the 
##'   \code{DROUGHT_MASK} flag in 
##'   \code{\link{PRE_FATE.params_globalParameters}})}}
##'   \item{COUNTER_RECOVERY}{number of years removed from the PFG counter of 
##'   cumulated consecutive years of drought events, during non-drought years}
##'   \item{COUNTER_SENS}{number of consecutive years of drought the PFG must 
##'   experience before suffering severe effects due to a severe drought \cr 
##'   (\emph{sensitivity to severe drought})}
##'   \item{COUNTER_CUM}{number of consecutive years of drought the PFG must 
##'   experience before any subsequent drought event start having severe effects 
##'   \cr (\emph{cumulative drought response}) \cr \cr}
##' }
##' 
##' A \code{DROUGHT_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \code{name.simulation/DATA/PFGS/} directory.  
##'
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \code{name.simulation/DATA/PFGS/DROUGHT/opt.folder.name/}.
##' 
##' 
##' 
##' @keywords FATE, simulation, disturbance, killing, resprouting
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}},
##' \code{\link{PRE_FATE.params_PFGdisturbance}}
##' 
##' 
##' @examples
##' 
##' 
##' ## ----------------------------------------------------------------------------------------- ##
##'                                     
##' ## Load example data
##'                                                            
##' 
##' @export
##' 
##' @importFrom utils write.table
##' @importFrom foreach foreach %do%
##'
## END OF HEADER ###############################################################



PRE_FATE.params_PFGdrought = function(
  name.simulation
  , mat.PFG.dist = NULL
  , mat.PFG.tol
  , mat.PFG.drought
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/DROUGHT/")
  
  ## CHECK parameter mat.PFG.dist
  if (!is.null(mat.PFG.dist))
  {
    if (.testParam_notDf(mat.PFG.dist))
    {
      .stopMessage_beDataframe("mat.PFG.dist")
    } else
    {
      if (nrow(mat.PFG.dist) == 0 || ncol(mat.PFG.dist) != 5)
      {
        .stopMessage_numRowCol("mat.PFG.dist", c("PFG", "type", "maturity", "longevity", "age_above_150cm"))
      } else if (.testParam_notColnames(mat.PFG.dist, c("PFG", "type", "maturity", "longevity", "age_above_150cm")))
      {
        .stopMessage_columnNames("mat.PFG.dist", c("PFG", "type", "maturity", "longevity", "age_above_150cm"))
      }
      mat.PFG.dist$PFG = as.character(mat.PFG.dist$PFG)
      .testParam_samevalues.m("mat.PFG.dist$PFG", mat.PFG.dist$PFG)
      .testParam_notChar.m("mat.PFG.dist$PFG", mat.PFG.dist$PFG)
      mat.PFG.dist$type = as.character(mat.PFG.dist$type)
      .testParam_notInValues.m("mat.PFG.dist$type", mat.PFG.dist$type, c("H", "C", "P"))
      .testParam_notNum.m("mat.PFG.dist$maturity", mat.PFG.dist$maturity)
      .testParam_NAvalues.m("mat.PFG.dist$maturity", mat.PFG.dist$maturity)
      .testParam_notNum.m("mat.PFG.dist$longevity", mat.PFG.dist$longevity)
      .testParam_NAvalues.m("mat.PFG.dist$longevity", mat.PFG.dist$longevity)
      .testParam_notNum.m("mat.PFG.dist$age_above_150cm", mat.PFG.dist$age_above_150cm)
      .testParam_NAvalues.m("mat.PFG.dist$age_above_150cm", mat.PFG.dist$age_above_150cm)
      if (sum(mat.PFG.dist$maturity > mat.PFG.dist$longevity) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.dist$maturity` must contain "
                    , "values equal or inferior to `mat.PFG.dist$longevity`"))
      }
      mat.PFG.dist$longevity = mat.PFG.dist$longevity - 1
    }
  }
  ## CHECK parameter mat.PFG.tol
  if (.testParam_notDf(mat.PFG.tol))
  {
    .stopMessage_beDataframe("mat.PFG.tol")
  } else
  {
    if (nrow(mat.PFG.tol) == 0 || !(ncol(mat.PFG.tol) %in% c(3, 5, 6, 7)))
    {
      .stopMessage_numRowCol("mat.PFG.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                              , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.tol))
                          , "3" = .testParam_notColnames(mat.PFG.tol, c("nameDist", "PFG", "strategy_tol"))
                          , "5" = .testParam_notColnames(mat.PFG.tol, c("nameDist", "PFG", "responseStage"
                                                                        , "killedIndiv", "resproutIndiv"))
                          , "6" = .testParam_notColnames(mat.PFG.tol, c("nameDist", "PFG", "responseStage"
                                                                        , "breakAge", "resproutAge"
                                                                        , "strategy_tol"))
                          , "7" = .testParam_notColnames(mat.PFG.tol, c("nameDist", "PFG", "responseStage"
                                                                        , "breakAge", "resproutAge"
                                                                        , "killedIndiv", "resproutIndiv"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.tol", c("nameDist", "PFG", "responseStage", "(breakAge)", "(resproutAge)"
                                                  , "killedIndiv", "resproutIndiv", "(strategy_tol)"))
      }
    }
    mat.PFG.tol$nameDist = as.character(mat.PFG.tol$nameDist)
    .testParam_notInValues.m("mat.PFG.tol$nameDist", mat.PFG.tol$nameDist, c("immediate", "delayed"))
    mat.PFG.tol$PFG = as.character(mat.PFG.tol$PFG)
    .testParam_notChar.m("mat.PFG.tol$PFG", mat.PFG.tol$PFG)
    if (!is.null(mat.PFG.dist))
    {
      .testParam_notInValues.m("mat.PFG.tol$PFG", mat.PFG.tol$PFG, c("H", "C", "P", mat.PFG.dist$PFG))
    }
    if (sum(colnames(mat.PFG.tol) == "responseStage") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.tol$responseStage", mat.PFG.tol$responseStage)
      .testParam_notInValues.m("mat.PFG.tol$responseStage", mat.PFG.tol$responseStage, 0:10)
      if (sum(colnames(mat.PFG.tol) == "breakAge") == 1)
      {
        .testParam_notNum.m("mat.PFG.tol$breakAge", mat.PFG.tol$breakAge)
        .testParam_NAvalues.m("mat.PFG.tol$breakAge", mat.PFG.tol$breakAge)
        .testParam_notNum.m("mat.PFG.tol$resproutAge", mat.PFG.tol$resproutAge)
        .testParam_NAvalues.m("mat.PFG.tol$resproutAge", mat.PFG.tol$resproutAge)
      }
      if (sum(colnames(mat.PFG.tol) == "killedIndiv") == 1)
      {
        .testParam_NAvalues.m("mat.PFG.tol$killedIndiv", mat.PFG.tol$killedIndiv)
        .testParam_notInValues.m("mat.PFG.tol$killedIndiv", mat.PFG.tol$killedIndiv, 0:10)
        .testParam_NAvalues.m("mat.PFG.tol$resproutIndiv", mat.PFG.tol$resproutIndiv)
        .testParam_notInValues.m("mat.PFG.tol$resproutIndiv", mat.PFG.tol$resproutIndiv, 0:10)
      }
    }
    if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
    {
      mat.PFG.tol$strategy_tol = as.character(mat.PFG.tol$strategy_tol)
      .testParam_notInValues.m("mat.PFG.tol$strategy_tol", mat.PFG.tol$strategy_tol
                               , c("herbs_cham_1", "herbs_cham_2", "herbs_cham_3"
                                   , "trees_1", "trees_2", "trees_3"))
    }
  }
  ## CHECK parameter mat.PFG.drought
  if (.testParam_notDf(mat.PFG.drought))
  {
    .stopMessage_beDataframe("mat.PFG.drought")
  } else
  {
    if (nrow(mat.PFG.drought) == 0 || !(ncol(mat.PFG.drought) %in% c(4, 6)))
    {
      .stopMessage_numRowCol("mat.PFG.drought", c("PFG", "threshold_moderate"
                                                  , "threshold_severe", "counter_recovery"
                                                  , "counter_sens", "counter_cum"
                                                  , "(strategy_drou)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.drought))
                          , "4" = .testParam_notColnames(mat.PFG.drought
                                                         , c("PFG", "threshold_moderate"
                                                             , "threshold_severe","strategy_drou"))
                          , "6" = .testParam_notColnames(mat.PFG.drought
                                                         , c("PFG", "threshold_moderate"
                                                             , "threshold_severe", "counter_recovery"
                                                             , "counter_sens", "counter_cum"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.drought", c("PFG", "threshold_moderate"
                                                      , "threshold_severe", "counter_recovery"
                                                      , "counter_sens", "counter_cum"
                                                      , "(strategy_drou)"))
      }
    }
    mat.PFG.drought$PFG = as.character(mat.PFG.drought$PFG)
    .testParam_notChar.m("mat.PFG.drought$PFG", mat.PFG.drought$PFG)
    .testParam_notNum.m("mat.PFG.drought$threshold_moderate", mat.PFG.drought$threshold_moderate)
    .testParam_NAvalues.m("mat.PFG.drought$threshold_moderate", mat.PFG.drought$threshold_moderate)
    .testParam_notNum.m("mat.PFG.drought$threshold_severe", mat.PFG.drought$threshold_severe)
    .testParam_NAvalues.m("mat.PFG.drought$threshold_severe", mat.PFG.drought$threshold_severe)
    if (sum(mat.PFG.drought$threshold_severe > mat.PFG.drought$threshold_moderate) > 0){
      stop(paste0("Wrong type of data!\n `mat.PFG.drought$threshold_severe` must contain "
                  , "values equal or inferior to `mat.PFG.drought$threshold_moderate`"))
    }
    if (ncol(mat.PFG.drought) == 6)
    {
      .testParam_NAvalues.m("mat.PFG.drought$counter_recovery", mat.PFG.drought$counter_recovery)
      .testParam_notInteger.m("mat.PFG.drought$counter_recovery", mat.PFG.drought$counter_recovery)
      .testParam_NAvalues.m("mat.PFG.drought$counter_sens", mat.PFG.drought$counter_sens)
      .testParam_notInteger.m("mat.PFG.drought$counter_sens", mat.PFG.drought$counter_sens)
      .testParam_NAvalues.m("mat.PFG.drought$counter_cum", mat.PFG.drought$counter_cum)
      .testParam_notInteger.m("mat.PFG.drought$counter_cum", mat.PFG.drought$counter_cum)
      if (sum(mat.PFG.drought$counter_sens > mat.PFG.drought$counter_cum) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.drought$counter_sens` must contain "
                    , "values equal or inferior to `mat.PFG.drought$counter_cum`"))
      }
    }
    if (sum(colnames(mat.PFG.drought) == "strategy_drou") == 1)
    {
      mat.PFG.drought$strategy_drou = as.character(mat.PFG.drought$strategy_drou)
      .testParam_notInValues.m("mat.PFG.drought$strategy_drou", mat.PFG.drought$strategy_drou
                               , c("herbs", "chamaephytes", "trees_shrubs"))
    }
  }
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/PFGS/DROUGHT/"))

    
  #############################################################################
  
  ## GET informations
  NAME = unique(as.character(mat.PFG.tol$PFG))
  no.PFG = length(NAME)
  DIST_NAME = c("immediate", "delayed")
  no.DIST = length(DIST_NAME)
  no.STAGES = 4
  if (sum(colnames(mat.PFG.tol) == "responseStage") == 1){
    no.STAGES = max(mat.PFG.tol$responseStage)
  }
  if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1){
    no.STAGES = 4
  }
  
  cat("\n ---------- INFORMATION : DROUGHT \n")
  cat("\n  Number of disturbances : ", no.DIST)
  cat("\n  Names of disturbances : ", DIST_NAME)
  cat("\n  Number of response stages : ", no.STAGES)
  cat("\n")
  
  
  
  #############################################################################
  
  ## GET drought informations
  THRESHOLD_MOD = THRESHOLD_SEV = rep(0, no.PFG)
  for (i in 1:no.PFG)
  {
    ind.i = which(mat.PFG.drought$PFG == NAME[i])
    THRESHOLD_MOD[i] = mat.PFG.drought$threshold_moderate[ind.i]
    THRESHOLD_SEV[i] = mat.PFG.drought$threshold_severe[ind.i]
  }
  
  ## GET drought informations
  COUNTER_CUM = COUNTER_SENS = COUNTER_RECOVERY = rep(0, no.PFG)
  
  if (sum(colnames(mat.PFG.drought) == "strategy_drou") == 1)
  {
    for (i in 1:no.PFG){
      ind.i = which(mat.PFG.drought$PFG == NAME[i])
      COUNTER_RECOVERY[i] = switch(mat.PFG.drought$strategy_drou[ind.i]
                                   , herbs = 2
                                   , chamaephytes = 2
                                   , trees_shrubs = 1 )
      COUNTER_SENS[i] = switch(mat.PFG.drought$strategy_drou[ind.i]
                               , herbs = 1
                               , chamaephytes = 2
                               , trees_shrubs = 3 )
      COUNTER_CUM[i] = switch(mat.PFG.drought$strategy_drou[ind.i]
                                   , herbs = 2
                                   , chamaephytes = 3
                                   , trees_shrubs = 5 )
    }
  } else
  {
    for (i in 1:no.PFG)
    {
      ind.i = which(mat.PFG.drought$PFG == NAME[i])
      COUNTER_RECOVERY[i] = mat.PFG.drought$counter_recovery[ind.i]
      COUNTER_SENS[i] = mat.PFG.drought$counter_sens[ind.i]
      COUNTER_CUM[i] = mat.PFG.drought$counter_cum[ind.i]
    }
  }
  
  
  #############################################################################
  
  ## GET CHANGE between RESPONSE STAGES AGES
  ##   = response classes depend on the age of the PFG
  ## Annuals and biennials won't change their response to disturbances
  BREAK_AGE = matrix(0, nrow = no.DIST * (no.STAGES - 1), ncol = no.PFG)
  
  if (!is.null(mat.PFG.dist) && sum(colnames(mat.PFG.dist) == "type") == 1)
  {
    ind.H = which(mat.PFG.dist$type == "H")
    ind.CP = which(mat.PFG.dist$type != "H")
    
    brk_ages_tmp = matrix(0, nrow = no.STAGES - 1, ncol = no.PFG)
    
    ## A12 = for herbaceous : maturity - 2 / for chamaephyte and phanerophyte : 1
    brk_ages_tmp[1, ] = ifelse(mat.PFG.dist$type == "H"
                               , apply(cbind(mat.PFG.dist$maturity - 2, 0), 1, max)
                               , 1)
    if (length(ind.H) > 0)
    {
      ## A23 = min(CHANG_STR_AGES_to_str_3, maturity)
      brk_ages_tmp[2, ind.H] = mat.PFG.dist$maturity[ind.H]
      ## A34 = min(CHANG_STR_AGES_to_str_3, longevity - 2)
      brk_ages_tmp[3, ind.H] = mat.PFG.dist$longevity[ind.H] - 2
    }
    
    if (length(ind.CP) > 0)
    {
      ## A23 = min(CHANG_STR_AGES_to_str_3, maturity)
      brk_ages_tmp[2, ind.CP] = apply(mat.PFG.dist[ind.CP, c("maturity", "age_above_150cm")]
                                      , 1, min)
      ## A34 = min(CHANG_STR_AGES_to_str_3, longevity - 2)
      brk_ages_tmp[3, ind.CP] = apply(cbind(mat.PFG.dist$longevity[ind.CP] - 2
                                            , mat.PFG.dist[ind.CP, "age_above_150cm"])
                                      , 1, min)
    }
    
    ## ANNUALS / BIENNIALS : die after the first or second year, 
    ##   = so not affected differently according to life stages
    ##   = no senescence (never pass to last age class)
    brk_ages_tmp[, which(mat.PFG.dist$longevity <= 2)] = 1
    brk_ages_tmp[3, which(mat.PFG.dist$longevity == 2)] = 2
    
    
    ## SAME FOR ALL DISTURBANCE
    for (i in 1:no.DIST)
    {
      ind_1 = 1 + (i - 1) * (no.STAGES - 1)
      ind_2 = (no.STAGES - 1) + (i - 1) * (no.STAGES - 1)
      BREAK_AGE[ind_1:ind_2, ] = brk_ages_tmp
    }
  } else if (sum(colnames(mat.PFG.tol) == "breakAge") == 1)
  {
    tmp = mat.PFG.tol[, c("nameDist", "PFG", "responseStage", "breakAge")]
    tmp = tmp[which(tmp$responseStage > 1), , drop = FALSE]
    if (nrow(tmp) > 0)
    {
      for (i in 1:nrow(tmp))
      {
        ind.pfg = which(NAME == tmp$PFG[i])
        ind.dist = which(DIST_NAME == tmp$nameDist[i])
        ind.stage = (tmp$responseStage[i] - 1) + (ind.dist - 1) * (no.STAGES - 1)
        BREAK_AGE[ind.stage, ind.pfg] = tmp$breakAge[i]
      }
    } else
    {
      warning("Missing data! The `BREAK_AGE` parameter has not been set. Please check.")
    }
  } else
  {
    warning("Missing data! The `BREAK_AGE` parameter has not been set. Please check.")
  }
  
  
  #############################################################################
  
  ## GET RESPROUTING AGES
  ##   = living ones are rejuvenated at a younger age
  ##   = does not impact dead individuals
  RESPR_AGE = matrix(0, nrow = no.DIST * no.STAGES, ncol = no.PFG)
  
  if (!is.null(mat.PFG.dist) && sum(colnames(mat.PFG.dist) == "type") == 1)
  {
    ## stage 1 : too young to resprout
    RESPR_AGE[seq(1, nrow(RESPR_AGE), by = no.STAGES), ] = 0
    ## stage 2 : too young to resprout
    RESPR_AGE[seq(2, nrow(RESPR_AGE), by = no.STAGES), ] = 0
    ## stage 3 : juveniles are not affected, matures resprout at maturity - 2
    val.tmp = apply(cbind(apply(cbind(mat.PFG.dist$maturity - 2, 0), 1, max)
                          , mat.PFG.dist$age_above_150cm), 1, min)
    RESPR_AGE[seq(3, nrow(RESPR_AGE), by = no.STAGES), ] = rep(val.tmp, each = no.DIST)
    ## stage 4 : resprout at longevity - 2
    RESPR_AGE[seq(4, nrow(RESPR_AGE), by = no.STAGES), ] = rep(mat.PFG.dist$longevity - 2
                                                               , each = no.DIST)
    
    ## ANNUALS and BIENNIALS
    ##   = always start back at 0 when resprout, even in the 3rd age class
    RESPR_AGE[seq(3, nrow(RESPR_AGE), by = no.STAGES)
              , which(mat.PFG.dist$longevity <= 2)] = 0
    
  } else if (sum(colnames(mat.PFG.tol) == "resproutAge") == 1)
  {
    tmp = mat.PFG.tol[, c("nameDist", "PFG", "responseStage", "resproutAge")]
    if (nrow(tmp) > 0)
    {
      for (i in 1:nrow(tmp))
      {
        ind.pfg = which(NAME == tmp$PFG[i])
        ind.dist = which(DIST_NAME == tmp$nameDist[i])
        ind.stage = tmp$responseStage[i] + (ind.dist - 1) * no.STAGES
        RESPR_AGE[ind.stage, ind.pfg] = tmp$resproutAge[i]
      }
    }
  } else
  {
    warning("Missing data! The `RESPR_AGE` parameter has not been set. Please check.")
  }
  
  #############################################################################
  
  ## GET FATES
  ##   = proportion of killed or resprouting individuals
  ##   = for each disturbance, for each response stage : 2 values
  ##     proportion of killed individuals, and of resprouting individuals
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  FATES = matrix(0, nrow = no.DIST * no.STAGES * 2, ncol = no.PFG)
  
  if (sum(colnames(mat.PFG.tol) == "killedIndiv") == 1)
  {
    for (no.di in 1:no.DIST)
    {
      di = DIST_NAME[no.di]
      ind_dist = which(mat.PFG.tol$nameDist == di)
      
      for (no.pfg in 1:no.PFG)
      {
        pfg = NAME[no.pfg]
        ind_pfg = which(mat.PFG.tol$PFG == pfg)
        ind_lines = intersect(ind_dist, ind_pfg)
        ind_lines = ind_lines[order(mat.PFG.tol$responseStage[ind_lines])]
        
        ## KILLED INDIVIDUALS 
        ind_fates = mat.PFG.tol$responseStage[ind_lines] +
          (mat.PFG.tol$responseStage[ind_lines] - 1) +
          (no.di - 1) * 2 * no.STAGES
        
        if (pfg %in% c("H", "C", "P"))
        {
          if (!is.null(mat.PFG.dist) && sum(colnames(mat.PFG.dist) == "type") == 1)
          {
            ind_pfg = which(NAME %in% mat.PFG.dist$PFG[which(mat.PFG.dist$type == pfg)])
            if (length(ind_pfg) > 0)
            {
              FATES[ind_fates, ind_pfg] = mat.PFG.tol[ind_lines, "killedIndiv"]
            } else
            {
              warning(paste0("Missing data! Treating the `killedIndiv `column : "
                             , "no PFG correspond to the type `"
                             , pfg
                             , "` given within the `PFG` column of `mat.PFG.tol`. "
                             , "Please check."))
            }
          } else
          {
            warning(paste0("Missing data! Treating the `resproutIndiv `column : "
                           , "the `type` column of the `mat.PFG.dist` parameter "
                           , "has not been set. Please check."))
          }
        } else if (pfg %in% NAME)
        {
          FATES[ind_fates, no.pfg] = mat.PFG.tol[ind_lines, "killedIndiv"]
        }
        
        ## RESPROUTING INDIVIDUALS
        ind_fates = ind_fates + 1
        
        if (pfg %in% c("H", "C", "P"))
        {
          if (!is.null(mat.PFG.dist) && sum(colnames(mat.PFG.dist) == "type") == 1)
          {
            ind_pfg = which(NAME %in% mat.PFG.dist$PFG[which(mat.PFG.dist$type == pfg)])
            if (length(ind_pfg) > 0)
            {
              FATES[ind_fates, ind_pfg] = mat.PFG.tol[ind_lines, "resproutIndiv"]
            } else
            {
              warning(paste0("Missing data! Treating the `resproutIndiv `column : "
                             , "no PFG correspond to the type `"
                             , pfg
                             , "` given within the `PFG` column of `mat.PFG.tol`. "
                             , "Please check."))
            }
          } else
          {
            warning(paste0("Missing data! Treating the `resproutIndiv `column : "
                           , "the `type` column of the `mat.PFG.dist` parameter "
                           , "has not been set. Please check."))
          }
        } else if (pfg %in% NAME)
        {
          FATES[ind_fates, no.pfg] = mat.PFG.tol[ind_lines, "resproutIndiv"]
        }
      }
    }
  } else if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
  {
    for (i in 1:no.PFG){
      FATES[, i] = switch(mat.PFG.tol$strategy_tol[i]
                          , herbs_cham_1 = c(1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1)
                          , herbs_cham_2 = c(2,0,0,0,0,0,1,0,0,0,0,1,0,1,0,1)
                          , herbs_cham_3 = c(4,0,1,0,1,0,2,0,0,0,0,1,0,1,0,1)
                          , trees_1 = c(1,0,0,0,0,4,0,4,0,0,0,1,0,4,0,4)
                          , trees_2 = c(2,0,0,1,0,5,1,5,1,0,0,4,0,4,0,4)
                          , trees_3 = c(4,0,1,4,1,8,2,8,2,0,1,4,1,5,1,5)
      )
    }
  }
  
  #############################################################################
  
  ## GET PROPORTION OF KILLED PROPAGULES
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  ## 0 for all PFG and disturbances
  PROP_KILLED = matrix(0, nrow = no.DIST, ncol = no.PFG)
  
  
  #############################################################################
  
  ## GET END OF SEED DORMANCY : % of seeds activated by the perturbation
  ## 11 levels : 0 = 0 %
  ##             1 = 10 %
  ##             2 = 20 %
  ##             3 = 30 %
  ##             4 = 40 %
  ##             5 = 50 %
  ##             6 = 60 %
  ##             7 = 70 %
  ##             8 = 80 %
  ##             9 = 90 %
  ##             10 = 100 %
  ## 0 for all PFG and disturbances
  ACTIVATED_SEED = matrix(0, nrow = no.DIST, ncol = no.PFG)
  
  
  #############################################################################
  
  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "BREAK_AGE"
                            , "RESPR_AGE"
                            , "FATES"
                            , "PROP_KILLED"
                            , "ACTIVATED_SEED"
                            , "THRESHOLD_MOD"
                            , "THRESHOLD_SEV"
                            , "COUNTER_RECOVERY"
                            , "COUNTER_SENS"
                            , "COUNTER_CUM")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = do.call(rbind, params.list)
  rownames(params.csv) = c("NAME"
                           , paste0("BREAK_AGE_"
                                    , rep(DIST_NAME, each = no.STAGES-1)
                                    , paste0("_", 1:(no.STAGES-1), "to", 2:no.STAGES))
                           , paste0("RESPR_AGE_"
                                    , rep(DIST_NAME, each = no.STAGES)
                                    , "_", 1:no.STAGES)
                           , paste0("FATES_"
                                    , rep(DIST_NAME, each = no.STAGES * 2)
                                    , "_"
                                    , paste0(rep(1:no.STAGES, each = 2)
                                             , c("_kill","_respr")))
                           , paste0("PROP_KILLED_", DIST_NAME)
                           , paste0("ACTIVATED_SEED_", DIST_NAME)
                           , "THRESHOLD_MOD"
                           , "THRESHOLD_SEV"
                           , "COUNTER_RECOVERY"
                           , "COUNTER_SENS"
                           , "COUNTER_CUM")
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == ""
                                       , ""
                                       , sub("/$", "_", opt.folder.name))
                              , "DROUGHT_COMPLETE_TABLE.csv")
              , row.names = TRUE
              , col.names = FALSE)
  
  #############################################################################
  
  params.list = lapply(1:no.PFG, function(x) {
    lapply(names.params.list.sub, function(y) {
      val = get(y)
      if (is.null(nrow(val))){
        val = val[x]
      } else {
        val = val[, x]
      }
      return(val)
    })
  })
  
  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation
                                       , "/DATA/PFGS/DROUGHT/"
                                       , opt.folder.name
                                       , "DROUGHT_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG drought parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

