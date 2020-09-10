### HEADER #####################################################################
##' @title Create \emph{DISTURBANCE} parameter files for a \code{FATE}
##' simulation
##' 
##' @name PRE_FATE.params_PFGdisturbance
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' response to disturbance parameters for each PFG (one file for each of them) 
##' used in the disturbance module of \code{FATE}.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param mat.PFG.dist (\emph{optional}) \cr 
##' a \code{data.frame} with 5 columns : \cr 
##' \code{PFG}, \code{type}, \code{maturity}, \code{longevity}, 
##' \code{age_above_150cm} (see 
##' \href{PRE_FATE.params_PFGdisturbance.html#details}{\code{Details}})
##' @param mat.PFG.tol a \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{nameDist},
##'   \item \code{PFG},
##'   \item (\emph{\code{responseStage}, \code{breakAge}, \code{resproutAge}}), 
##'   \item \code{responseStage}, \code{killedIndiv}, \code{resproutIndiv}  
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \href{PRE_FATE.params_PFGdisturbance.html#details}{\code{Details}})
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/DIST/} directory to store the results
##' 
##' 
##' @details
##' 
##' The \strong{disturbance module} allows the user to simulate spatial 
##' perturbation(s) that will impact each PFG in terms of \emph{resprouting} and 
##' \emph{mortality} at different response stages. \cr \cr
##' 
##' 
##' Several parameters, given within \code{mat.PFG.dist} or \code{mat.PFG.tol}, 
##' are required for each PFG in order to set up these responses :
##' 
##' \describe{
##'   \item{PFG}{the concerned plant functional group \cr \cr}
##'   
##'   \item{(\emph{type})}{or life-form, based on Raunkier. \cr It should be 
##'   either \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} 
##'   (phanerophyte) for now}
##'   \item{(\emph{maturity})}{the age from which the PFG can reproduce}
##'   \item{(\emph{longevity})}{the maximum or average lifespan of the PFG}
##'   \item{(\emph{age_above_150cm})}{the maximum height stratum that the PFG 
##'   can reach \cr \cr}
##'   
##'   \item{nameDist}{the name of each perturbation (several can be defined at 
##'   the same time) \cr \cr}
##'   
##'   \item{(\emph{responseStage})}{an \code{integer} corresponding to the 
##'   concerned response class}
##'   \item{(\emph{breakAge})}{the age from which the PFG is associated with 
##'   this response class}
##'   \item{(\emph{resproutAge})}{the age at which the plants will grow back, 
##'   if they grow back \cr \cr}
##'   
##'   \item{responseStage}{an \code{integer} corresponding to the concerned 
##'   response class}
##'   \item{killedIndiv}{an \code{integer} between \code{0} and \code{10} 
##'   corresponding to the proportion of killed individuals}
##'   \item{resproutIndiv}{an \code{integer} between \code{0} and \code{10} 
##'   corresponding to the proportion of resprouting individuals \cr \cr}
##'   
##'   \item{(\emph{strategy_tol})}{a \code{string} to choose the response to 
##'   disturbance strategy : \cr \code{indifferent}, \code{mowing_herbs}, 
##'   \code{mowing_trees}, \code{grazing_herbs_1}, \code{grazing_herbs_2}, 
##'   \code{grazing_herbs_3}, \code{grazing_trees_1}, \code{grazing_trees_2}, 
##'   \code{grazing_trees_3} \cr \cr}
##' }
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{BREAK_AGE}{ = each PFG can respond to a disturbance in several 
##'   different ways that depend on the PFG age \cr
##'    = ages at which each PFG changes of response stage \cr \cr
##'   Two methods to define these ages are available :
##'   \itemize{
##'     \item from \strong{predefined rules} (using \code{type}, 
##'     \code{maturity}, \code{longevity}, \code{age_above_150cm}) : \cr \cr
##'     4 classes are defined that can be labelled as : \cr \strong{JustBorn 
##'     (\code{1})}, \strong{Juveniles (\code{2})}, \strong{Matures (\code{3})}, 
##'     \strong{Senescents (\code{4})} \cr \cr
##'     \tabular{rcc}{
##'        \tab \strong{\code{H} (herbaceous)} \tab \strong{\code{C} 
##'        (chamaephyte) or \code{P} (phanerophyte)} \cr
##'       \strong{from class \code{1} to \code{2}} \tab \code{maturity - 2} 
##'       \tab \code{1} \cr
##'       \strong{from class \code{2} to \code{3}} \tab \code{maturity} \tab 
##'       \code{min}(\code{maturity - 2 , age_above_150cm}) \cr
##'       \strong{from class \code{3} to \code{4}} \tab \code{longevity - 2} 
##'       \tab \code{min}(\code{longevity - 2 , age_above_150cm})
##'     }
##'     
##'     Some corrections are made for short-living plants (annuals and 
##'     biennials) :
##'     \itemize{
##'       \item as they die after 1 or 2 years, they are not affected 
##'       differently according to life stages
##'       \item break ages from class \code{1} to \code{3} are set to \code{1}, 
##'       and break age from \code{3} to \code{4} is set to their longevity 
##'       (\code{1} or \code{2}) \cr \cr
##'     }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{breakAge} column, 
##'       if provided \cr \cr}
##'   }
##'   }
##'   \item{RESPR_AGE}{ = when subject to a perturbation, each PFG can either 
##'   stay undisturbed, be killed, or resprout at a particular age 
##'   \emph{(in years)} \cr
##'    = ages at which each PFG will be rejuvenated by a disturbance \cr \cr
##'   Two methods to define these ages are available :
##'   \itemize{
##'     \item from \strong{predefined rules} (using \code{maturity}, 
##'     \code{longevity}, \code{age_above_150cm}) :
##'     \itemize{
##'       \item classes \code{1} and \code{2} : too young to resprout
##'       \item class \code{3} : 
##'       \code{min}(\code{maturity - 2 , age_above_150cm})
##'       \item class \code{4} : \code{longevity - 2}
##'       \item short-living plants (annuals and biennials) always start back 
##'       at \code{0} \cr \cr
##'     }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{resproutAge} column, 
##'       if provided \cr \cr}
##'   }
##'   }
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
##'         \item{}{\code{_________________________________________}}
##'         \item{indifferent}{\code{| _0_ _0_ | _0_ _0_ | _0_ _0_ | _0_ _0_ |}}
##'         \item{}{\code{_________________________________________}}
##'         \item{mowing_herbs}{\code{| _0_ _0_ | _0_ _0_ | 50\% 50\% | 100\% 
##'         0_ |}}
##'         \item{mowing_trees}{\code{| _0_ _0_ | 100\% 0_ | 100\% 0_ | 100\% 
##'         0_ |}}
##'         \item{}{\code{_________________________________________}}
##'         \item{grazing_herbs_1}{\code{| _0_ _0_ | 10\% _0_ | _0_ 50\% | _0_ 
##'         10\% |}}
##'         \item{grazing_herbs_2}{\code{| _0_ _0_ | 50\% _0_ | _0_ 80\% | 10\% 
##'         50\% |}}
##'         \item{grazing_herbs_3}{\code{| _0_ _0_ | 90\% _0_ | 10\% 90\% | 50\% 
##'         50\% |}}
##'         \item{}{\code{_________________________________________}}
##'         \item{grazing_trees_1}{\code{| 40\% _0_ | _0_ _0_ | _0_ _0_ | _0_ 
##'         _0_ |}}
##'         \item{grazing_trees_2}{\code{| 80\% _0_ | _0_ _0_ | _0_ _0_ | _0_ 
##'         _0_ |}}
##'         \item{grazing_trees_3}{\code{| 100\% 0_ | 40\% _0_ | _0_ _0_ | _0_ 
##'         _0_ |}}
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
##'   \item{PROP_KILLED}{ = the proportion of propagules killed by each 
##'   disturbance \cr
##'   (\emph{currently set to \code{0} for all PFG and disturbances})
##'   }
##'   \item{ACTIVATED_SEED}{ = the proportion of seeds activated by each 
##'   disturbance \cr
##'   (\emph{currently set to \code{0} for all PFG and disturbances})
##'   }
##' }
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/DIST/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{BREAK_AGE}{ages at which the PFG changes of response stage 
##'   \emph{(in years)}}
##'   \item{RESPR_AGE}{resprouting age table (in a single row) \cr
##'   This is a vector of \code{no.DIST * no.responseStages} numbers 
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
##'   This is a vector of \code{no.DIST * no.responseStages * 2} numbers 
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
##' }
##' 
##' A \code{DIST_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \code{name.simulation/DATA/PFGS/} directory.  
##'
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \code{name.simulation/DATA/PFGS/DIST/opt.folder.name/}.
##' 
##' 
##' 
##' @keywords FATE, simulation, disturbance, killing, resprouting
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}}
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



PRE_FATE.params_PFGdisturbance = function(
  name.simulation
  , mat.PFG.dist = NULL
  , mat.PFG.tol
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/DIST/")
  
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
    .testParam_notChar.m("mat.PFG.tol$nameDist", mat.PFG.tol$nameDist)
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
                               , c("indifferent", "mowing_herbs", "mowing_trees"
                                   , "grazing_herbs_1", "grazing_herbs_2", "grazing_herbs_3"
                                   , "grazing_trees_1", "grazing_trees_2", "grazing_trees_3"))
    }
  }
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/PFGS/DIST/"))

    
  #############################################################################
  
  ## GET informations
  NAME = unique(as.character(mat.PFG.tol$PFG))
  no.PFG = length(NAME)
  DIST_NAME = unique(as.character(mat.PFG.tol$nameDist))
  no.DIST = length(DIST_NAME)
  no.STAGES = 4
  if (sum(colnames(mat.PFG.tol) == "responseStage") == 1){
    no.STAGES = max(mat.PFG.tol$responseStage)
  }
  if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1){
    no.STAGES = 4
  }
  
  cat("\n ---------- INFORMATION : DIST \n")
  cat("\n  Number of disturbances : ", no.DIST)
  cat("\n  Names of disturbances : ", DIST_NAME)
  cat("\n  Number of response stages : ", no.STAGES)
  cat("\n")
  
  
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
                          , indifferent = c(0,0,0,0,0,0,0,0)
                          , mowing_herbs = c(0,0,0,0,5,5,10,0)
                          , mowing_trees = c(0,0,10,0,10,0,10,0)
                          , grazing_herbs_1 = c(0,0,1,0,0,5,0,1)
                          , grazing_herbs_2 = c(0,0,5,0,0,8,1,5)
                          , grazing_herbs_3 = c(0,0,9,0,1,9,5,5)
                          , grazing_trees_1 = c(4,0,0,0,0,0,0,0)
                          , grazing_trees_2 = c(8,0,0,0,0,0,0,0)
                          , grazing_trees_3 = c(10,0,4,0,0,0,0,0)
                          
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
                            , "ACTIVATED_SEED")
  
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
                           , paste0("ACTIVATED_SEED_", DIST_NAME))
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == ""
                                       , ""
                                       , sub("/$", "_", opt.folder.name))
                              , "DIST_COMPLETE_TABLE.csv")
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
                                       , "/DATA/PFGS/DIST/"
                                       , opt.folder.name
                                       , "DIST_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG disturbance parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

