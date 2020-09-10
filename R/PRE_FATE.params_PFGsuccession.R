### HEADER #####################################################################
##' @title Create \emph{SUCCESSION} parameter files for a \code{FATE} simulation
##' 
##' @name PRE_FATE.params_PFGsuccession
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' succession parameters for each PFG (one file for each of them) used in the 
##' core module of \code{FATE}.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param mat.PFG.succ a \code{data.frame} with at least 5 columns : \cr 
##' \code{PFG}, \code{type}, \code{height}, \code{maturity}, \code{longevity} 
##' \cr (\emph{and optionally, \code{max_abundance}, \code{potential_fecundity}, 
##' \code{immature_size}, \code{is_alien}, \code{flammability}}) 
##' \cr (see \href{PRE_FATE.params_PFGsuccession.html#details}{\code{Details}})
##' @param strata.limits a \code{vector} of \code{integer} containing values 
##' among which height strata limits will be chosen
##' @param strata.limits_reduce default \code{TRUE}. \cr If \code{TRUE}, stratum 
##' height limits are checked to try and bring several PFGs together in a same 
##' stratum
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/SUCC/} directory to store the results
##' 
##' 
##' @details
##' 
##' The \strong{core module} of \code{FATE} allows the user to simulate a 
##' primary vegetation succession based on a \strong{demography model}. \cr \cr
##' 
##' Several parameters, given within \code{mat.PFG.succ}, are required for 
##' each PFG in order to set up this life cycle :
##' 
##' \describe{
##'   \item{type}{or life-form, based on Raunkier. \cr It should be either 
##'   \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) 
##'   for now}
##'   \item{height}{the maximum or average height that reach the PFG}
##'   \item{maturity}{the age from which the PFG can reproduce}
##'   \item{longevity}{the maximum or average lifespan of the PFG \cr \cr}
##'   \item{(\emph{max_abundance})}{the maximum abundance of mature PFG in 
##'   favorable conditions}
##'   \item{(\emph{potential_fecundity})}{the maximum number of seeds produced 
##'   by the PFG}
##'   \item{(\emph{immature_size})}{the relative size of immature versus mature 
##'   plants}
##'   \item{(\emph{is_alien})}{if the PFG is to be considered as an alien 
##'   (\code{1}) or not (\code{0})}
##'   \item{(\emph{flammability})}{how easily the PFG burns \cr \cr}
##' }
##' 
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' all PFG :
##' 
##' \describe{
##'   \item{STRATA_LIMITS}{= height values that define each stratum.\cr \cr
##'   Two methods to define these limits are available :
##'   \itemize{
##'     \item from \strong{predefined rules} (using \code{strata.limits_reduce 
##'     = TRUE}, \code{strata.limits}, \code{height}) : \cr \cr
##'     \itemize{
##'       \item limits should go exponentially and are selected among 
##'       \code{strata.limits}
##'       \item PFG are separated according to these \code{strata.limits} and 
##'       then grouped making sure to have 
##'       \deqn{\text{number of PFG per stratum} \geq \sqrt{\text{total number 
##'       of PFG}} - 2}
##'       to try to homogenize the number of PFG within each stratum. \cr \cr
##'     }
##'     \item from \strong{user data} : (using \code{strata.limits_reduce = 
##'     FALSE}) \cr
##'       \emph{with the values contained within the \code{strata.limits} 
##'       column, if provided \cr \cr}
##'   }
##'   }
##' }
##' 
##' and a set of characteristics for each PFG :
##' 
##' \describe{
##'   \item{MAX_STRATUM}{= maximum stratum that each PFG can reach}
##'   \item{MAX_ABUNDANCE}{= maximum abundance of mature PFG in favorable 
##'   conditions \cr
##'    = It can be seen as a proxy of maximum carrying capacity for mature 
##'   individuals \cr (\emph{and therefore as a proxy a quantity of shade 
##'   produced as well, if the light module is activated}). \cr \cr
##'   Two methods to define these abundances are available :
##'   \itemize{
##'     \item from \strong{predefined rules} (using \code{type}, 
##'     \code{MAX_STRATUM}) : \cr \cr
##'     \tabular{rcccc}{
##'       \strong{MAX_STRATUM} \tab \strong{1} \tab \strong{2} \tab 
##'       \strong{3} \tab \strong{+}\cr
##'       \strong{\code{H} (herbaceous)} \tab \code{1} \tab \code{1} \tab 
##'       \code{2} \tab \code{2} \cr
##'       \strong{\code{C} (chamaephyte)} \tab \code{1} \tab \code{2} \tab 
##'       \code{2} \tab \code{3} \cr
##'       \strong{\code{P} (phanerophyte)} \tab \code{1} \tab \code{2} \tab 
##'       \code{3} \tab \code{3}
##'     }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{max_abundance} 
##'       column, if provided \cr \cr}
##'   }
##'   }
##'   \item{IMM_SIZE}{= relative size of immature versus mature plants \cr
##'   = for example, immature herbaceous take as much space as mature 
##'   herbaceous, while immature phanerophytes take less space (\emph{and 
##'   contribute to shade half less}) than mature individuals \cr \cr
##'   Two methods to define these sizes are available :
##'   \itemize{
##'     \item from \strong{predefined rules} (using \code{type}, \code{MAX_STRATUM}) 
##'     : \cr \cr
##'     
##'     \tabular{rcccc}{
##'       \strong{MAX_STRATUM} \tab \strong{1} \tab \strong{2} \tab 
##'       \strong{3} \tab \strong{+}\cr
##'       \strong{\code{H} (herbaceous)} \tab \code{100\%} \tab \code{80\%} 
##'       \tab \code{50\%} \tab \code{50\%} \cr
##'       \strong{\code{C} (chamaephyte)} \tab \code{100\%} \tab \code{50\%} 
##'       \tab \code{50\%} \tab \code{50\%} \cr
##'       \strong{\code{P} (phanerophyte)} \tab \code{50\%} \tab \code{50\%} 
##'       \tab \code{50\%} \tab \code{10\%}
##'     }
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{immature_size} 
##'       column, if provided \cr \cr}
##'   }
##'   }
##'   \item{CHANG_STR_AGES}{= at what age each PFG goes into the upper stratum 
##'   \cr \cr It is defined using a logistic growth curve with 2 points to 
##'   parameterize it :
##'   \enumerate{
##'     \item at \eqn{age = \text{maturity}/2}, \eqn{height = \text{IMM_SIZE} * \text{height}}
##'     \item at \eqn{age = \text{longevity}}, \eqn{height = \text{height}} \cr \cr
##'   }
##'   }
##'   \item{POTENTIAL_FECUNDITY}{= maximum number of seeds produced by the PFG 
##'   \cr \cr
##'   Two methods to define this number are available :
##'   \itemize{
##'     \item from \strong{predefined rules} : same value for all PFG 
##'     (\code{100\%})
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{potential_fecundity} 
##'       column, if provided \cr \cr}
##'   }
##'   }
##'   \item{IS_ALIEN}{= if the PFG is to be considered as an alien (\code{1}) or 
##'   not (\code{0})}
##'   \item{FLAMMABILITY}{= how easily the PFG burns}
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \file{name.simulation/DATA/PFGS/SUCC/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{NAME}{name of the PFG}
##'   \item{TYPE}{PFG life-form \emph{(\code{H}: herbaceous \code{C}: 
##'   chamaephyte \code{P}: phanerophyte)}}
##'   \item{HEIGHT}{PFG maximum height \emph{(in cm)}}
##'   \item{MATURITY}{PFG maturity age \emph{(in years)}}
##'   \item{LONGEVITY}{PFG life span \emph{(in years)}}
##'   \item{MAX_STRATUM}{maximum height stratum that the PFG can reach}
##'   \item{MAX_ABUNDANCE}{maximum abundance / space (qualitative) that the PFG 
##'   is able to produce / occupy \cr \emph{(\code{1}: Low \code{2}: Medium 
##'   \code{3}: High)}}
##'   \item{IMM_SIZE}{PFG immature relative size \emph{(from \code{0} to 
##'   \code{10}, corresponding to 0 to 100\%)}}
##'   \item{CHANG_STR_AGES}{ages at which the PFG goes in the upper stratum 
##'   \cr \emph{(in years, put a value higher than the PFG life span if it is 
##'   not supposed to rise a stratum)}}
##'   \item{SEED_POOL_LIFE}{maximum number of years seeds are able to survive 
##'   (for active and dormant pool)}
##'   \item{SEED_DORMANCY}{are the seeds dormant or not \emph{(\code{0}: No 
##'   \code{1}: Yes)}}
##'   \item{POTENTIAL_\cr FECUNDITY}{maximum number of seeds produced by the 
##'   PFG \cr \emph{(set by default to \code{100})}}
##'   \item{IS_ALIEN}{is the PFG an alien or not \emph{(\code{0}: No \code{1}: 
##'   Yes)}}
##'   \item{FLAMMABILITY}{how easily the PFG burns \emph{(\code{numeric})}
##'   \cr \cr}
##' }
##' 
##' A \file{SUCC_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \file{name.simulation/DATA/PFGS/} directory. \cr
##' This file can be used to parameterize the disturbance files (see 
##' \code{\link{PRE_FATE.params_PFGdisturbance}}).
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \file{name.simulation/DATA/PFGS/SUCC/opt.folder.name/}.
##' 
##' 
##' @keywords FATE, simulation, height, longevity, maturity
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}}, 
##' \code{\link{PRE_FATE.params_PFGdisturbance}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:6)
##'                                                           , type = c("C", "C", "H", "H", "P", "P")
##'                                                           , height = c(10, 250, 36, 68, 1250, 550)
##'                                                           , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                           , longevity = c(12, 200, 25, 4, 110, 70)))
##'                                                         
##' 
##' ## Create PFG succession parameter files
##' PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
##'                               , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:6)
##'                                                           , type = c("C", "C", "H", "H", "P", "P")
##'                                                           , height = c(10, 250, 36, 68, 1250, 550)
##'                                                           , maturity = c(5, 5, 3, 3, 8, 9)
##'                                                           , longevity = c(12, 200, 25, 4, 110, 70)
##'                                                           , immature_size = c(10, 8, 10, 10, 1, 5)))
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
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsuccession = function(
  name.simulation
  , mat.PFG.succ
  , strata.limits = c(0, 20, 50, 150, 400, 1000, 2000, 5000, 10000)
  , strata.limits_reduce = TRUE
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/SUCC/")
  
  ## CHECK parameter mat.PFG.succ
  if (.testParam_notDf(mat.PFG.succ))
  {
    .stopMessage_beDataframe("mat.PFG.succ")
  } else
  {
    if (nrow(mat.PFG.succ) == 0 || !(ncol(mat.PFG.succ) %in% c(5, 6, 7, 8, 9, 10)))
    {
      .stopMessage_numRowCol("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"
                                               , "(max_abundance)", "(potential_fecundity)"
                                               , "(immature_size)", "(is_alien)", "(flammability)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.succ))
                          , "5" = .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"))
                          , "6" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "flammability")))
                          , "7" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance"
                                                                          , "potential_fecundity")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size"
                                                                            , "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "immature_size"
                                                                            , "flammability")))
                          , "8" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                          , "longevity", "max_abundance"
                                                                          , "potential_fecundity", "immature_size")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "potential_fecundity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "potential_fecundity", "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "max_abundance"
                                                                            , "immature_size", "flammability")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "potential_fecundity", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity"
                                                                            , "longevity", "potential_fecundity"
                                                                            , "potential_fecundity", "flammability")))
                          , "9" = (.testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                          , "max_abundance", "potential_fecundity"
                                                                          , "immature_size", "is_alien")) &&
                                     .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                            , "max_abundance", "potential_fecundity"
                                                                            , "immature_size", "flammability")))
                          , "10" = .testParam_notColnames(mat.PFG.succ, c("PFG", "type","height", "maturity", "longevity"
                                                                          , "max_abundance", "potential_fecundity"
                                                                          , "immature_size", "is_alien", "flammability"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.succ", c("PFG", "type","height", "maturity", "longevity"
                                                   , "(max_abundance)", "(potential_fecundity)"
                                                   , "(immature_size)", "(is_alien)", "(flammability)"))
      }
    }
    mat.PFG.succ$PFG = as.character(mat.PFG.succ$PFG)
    .testParam_samevalues.m("mat.PFG.succ$PFG", mat.PFG.succ$PFG)
    .testParam_notChar.m("mat.PFG.succ$PFG", mat.PFG.succ$PFG)
    mat.PFG.succ$type = as.character(mat.PFG.succ$type)
    .testParam_notInValues.m("mat.PFG.succ$type", mat.PFG.succ$type, c("H", "C", "P"))
    .testParam_notNum.m("mat.PFG.succ$height", mat.PFG.succ$height)
    .testParam_NAvalues.m("mat.PFG.succ$height", mat.PFG.succ$height)
    .testParam_notNum.m("mat.PFG.succ$maturity", mat.PFG.succ$maturity)
    .testParam_NAvalues.m("mat.PFG.succ$maturity", mat.PFG.succ$maturity)
    .testParam_notNum.m("mat.PFG.succ$longevity", mat.PFG.succ$longevity)
    .testParam_NAvalues.m("mat.PFG.succ$longevity", mat.PFG.succ$longevity)
    if (sum(mat.PFG.succ$maturity > mat.PFG.succ$longevity) > 0){
      stop(paste0("Wrong type of data!\n `mat.PFG.succ$maturity` must contain "
                  , "values equal or inferior to `mat.PFG.succ$longevity`"))
    }
    
    if (sum(colnames(mat.PFG.succ) == "max_abundance") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$max_abundance", mat.PFG.succ$max_abundance)
      .testParam_notInValues.m("mat.PFG.succ$max_abundance", mat.PFG.succ$max_abundance, 1:3)
    }
    if (sum(colnames(mat.PFG.succ) == "potential_fecundity") == 1)
    {
      .testParam_notNum.m("mat.PFG.succ$potential_fecundity", mat.PFG.succ$potential_fecundity)
      .testParam_NAvalues.m("mat.PFG.succ$potential_fecundity", mat.PFG.succ$potential_fecundity)
    }
    if (sum(colnames(mat.PFG.succ) == "immature_size") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$immature_size", mat.PFG.succ$immature_size)
      .testParam_notInValues.m("mat.PFG.succ$immature_size", mat.PFG.succ$immature_size, 0:10)
    }
    if (sum(colnames(mat.PFG.succ) == "is_alien") == 1)
    {
      .testParam_NAvalues.m("mat.PFG.succ$is_alien", mat.PFG.succ$is_alien)
      .testParam_notInValues.m("mat.PFG.succ$is_alien", mat.PFG.succ$is_alien, 0:1)
    }
    if (sum(colnames(mat.PFG.succ) == "flammability") == 1)
    {
      .testParam_notNum.m("mat.PFG.succ$flammability", mat.PFG.succ$flammability)
      .testParam_NAvalues.m("mat.PFG.succ$flammability", mat.PFG.succ$flammability)
    }
  }
  ## CHECK parameter strata.limits
  strata.limits = sort(unique(na.exclude(strata.limits)))
  .testParam_notInteger.m("strata.limits", strata.limits)
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/PFGS/SUCC/"))
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # PRE_FATE.params_PFGsuccession")
  cat("\n #------------------------------------------------------------# \n")
  
  #############################################################################
  
  no.PFG = nrow(mat.PFG.succ)
  
  ## GET informations
  NAME = as.character(mat.PFG.succ$PFG)
  TYPE = as.character(mat.PFG.succ$type)
  HEIGHT = mat.PFG.succ$height
  MATURITY = mat.PFG.succ$maturity
  
  ## Death precedes seed productivity in the model 
  ## thus longevity param = longevity + 1
  LONGEVITY = mat.PFG.succ$longevity + 1
  
  IS_ALIEN = rep(0, no.PFG)
  if (sum(colnames(mat.PFG.succ) == "is_alien") == 1)
  {
    IS_ALIEN = mat.PFG.succ$is_alien
  }
  
  FLAMMABILITY = rep(0, no.PFG)
  if (sum(colnames(mat.PFG.succ) == "flammability") == 1)
  {
    FLAMMABILITY = mat.PFG.succ$flammability
  }
  
  cat("\n ---------- INFORMATION : GROUP \n")
  cat("\n  Number of groups : ", no.PFG)
  cat("\n  Number of PFG of each type : "
      , length(which(TYPE == "H")), " H, "
      , length(which(TYPE == "C")), " C, "
      , length(which(TYPE == "P")), " P, ")
  cat("\n  Number of aliens : ", length(which(IS_ALIEN == 1)))
  cat("\n")
  
  #############################################################################
  
  ## GET height strata limits (for light competition and PFG growth)
  ## n strata (+ germinants = 0)
  if (strata.limits_reduce)
  {
    no.PFG.perStrata = round(sqrt(no.PFG))
    categories = cut(mat.PFG.succ$height, breaks = strata.limits)
    categories.table = table(categories)
    if (no.PFG == 1)
    {
      categ = which(categories.table == 1)
      STRATA_LIMITS = c(0, strata.limits[categ], strata.limits[categ + 1])    
    } else
    {
      STRATA_LIMITS = 0
      tmp = categories.table[1]
      for (categ in 2:length(strata.limits))
      {
        if (tmp >= max(c(1, (no.PFG.perStrata - 2))))
        {
          STRATA_LIMITS = c(STRATA_LIMITS, strata.limits[categ])
          tmp = categories.table[categ]
        } else 
        {
          tmp = tmp + categories.table[categ]
        }
      }
    }
    STRATA_LIMITS = sort(unique(STRATA_LIMITS))
  } else
  {
    STRATA_LIMITS = strata.limits
  }
  # barplot(table(cut(mat.PFG.succ$height, breaks = STRATA_LIMITS)))
  
  ## GET MAX_STRATUM attribution
  MAX_STRATUM = sapply(mat.PFG.succ$height, function(h) {
    max(which(STRATA_LIMITS < h), na.rm = T)
  })
  
  no.strata = max(MAX_STRATUM)
  
  cat("\n ---------- INFORMATION : STRATA \n")
  cat("\n  Number of strata : ", no.strata)
  cat("\n  Height limits of selected strata : ", STRATA_LIMITS)
  cat("\n  Number of PFG within each stratum : "
      , table(cut(mat.PFG.succ$height
                  , breaks = STRATA_LIMITS)))
  cat("\n")
  
  #############################################################################
  
  ## GET MAX ABUNDANCE
  ##  = maximum abundance of mature PFG in favorable conditions
  ##  = maximum shade a PFG can make in a pixel corresponding to a number of individuals
  ## Defined according to the number of strata potentially occupied by a PFG
  ## 3 levels : 1 = Low, 2 = Medium or 3 = High
  if (sum(colnames(mat.PFG.succ) == "max_abundance") == 1)
  {
    MAX_ABUNDANCE = mat.PFG.succ$max_abundance
  } else
  {
    ## herbaceous make little shade 
    ## chamaephytes make medium shade 
    ## phanerophytes make lot of shade 
    ## all plants in first stratum make little shade 
    ## plants other than herbaceous in stratum 2 make medium shade 
    ## herbaceous in stratum > 2 make medium shade 
    ## chamaephytes in stratum > 3 make lot of shade
    MAX_ABUNDANCE = rep(NA, no.PFG)
    MAX_ABUNDANCE[which(mat.PFG.succ$type == "H")] = 1
    MAX_ABUNDANCE[which(mat.PFG.succ$type == "C")] = 2
    MAX_ABUNDANCE[which(mat.PFG.succ$type == "P")] = 3
    MAX_ABUNDANCE[which(MAX_STRATUM == 1)] = 1
    MAX_ABUNDANCE[which(MAX_STRATUM == 2 & mat.PFG.succ$type != "H")] = 2
    MAX_ABUNDANCE[which(MAX_STRATUM > 2 & mat.PFG.succ$type == "H")] = 2
    MAX_ABUNDANCE[which(MAX_STRATUM > 3 & mat.PFG.succ$type == "C")] = 3
  }
  
  #############################################################################
  
  ## GET IMMATURE SIZE
  ##   = relative shade of immature plants
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
  if (sum(colnames(mat.PFG.succ) == "immature_size") == 1)
  {
    IMM_SIZE = mat.PFG.succ$immature_size
  } else
  {
    ## immature herbaceous contribute to shade in the same way than mature herbaceous
    ## immature chamaephytes contribute to shade half less than mature herbaceous
    ## immature phanerophytes contribute to shade only by 10 % of their full capacity
    ## intermediate percentage for herbaceous in stratum 2
    ## intermediate percentage for herbaceous in stratum > 2
    ## immature chamaephytes in 1st stratum contribute to shade in the same way than mature chamaephytes
    ## immature phanerophytes with height < 10m contribute to shade half less than mature phanerophytes
    IMM_SIZE = rep(10, no.PFG)
    IMM_SIZE[which(mat.PFG.succ$type == "H")] = 10
    IMM_SIZE[which(mat.PFG.succ$type == "C")] = 5
    IMM_SIZE[which(mat.PFG.succ$type == "P")] = 1
    IMM_SIZE[which(mat.PFG.succ$type == "H" & MAX_STRATUM == 2)] = 8
    IMM_SIZE[which(mat.PFG.succ$type == "H" & MAX_STRATUM > 2)] = 5
    IMM_SIZE[which(mat.PFG.succ$type == "C" & MAX_STRATUM == 1)] = 10
    IMM_SIZE[which(mat.PFG.succ$type == "P" & mat.PFG.succ$height < 1000)] = 5
  }
  
  #############################################################################
  
  ## GET CHANGE STRATA AGES
  ## Logistic growth curve with 2 points to parameterize it :
  ## at age = maturity/2, height = IMM_SIZE * height	
  ## at age = longevity, height = height
  CHANG_STR_AGES = matrix(0, nrow = no.strata, ncol = no.PFG)
  if (no.strata > 1)
  {
    CHANG_STR_AGES[2:no.strata, ] = 10000
    for (i in 1:no.PFG)
    {
      ## If not in first stratum / herbaceous (or potentially chamaephytes) :
      if (!(IMM_SIZE[i] == 10))
      {
        k = -log(1 - IMM_SIZE[i] / 10) / (MATURITY[i] / 2)
        A = 1:LONGEVITY[i]
        
        ## negative binomiale curve
        H = mat.PFG.succ$height[i] * (1 - exp(-k * A))
        
        # calculation of transition ages depending on strata heights
        for (str in 2:no.strata) {
          age.brk = A[which(H >= STRATA_LIMITS[str])][1]
          CHANG_STR_AGES[str, i] = ifelse(is.na(age.brk), CHANG_STR_AGES[str, i], age.brk)
        }
      }
    }
  }
  
  #############################################################################
  
  ## GET SEED POOLS (active and dormant) LIFE SPAN
  ##   = available seeds will exponentially decrease according to seed pool life span parameter
  SEED_POOL_LIFE = matrix(0, nrow = 2, ncol = no.PFG)
  
  ## GET SEED DORMANCY
  ## 0 = no
  ## 1 = yes
  SEED_DORMANCY = rep(0, no.PFG)
  
  ## GET POTENTIAL FECUNDITY
  if (sum(colnames(mat.PFG.succ) == "potential_fecundity") == 1)
  {
    POTENTIAL_FECUNDITY = mat.PFG.succ$potential_fecundity
  } else
  {
    POTENTIAL_FECUNDITY = rep(100, no.PFG)
  }
  
  #############################################################################
  
  names.params.list = get("NAME")
  names.params.list.sub = c("NAME"
                            , "TYPE"
                            , "HEIGHT"
                            , "LONGEVITY"
                            , "MATURITY"
                            , "MAX_STRATUM"
                            , "MAX_ABUNDANCE"
                            , "IMM_SIZE"
                            , "CHANG_STR_AGES"
                            , "SEED_POOL_LIFE"
                            , "SEED_DORMANCY"
                            , "POTENTIAL_FECUNDITY"
                            , "IS_ALIEN"
                            , "FLAMMABILITY")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "TYPE"
                           , "HEIGHT"
                           , "LONGEVITY"
                           , "MATURITY"
                           , "MAX_STRATUM"
                           , "MAX_ABUNDANCE"
                           , "IMM_SIZE"
                           , paste0("CHANG_STR_AGES_to_str_"
                                    , 1:no.strata, "_"
                                    , STRATA_LIMITS[1:no.strata])
                           , paste0("SEED_POOL_LIFE_", c("active", "dormant"))
                           , "SEED_DORMANCY"
                           , "POTENTIAL_FECUNDITY"
                           , "IS_ALIEN"
                           , "FLAMMABILITY")
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == ""
                                       , ""
                                       , sub("/$", "_", opt.folder.name))
                              , "SUCC_COMPLETE_TABLE.csv")
              , row.names = FALSE
              , col.names = TRUE)
  
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
                                       , "/DATA/PFGS/SUCC/"
                                       , opt.folder.name
                                       , "SUCC_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG succession parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
  
}

