### HEADER #####################################################################
##' @title Create \emph{SOIL} parameter files for a \code{FATE} simulation
##' 
##' @name PRE_FATE.params_PFGsoil
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' soil contribution and tolerance for each PFG (one file for each of them) 
##' used in the soil module of \code{FATE}.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation 
##' @param mat.PFG.soil a \code{data.frame} with 3 to 7 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{type}, (\emph{or \code{active_germ_low}, 
##'   \code{active_germ_medium}, \code{active_germ_high}}) (\emph{or
##'   \code{strategy_ag}})
##'   \item \code{soil_contrib}, \code{soil_tol_min}, \code{soil_tol_max} 
##'   (\emph{or \code{strategy_contrib}})
##' }
##' (see \href{PRE_FATE.params_PFGsoil.html#details}{\code{Details}})
##' @param mat.PFG.tol (\emph{optional}) \cr 
##' a \code{data.frame} with 2 to 4 columns : \cr 
##' \itemize{
##'   \item \code{PFG},
##'   \item \code{lifeStage}, \code{resources}, \code{tolerance} 
##'   (\emph{or \code{strategy_tol}})
##' }
##' (see \href{PRE_FATE.params_PFGsoil.html#details}{\code{Details}})
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/SOIL/} directory to store the results
##' 
##' 
##' @details
##' 
##' The \strong{soil module} allows the user to add the effect of 
##' \strong{soil interaction} within a primary vegetation succession. \cr \cr
##' 
##' Several parameters, given within \code{mat.PFG.soil} or \code{mat.PFG.tol}, 
##' are required for each PFG in order to set up the soil interaction :
##' 
##' \describe{
##'   \item{PFG}{the concerned plant functional group \cr \cr}
##'   
##'   \item{type}{or life-form, based on Raunkier. \cr It should be either 
##'   \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) 
##'   for now}
##'   \item{(\emph{active_germ_low})}{an \code{integer} between \code{0} and 
##'   \code{100} corresponding to the proportion of seeds that will germinate for 
##'   \code{Low} soil condition}
##'   \item{(\emph{active_germ_medium})}{an \code{integer} between \code{0} and 
##'   \code{100} corresponding to the proportion of seeds that will germinate for 
##'   \code{Medium} soil condition}
##'   \item{(\emph{active_germ_high})}{an \code{integer} between \code{0} and 
##'   \code{100} corresponding to the proportion of seeds that will germinate for 
##'   \code{High} soil condition}
##'   \item{(\emph{strategy_ag})}{a \code{string} to choose the germination 
##'   strategy : \cr \code{poor_lover}, \code{indifferent}, \code{rich_lover} 
##'   \cr \cr}
##'   
##'   \item{soil_contrib}{a value corresponding to the PFG preference for soil 
##'   fertility \cr (e.g. nitrogen value from Ellenberg or Flora Indicativa)}
##'   \item{soil_tol_min}{the minimum soil value tolerated by the PFG (on the 
##'   same scale than \code{soil_contrib})}
##'   \item{soil_tol_max}{the maximum soil value tolerated by the PFG (on the 
##'   same scale than \code{soil_contrib})}
##'   \item{(\emph{strategy_contrib})}{a \code{string} to choose the 
##'   contribution strategy : \cr \code{oligotrophic}, \code{mesotrophic}, 
##'   \code{eutrophic} \cr \cr}
##'   
##'   \item{lifeStage}{the concerned life stage (\code{Germinant}, 
##'   \code{Immature}, \code{Mature})}
##'   \item{resources}{the concerned soil condition (\code{Low}, 
##'   \code{Medium}, \code{High})}
##'   \item{tolerance}{an \code{integer} between \code{0} and \code{100} 
##'   corresponding to the proportion of surviving individuals}
##'   \item{(\emph{strategy_tol})}{a \code{string} to choose the tolerance 
##'   strategy : \cr \code{poor_lover}, \code{ubiquist}, \code{rich_lover} 
##'   \cr \cr}
##' }
##' 
##' These values will allow to calculate or define a set of characteristics for 
##' each PFG :
##' 
##' \describe{
##'   \item{ACTIVE_GERM}{proportion of seeds that will germinate for each soil 
##'   condition (\code{Low}, \code{Medium}, \code{High}) \cr \cr
##'   Three methods to define these proportions are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using \code{strategy_ag}) : \cr \cr
##'     \strong{\code{| _L_ _M_ _H_ |}} \cr
##'     \code{_______________} \cr
##'     \code{| 80\% 90\% 50\% |} \strong{poor_lover} \cr
##'     \code{| 90\% 90\% 90\% |} \strong{indifferent} \cr
##'     \code{| 50\% 90\% 80\% |} \strong{rich_lover} \cr \cr
##'     
##'     \item from \strong{predefined rules} (using \code{type}) :
##'     \itemize{
##'       \item for \code{H} (herbaceous) : \code{80\%, 100\%, 50\%}
##'       \item for \code{C} (chamaephyte) or \code{P} (phanerophyte) : 
##'       \code{90\%, 100\%, 90\%}
##'     }
##'     \item from \strong{user data} : \cr
##'     \emph{with the values contained within the \code{active_germ_low}, 
##'     \code{active_germ_medium} and \code{active_germ_high} columns, if 
##'     provided \cr \cr}
##'   }
##'   }
##'   
##'   \item{SOIL_CONTRIB \cr SOIL_LOW \cr SOIL_HIGH}{
##'   Two methods to define these values are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using 
##'     \code{strategy_contrib}) : \cr
##'       \itemize{
##'         \item the values give the \code{soil_tol_min}, \code{soil_contrib} 
##'         and \code{soil_tol_max}
##'         \item with \code{L}: low soil, \code{M}: medium soil, \code{H}: 
##'         high soil \cr \cr
##'       }
##'       \strong{\code{| ___ L ___ | ___ M ___ | ___ H ___ |}} \cr
##'       \code{_____________________________________} \cr
##'       \code{__________ 1 ___ 1.5 ___ 2 __________} \strong{oligotrophic} \cr
##'       \code{__________ 1.5 _ 2.5 _ 4.5 __________} \strong{mesotrophic} \cr
##'       \code{__________ 3 ____ 4 ____ 5 __________} \strong{eutrophic} \cr \cr
##'       
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{soil_contrib}, 
##'       \code{soil_tol_min} and \code{soil_tol_max} columns, if provided \cr \cr}
##'   }
##'   }
##'   
##'   \item{SOIL_TOL}{ defined for each life stage (\code{Germinant}, 
##'   \code{Immature}, \code{Mature}) \cr and each soil condition (\code{Low}, 
##'   \code{Medium}, \code{High}) \cr \cr
##'   Three methods to define these tolerances are available :
##'   \itemize{
##'     \item from \strong{predefined scenarios} (using 
##'     \code{strategy_tol}) : \cr
##'       \itemize{
##'         \item the values give the percentage of surviving individuals to the 
##'         concerned conditions
##'         \item with \code{g}: Germinant, \code{i}: Immature, \code{m}: Mature
##'         \item with \code{L}: low soil, \code{M}: medium soil, \code{H}: 
##'         high soil \cr \cr
##'       }
##'       \strong{\code{| _____ g ____ | _____ i ____ | _____ m ____ |}} \cr
##'       \strong{\code{| _L__ _M_ _H_ | _L__ _M_ _H_ | _L__ _M_ _H_ |}} \cr
##'       \code{______________________________________________} \cr
##'       \code{| 30\% 100\% 10\% | 60\% 100\% 40\% | 90\% 100\% 70\% |} \strong{poor_lover} \cr
##'       \code{| 90\% 100\% 80\% | 90\% 100\% 80\% | 90\% 100\% 80\% |} \strong{ubiquist} \cr
##'       \code{| 10\% 100\% 30\% | 40\% 100\% 60\% | 70\% 100\% 90\% |} \strong{rich_lover} \cr \cr
##'       
##'     \item from \strong{predefined rules} (corresponding to the 
##'     \code{poor_lover} strategy) :
##'       \describe{
##'         \item{(A)}{germinants are severely impacted by wrong soil conditions}
##'         \item{(B)}{immatures are half impacted by wrong soil conditions}
##'         \item{(C)}{matures are little impacted by wrong soil conditions}
##'         \item{(D)}{for all life stages, not enough is better than too much}
##'       }
##'       \itemize{
##'         \item the values give the percentage of surviving individuals to the 
##'         concerned conditions
##'         \item with \code{g}: Germinant, \code{i}: Immature, \code{m}: Mature
##'         \item with \code{L}: low soil, \code{M}: medium soil, \code{H}: 
##'         high soil \cr \cr
##'       }
##'       \strong{\code{| _____ g ____ | _____ i ____ | _____ m ____ |}} \cr
##'       \strong{\code{| _L__ _M_ _H_ | _L__ _M_ _H_ | _L__ _M_ _H_ |}} \cr
##'       \code{______________________________________________} \cr
##'       \code{| 30\% 100\% 10\% | 60\% 100\% 40\% | 90\% 100\% 70\% |} \cr \cr
##'       
##'     \item from \strong{user data} : \cr
##'       \emph{with the values contained within the \code{lifeStage}, 
##'       \code{resources} and \code{tolerance} columns, if provided}
##'   }
##'   }
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/SOIL/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{NAME}{name of the PFG}
##'   \item{ACTIVE_GERM}{germination rates depending on soil conditions
##'   \cr \emph{(integer between \code{0} and \code{100}\%)}}
##'   \item{SOIL_CONTRIB}{contribution to the soil value of the pixel}
##'   \item{SOIL_LOW}{lower value of soil supported by the PFG, \cr 
##'   defining the limit between \code{Low} and \code{Medium} soil resources 
##'   for this PFG}
##'   \item{SOIL_HIGH}{upper value of soil supported by the PFG, \cr 
##'   defining the limit between \code{Medium} and \code{High} soil resources 
##'   for this PFG}
##'   \item{SOIL_TOL}{soil tolerance table (in a single row). \cr 
##'   This is a vector of 9 numbers corresponding to the ability of the PFG to 
##'   survive or not :
##'   \itemize{
##'     \item at different life stages \emph{(Germinant (\code{Ge}), Immature 
##'     (\code{Im}), Mature (\code{Ma}))}
##'     \item under different soil conditions \emph{(Low (\code{L}), Medium 
##'     (\code{M}) or High (\code{H}))}.
##'   }
##'   These parameters should be given in this order : \code{GeL, GeM, GeH, ImL, 
##'   ImM, ImH, MaL, MaM, MaH}
##'   \cr \emph{(integer between \code{0} and \code{100}\%)}. 
##'   \cr \cr}
##' }
##' 
##' A \code{SOIL_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \code{name.simulation/DATA/PFGS/} directory.  
##'
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \code{name.simulation/DATA/PFGS/SOIL/opt.folder.name/}.
##' 
##' 
##' @keywords FATE, simulation, soil tolerance, nitrogen
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}},
##' \code{\link{PRE_FATE.params_PFGsuccession}}
##' 
##' @examples
##' 
##'                                                     
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG soil parameter files (with strategies) -----------------------------------------
##' tab.soil = data.frame(PFG = paste0('PFG', 1:6)
##'                       , strategy_ag = c('rich_lover', 'indifferent' , 'indifferent'
##'                                         , 'rich_lover', 'indifferent', 'poor_lover')
##'                       , strategy_contrib = c('eutrophic', 'mesotrophic', 'mesotrophic'
##'                                              , 'mesotrophic', 'mesotrophic', 'oligotrophic'))
##' tab.tol = data.frame(PFG = paste0('PFG', 1:6)
##'                      , strategy_tol = c('rich_lover', 'ubiquist', 'poor_lover'
##'                                         , 'ubiquist', 'poor_lover', 'poor_lover'))
##' 
##' PRE_FATE.params_PFGsoil(name.simulation = 'FATE_simulation'
##'                         , mat.PFG.soil = tab.soil
##'                         , mat.PFG.tol = tab.tol)
##'                                                         
##' 
##' ## Create PFG soil parameter files (with all values) -----------------------------------------
##' tab.soil = data.frame(PFG = paste0('PFG', 1:6)
##'                       , active_germ_low = c(50, 80, 80, 60, 80, 80)
##'                       , active_germ_medium = rep(90, 6)
##'                       , active_germ_high = c(90, 80, 80, 90, 80, 40)
##'                       , strategy_contrib = c('eutrophic', 'mesotrophic', 'mesotrophic'
##'                                              , 'mesotrophic', 'mesotrophic', 'oligotrophic'))
##' tab.tol = expand.grid(resources = c('Low', 'Medium', 'High')
##'                       , lifeStage = c('Germinant', 'Immature', 'Mature')
##'                       , PFG = paste0('PFG', 1:6))
##' tab.tol$tolerance = c(80, 80, 40, 80, 50, 40, 90, 40, 40
##'                       , rep(90, 9)
##'                       , rep(90, 9)
##'                       , 80, 80, 60, 80, 60, 60, 90, 50, 50
##'                       , 80, 80, 80, 50, 60, 90, 30, 40, 90
##'                       , 80, 80, 80, 50, 50, 90, 50, 50, 90)
##' 
##' PRE_FATE.params_PFGsoil(name.simulation = 'FATE_simulation'
##'                         , mat.PFG.soil = tab.soil
##'                         , mat.PFG.tol = tab.tol)
##'                                                         
##'                                                         
##' ## -------------------------------------------------------------------------------------------
##'
##' ## Load example data
##' Champsaur_params = .loadData('Champsaur_params', 'RData')
##' 
##' ## Create a skeleton folder
##' PRE_FATE.skeletonDirectory(name.simulation = 'FATE_Champsaur')
##' 
##' 
##' ## PFG traits for light
##' tab.soil = Champsaur_params$tab.SOIL
##' str(tab.soil)
##' 
##' ## Create PFG soil parameter files -----------------------------------------------------------
##' PRE_FATE.params_PFGsoil(name.simulation = 'FATE_Champsaur'
##'                            , mat.PFG.soil = tab.soil)
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGsoil = function(
  name.simulation
  , mat.PFG.soil
  , mat.PFG.tol = NULL
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/SOIL/")
  
  ## CHECK parameter mat.PFG.soil
  if (.testParam_notDf(mat.PFG.soil))
  {
    .stopMessage_beDataframe("mat.PFG.soil")
  } else
  {
    if (nrow(mat.PFG.soil) == 0 || !(ncol(mat.PFG.soil) %in% c(3, 5, 7)))
    {
      .stopMessage_numRowCol("mat.PFG.soil", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                               , "(active_germ_high)", "(strategy_ag)", "soil_contrib"
                                               , "soil_tol_min", "soil_tol_max", "(strategy_contrib)"))
    } else
    {
      notCorrect = switch(as.character(ncol(mat.PFG.soil))
                          , "3" = (.testParam_notColnames(mat.PFG.soil, c("PFG", "type", "strategy_contrib")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "strategy_ag", "strategy_contrib")))
                          , "5" = (.testParam_notColnames(mat.PFG.soil, c("PFG", "type", "soil_contrib"
                                                                          , "soil_tol_min", "soil_tol_max")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "active_germ_low"
                                                                            , "active_germ_medium"
                                                                            , "active_germ_high"
                                                                            , "strategy_contrib")) &&
                                     .testParam_notColnames(mat.PFG.soil, c("PFG", "strategy_ag", "soil_contrib"
                                                                            , "soil_tol_min", "soil_tol_max")))
                          , "7" = .testParam_notColnames(mat.PFG.soil, c("PFG", "active_germ_low"
                                                                         , "active_germ_medium"
                                                                         , "active_germ_high", "soil_contrib"
                                                                         , "soil_tol_min", "soil_tol_max"))
                          , TRUE)
      if (notCorrect){
        .stopMessage_columnNames("mat.PFG.soil", c("PFG", "type", "(active_germ_low)", "(active_germ_medium)"
                                                   , "(active_germ_high)", "(strategy_ag)", "soil_contrib"
                                                   , "soil_tol_min", "soil_tol_max", "(strategy_contrib)"))
      }
    }
    mat.PFG.soil$PFG = as.character(mat.PFG.soil$PFG)
    .testParam_samevalues.m("mat.PFG.soil$PFG", mat.PFG.soil$PFG)
    .testParam_notChar.m("mat.PFG.soil$PFG", mat.PFG.soil$PFG)
    if (sum(colnames(mat.PFG.soil) == "type") == 1)
    {
      mat.PFG.soil$type = as.character(mat.PFG.soil$type)
      .testParam_notInValues.m("mat.PFG.soil$type", mat.PFG.soil$type, c("H", "C", "P"))
    }
    if (sum(colnames(mat.PFG.soil) == "active_germ_low") == 1)
    {
      .testParam_notInteger.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low)
      .testParam_notBetween.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low, 0, 100)
      .testParam_notInteger.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium)
      .testParam_notBetween.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium, 0, 100)
      .testParam_notInteger.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high)
      .testParam_notBetween.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high, 0, 100)
    }
    if (sum(colnames(mat.PFG.soil) == "strategy_ag") == 1)
    {
      mat.PFG.soil$strategy_ag = as.character(mat.PFG.soil$strategy_ag)
      .testParam_notInValues.m("mat.PFG.soil$strategy_ag", mat.PFG.soil$strategy_ag
                               , c("poor_lover", "indifferent", "rich_lover"))
    }
    if (sum(colnames(mat.PFG.soil) == "soil_contrib") == 1)
    {
      .testParam_notNum.m("mat.PFG.soil$soil_contrib", mat.PFG.soil$soil_contrib)
      .testParam_NAvalues.m("mat.PFG.soil$soil_contrib", mat.PFG.soil$soil_contrib)
      .testParam_notNum.m("mat.PFG.soil$soil_tol_min", mat.PFG.soil$soil_tol_min)
      .testParam_NAvalues.m("mat.PFG.soil$soil_tol_min", mat.PFG.soil$soil_tol_min)
      .testParam_notNum.m("mat.PFG.soil$soil_tol_max", mat.PFG.soil$soil_tol_max)
      .testParam_NAvalues.m("mat.PFG.soil$soil_tol_max", mat.PFG.soil$soil_tol_max)
      if (sum(mat.PFG.soil$soil_tol_min > mat.PFG.soil$soil_contrib) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.soil$soil_tol_min` must contain "
                    , "values equal or inferior to `mat.PFG.soil$soil_contrib`"))
      }
      if (sum(mat.PFG.soil$soil_tol_max < mat.PFG.soil$soil_contrib) > 0){
        stop(paste0("Wrong type of data!\n `mat.PFG.soil$soil_tol_max` must contain "
                    , "values equal or superior to `mat.PFG.soil$soil_contrib`"))
      }
    }
    if (sum(colnames(mat.PFG.soil) == "strategy_contrib") == 1)
    {
      mat.PFG.soil$strategy_contrib = as.character(mat.PFG.soil$strategy_contrib)
      .testParam_notInValues.m("mat.PFG.soil$strategy_contrib", mat.PFG.soil$strategy_contrib
                               , c("oligotrophic", "mesotrophic", "eutrophic"))
    }
  }
  ## CHECK parameter mat.PFG.tol
  if (!is.null(mat.PFG.tol))
  {
    if (.testParam_notDf(mat.PFG.tol))
    {
      .stopMessage_beDataframe("mat.PFG.tol")
    } else
    {
      if (nrow(mat.PFG.tol) == 0 || !(ncol(mat.PFG.tol) %in% c(2, 4)))
      {
        .stopMessage_numRowCol("mat.PFG.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
      } else
      {
        notCorrect = switch(as.character(ncol(mat.PFG.tol))
                            , "2" = .testParam_notColnames(mat.PFG.tol, c("PFG", "strategy_tol"))
                            , "4" = .testParam_notColnames(mat.PFG.tol, c("PFG", "lifeStage", "resources", "tolerance"))
                            , TRUE)
        if (notCorrect){
          .stopMessage_columnNames("mat.PFG.tol", c("PFG", "lifeStage", "resources", "tolerance", "(strategy_tol)"))
        }
      }
      mat.PFG.tol$PFG = as.character(mat.PFG.tol$PFG)
      .testParam_notChar.m("mat.PFG.tol$PFG", mat.PFG.tol$PFG)
      if (sum(colnames(mat.PFG.tol) == "lifeStage") == 1)
      {
        .testParam_notInValues.m("mat.PFG.tol$lifeStage", mat.PFG.tol$lifeStage, c("Germinant", "Immature", "Mature"))
        .testParam_notInValues.m("mat.PFG.tol$resources", mat.PFG.tol$resources, c("Low", "Medium", "High"))
        
        .testParam_notInteger.m("mat.PFG.tol$tolerance", mat.PFG.tol$tolerance)
        .testParam_NAvalues.m("mat.PFG.tol$tolerance", mat.PFG.tol$tolerance)
        .testParam_notBetween.m("mat.PFG.tol$tolerance", mat.PFG.tol$tolerance, 0, 100)
      }
      if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
      {
        mat.PFG.tol$strategy_tol = as.character(mat.PFG.tol$strategy_tol)
        .testParam_notInValues.m("mat.PFG.tol$strategy_tol", mat.PFG.tol$strategy_tol
                                 , c("poor_lover", "ubiquist", "rich_lover"))
      }
    }
  }
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/PFGS/SOIL/"))

    
  #############################################################################
  
  no.PFG = nrow(mat.PFG.soil)
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.soil$PFG)
  
  #############################################################################
  
  ## GET GERMINATION RATE depending on soil conditions
  ##   = these rates should express a deviation from the
  ##     germination rate in optimal conditions (=100%)
  ##   = for each soil condition (Low, Medium, High)
  ACTIVE_GERM = matrix(100, nrow = 3, ncol = no.PFG)
  
  if (sum(colnames(mat.PFG.soil) == "type") == 1)
  {
    ## woody species have little variation in germination rate depending on soil conditions
    ACTIVE_GERM[c(1,3), which(mat.PFG.soil$type %in% c("C", "P"))] = 90
    ## herbaceous germinate less in richer soil
    ACTIVE_GERM[1, which(mat.PFG.soil$type == "H")] = 80 ## low soil conditions
    ACTIVE_GERM[3, which(mat.PFG.soil$type == "H")] = 50 ## high soil conditions
    
  } else if (sum(colnames(mat.PFG.soil) == "active_germ_low") == 1 ||
             sum(colnames(mat.PFG.soil) == "active_germ_medium") == 1 ||
             sum(colnames(mat.PFG.soil) == "active_germ_high") == 1)
  {
    if (sum(colnames(mat.PFG.soil) == "active_germ_low") == 1)
    {
      ACTIVE_GERM[1, ] = mat.PFG.soil$active_germ_low
    }
    if (sum(colnames(mat.PFG.soil) == "active_germ_medium") == 1)
    {
      ACTIVE_GERM[2, ] = mat.PFG.soil$active_germ_medium
    }
    if (sum(colnames(mat.PFG.soil) == "active_germ_high") == 1)
    {
      ACTIVE_GERM[3, ] = mat.PFG.soil$active_germ_high
    }
    
  } else if (sum(colnames(mat.PFG.soil) == "strategy_ag") == 1)
  {
    for (i in 1:no.PFG){
      ACTIVE_GERM[, i] = switch(mat.PFG.soil$strategy_ag[i]
                                , poor_lover = c(80, 90, 50)
                                , indifferent = c(90, 90, 90)
                                , rich_lover = c(50, 90, 80)
      )
    }
  } else
  {
    warning(paste0("Missing data! The `ACTIVE_GERM` parameter has not been set. "
                   , "Please check."))
  }
  
  #############################################################################
  
  if (sum(colnames(mat.PFG.soil) == "soil_contrib") == 1)
  {
    ## GET SOIL CONTRIBUTION
    SOIL_CONTRIB = as.numeric(mat.PFG.soil$soil_contrib)
    
    ## GET SOIL TOLERANCE LIMITS
    SOIL_LOW = as.numeric(mat.PFG.soil$soil_tol_min)
    SOIL_HIGH = as.numeric(mat.PFG.soil$soil_tol_max)
  } else if (sum(colnames(mat.PFG.soil) == "strategy_contrib") == 1)
  {
    SOIL_CONTRIB = SOIL_LOW = SOIL_HIGH = vector(length = no.PFG)
    for (i in 1:no.PFG){
      tmp = switch(mat.PFG.soil$strategy_contrib[i]
                   , oligotrophic = c(1, 1.5, 2)
                   , mesotrophic = c(2.5, 1.5, 4.5)
                   , eutrophic = c(3, 4, 5)
      )
      SOIL_CONTRIB[i] = tmp[1]
      SOIL_LOW[i] = tmp[2]
      SOIL_HIGH[i] = tmp[3]
    }
  } else
  {
    warning(paste0("Missing data! The `SOIL_CONTRIB`, `SOIL_LOW` and "
                   , "`SOIL_HIGH` parameters have not been set. "
                   , "Please check."))
  }
  
  no.class = seq(min(round(SOIL_CONTRIB))
                 , max(round(SOIL_CONTRIB))
                 , 1)
  
  cat("\n ---------- INFORMATION : SOIL \n")
  cat("\n  Classes : ", no.class)
  cat("\n  Number of classes : ", max(no.class))
  cat("\n  Number of PFG within each class (contribution) : "
      , table(cut(SOIL_CONTRIB, breaks = 1:max(no.class))))
  cat("\n")
  
  
  
  #############################################################################
  
  ## GET SOIL TOLERANCE
  ##    = for each life stage (Germinant, Immature, Mature)
  ##    = for each soil condition (Low, Medium, High)
  SOIL_TOL = matrix(100, nrow = 3 * 3, ncol = no.PFG)
  
  if (is.null(mat.PFG.tol))
  {
    SOIL_TOL[1, ] = 30 ## Germinant - Low soil conditions
    SOIL_TOL[3, ] = 10 ## Germinant - High soil conditions
    
    SOIL_TOL[4, ] = 60 ## Immature - Low soil conditions
    SOIL_TOL[6, ] = 40 ## Immature - High soil conditions
    
    SOIL_TOL[7, ] = 90 ## Mature - Low soil conditions
    SOIL_TOL[9, ] = 70 ## Mature - High soil conditions
  } else
  {
    if (sum(colnames(mat.PFG.tol) == "lifeStage") == 1)
    {
      for (ii in 1:nrow(mat.PFG.tol))
      {
        LS_res = paste0(mat.PFG.tol$lifeStage[ii], "_", mat.PFG.tol$resources[ii])
        ind_ii = switch(LS_res
                        , Germinant_Low = 1
                        , Germinant_Medium = 2
                        , Germinant_High = 3
                        , Immature_Low = 4
                        , Immature_Medium = 5
                        , Immature_High = 6
                        , Mature_Low = 7
                        , Mature_Medium = 8
                        , Mature_High = 9
        )
        SOIL_TOL[ind_ii, which(NAME == mat.PFG.tol$PFG[ii])] = mat.PFG.tol$tolerance[ii]
      }
    } else if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
    {
      for (i in 1:no.PFG){
        SOIL_TOL[, i] = switch(mat.PFG.tol$strategy_tol[i]
                               , poor_lover = c(30, 100, 10, 60, 100, 40, 90, 100, 70)
                               , ubiquist = c(90, 100, 80, 90, 100, 80, 90, 100, 80)
                               , rich_lover = c(10, 100, 30, 40, 100, 60, 70, 100, 90)
        )
      }
    } else
    {
      warning("Missing data! The `SOIL_TOL` parameter has not been set. Please check.")
    }
  }
  
  #############################################################################
  
  names.params.list = mat.PFG.soil$PFG
  names.params.list.sub = c("NAME", "SOIL_CONTRIB"
                            , "SOIL_LOW", "SOIL_HIGH"
                            , "ACTIVE_GERM", "SOIL_TOL")
  
  params.list = lapply(names.params.list.sub, function(x) { return(get(x)) })
  
  params.csv = t(do.call(rbind, params.list))
  colnames(params.csv) = c("NAME"
                           , "SOIL_CONTRIB"
                           , "SOIL_LOW"
                           , "SOIL_HIGH"
                           , paste0("ACTIVE_GERM_for_", c("L", "M", "H"))
                           , paste0("SOIL_TOL_for_",
                                    c("GeL", "GeM", "GeH"
                                      , "ImL", "ImM", "ImH"
                                      , "MaL", "MaM", "MaH"))
  )
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == ""
                                       , ""
                                       , sub("/$", "_", opt.folder.name))
                              , "SOIL_COMPLETE_TABLE.csv")
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
                                       , "/DATA/PFGS/SOIL/"
                                       , opt.folder.name
                                       , "SOIL_"
                                       , names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }
  
  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG soil parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
}
