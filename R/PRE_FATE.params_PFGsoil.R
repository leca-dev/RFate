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
##' \strong{soil competition} within a primary vegetation succession. \cr \cr
##' 
##' Several parameters, given within \code{mat.PFG.soil} or \code{mat.PFG.tol}, 
##' are required for each PFG in order to set up the soil competition :
##' 
##' \describe{
##'   \item{PFG}{the concerned plant functional group \cr \cr}
##'   
##'   \item{type}{or life-form, based on Raunkier. \cr It should be either 
##'   \code{H} (herbaceous), \code{C} (chamaephyte) or \code{P} (phanerophyte) 
##'   for now}
##'   \item{(\emph{active_germ_low})}{an \code{integer} between \code{0} and 
##'   \code{10} corresponding to the proportion of seeds that will germinate for 
##'   \code{Low} soil condition}
##'   \item{(\emph{active_germ_medium})}{an \code{integer} between \code{0} and 
##'   \code{10} corresponding to the proportion of seeds that will germinate for 
##'   \code{Medium} soil condition}
##'   \item{(\emph{active_germ_high})}{an \code{integer} between \code{0} and 
##'   \code{10} corresponding to the proportion of seeds that will germinate for 
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
##'   contribution strategy : \cr \code{ubiquist}, \code{tobedefined} \cr \cr}
##'   
##'   \item{lifeStage}{the concerned life stage (\code{Germinant}, 
##'   \code{Immature}, \code{Mature})}
##'   \item{resources}{the concerned soil condition (\code{Low}, 
##'   \code{Medium}, \code{High})}
##'   \item{tolerance}{an \code{integer} between \code{0} and \code{10} 
##'   corresponding to the proportion of surviving individuals}
##'   \item{(\emph{strategy_tol})}{a \code{string} to choose the tolerance 
##'   strategy : \cr \code{ubiquist}, \code{tobedefined} \cr \cr}
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
##'     \item from \strong{predefined scenarios} (using \code{strategy_ag}) :
##'     \describe{
##'       \item{}{\strong{\code{| _L_ _M_ _H_ |}}}
##'       \item{}{\code{_______________}}
##'       \item{poor_lover}{\code{| 80\% 90\% 50\% |}}
##'       \item{indifferent}{\code{| 90\% 90\% 90\% |}}
##'       \item{rich_lover}{\code{| 50\% 90\% 80\% |}}
##'     }
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
##'       \describe{
##'         \item{}{\strong{\code{| ___ L ___ | ___ M ___ | ___ H ___ |}}}
##'         \item{}{\code{_____________________________________}}
##'         \item{ubiquist}{\code{__________ 1.5 _ 2.5 _ 4.5 __________}}
##'         \item{to be filled}{}
##'       }
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
##'       \describe{
##'         \item{}{\strong{\code{| ___ g ___ | ___ i ___ | ___ m ___ |}}}
##'         \item{}{\strong{\code{| _L _M_ H_ | _L _M_ H_ | _L _M_ H_ |}}}
##'         \item{}{\code{_____________________________________}}
##'         \item{ubiquist}{\code{| 90 100 90 | 90 100 90 | 90 100 90 |}}
##'         \item{to be filled}{}
##'       }
##'     \item from \strong{predefined rules} :
##'       \describe{
##'         \item{(A)}{germinants are severely impacted by wrong soil conditions}
##'         \item{(B)}{immatures are half impacted by wrong soil conditions}
##'         \item{(C)}{matures are little impacted by wrong soil conditions}
##'       }
##'       \itemize{
##'         \item the values give the percentage of surviving individuals to the 
##'         concerned conditions
##'         \item with \code{g}: Germinant, \code{i}: Immature, \code{m}: Mature
##'         \item with \code{L}: low soil, \code{M}: medium soil, \code{H}: 
##'         high soil \cr \cr
##'       }
##'       \describe{
##'         \item{}{\strong{\code{| ___ g ___ | ___ i ___ | ___ m ___ |}}}
##'         \item{}{\strong{\code{| _L _M_ H_ | _L _M_ H_ | _L _M_ H_ |}}}
##'         \item{}{\code{_____________________________________}}
##'         \item{}{\code{| 10 100 30 | 40 100 60 | 70 100 90 |}}
##'       }
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
##'   \cr \emph{(from \code{0} to \code{10}, corresponding to 0 to 100\%)}}
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
##'   \cr \emph{(from \code{0} to \code{10}, corresponding to 0 to 100\%)}. 
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
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' ## Create PFG soil parameter files
##' PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
##'                         , mat.PFG.soil = data.frame(PFG = c("PFG1", "PFG2", "PFG3")
##'                                                     , type = c("H", "H", "C")
##'                                                     , soil_contrib = c(2.5, 3, 4.8)
##'                                                     , soil_tol_min = c(2, 3, 3)
##'                                                     , soil_tol_max = c(3, 3, 6)))
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
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_low", mat.PFG.soil$active_germ_low, 0:10)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_medium", mat.PFG.soil$active_germ_medium, 0:10)
      .testParam_NAvalues.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high)
      .testParam_notInValues.m("mat.PFG.soil$active_germ_high", mat.PFG.soil$active_germ_high, 0:10)
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
                               , c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
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
        .testParam_NAvalues.m("mat.PFG.tol$tolerance", mat.PFG.tol$tolerance)
        .testParam_notInValues.m("mat.PFG.tol$tolerance", mat.PFG.tol$tolerance, 0:10)
      }
      if (sum(colnames(mat.PFG.tol) == "strategy_tol") == 1)
      {
        mat.PFG.tol$strategy_tol = as.character(mat.PFG.tol$strategy_tol)
        .testParam_notInValues.m("mat.PFG.tol$strategy_tol", mat.PFG.tol$strategy_tol
                                 , c("full_light", "pioneer", "ubiquist", "semi_shade", "undergrowth"))
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
  ACTIVE_GERM = matrix(10, nrow = 3, ncol = no.PFG)
  
  if (sum(colnames(mat.PFG.soil) == "type") == 1)
  {
    ## woody species have little variation in germination rate depending on soil conditions
    ACTIVE_GERM[c(1,3), which(mat.PFG.soil$type %in% c("C", "P"))] = 9
    ## herbaceous germinate less in richer soil
    ACTIVE_GERM[1, which(mat.PFG.soil$type == "H")] = 8 ## low soil conditions
    ACTIVE_GERM[3, which(mat.PFG.soil$type == "H")] = 5 ## high soil conditions
    
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
                                , poor_lover = c(8, 9, 5)
                                , indifferent = c(9, 9, 9)
                                , rich_lover = c(5, 9, 8)
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
                   # , full_light = c(1,1,1,0,0,1,0,0,1)
                   # , pioneer = c(1,1,1,0,1,1,0,1,1)
                   , ubiquist = c(2.5, 1, 4)
                   # , semi_shade = c(1,1,0,1,1,0,1,1,1)
                   # , undergrowth = c(1,1,0,1,1,0,1,1,0)
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
  SOIL_TOL = matrix(10, nrow = 3 * 3, ncol = no.PFG)
  
  if (is.null(mat.PFG.tol))
  {
    SOIL_TOL[1, ] = 1 ## Germinant - Low soil conditions
    SOIL_TOL[3, ] = 3 ## Germinant - High soil conditions
    
    SOIL_TOL[4, ] = 4 ## Immature - Low soil conditions
    SOIL_TOL[6, ] = 6 ## Immature - High soil conditions
    
    SOIL_TOL[7, ] = 7 ## Mature - Low soil conditions
    SOIL_TOL[9, ] = 9 ## Mature - High soil conditions
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
                               # , full_light = c(1,1,1,0,0,1,0,0,1)
                               # , pioneer = c(1,1,1,0,1,1,0,1,1)
                               , ubiquist = c(9,10,9,9,10,9,9,10,9)
                               # , semi_shade = c(1,1,0,1,1,0,1,1,1)
                               # , undergrowth = c(1,1,0,1,1,0,1,1,0)
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
