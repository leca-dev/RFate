### HEADER #####################################################################
##' @title Create \emph{DISPERSAL} parameter files for a \code{FATE} 
##' simulation
##' 
##' @name PRE_FATE.params_PFGdispersal
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to create parameter files containing 
##' dispersal distances for each PFG (one file for each of them) used in the 
##' dispersal module of \code{FATE}.
##'              
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param mat.PFG.disp a \code{data.frame} with 4 columns : \code{PFG}, 
##' \code{d50}, \code{d99}, \code{ldd}
##' @param opt.folder.name (\emph{optional}) \cr a \code{string} corresponding 
##' to the name of the folder that will be created into the 
##' \code{name.simulation/DATA/PFGS/DISP/} directory to store the results
##' 
##' 
##' @details
##' 
##' A \strong{dispersal module} is available to make the \code{FATE} model 
##' spatially explicit by dispersing seeds of each PFG with a kernel (see 
##' \code{\link{PRE_FATE.params_globalParameters}}). \cr \cr
##' 
##' Dispersal distances are needed for each PFG to quantify the amount of seeds 
##' dispersed into 3 different concentric circles :
##' 
##' \describe{
##'   \item{d50}{the distance at which \code{50\%} of seeds are dispersed}
##'   \item{d99}{the distance at which \code{99\%} of seeds are dispersed}
##'   \item{ldd}{the long dispersal distance at which \code{100\%} of seeds are dispersed}
##' }
##' 
##' 
##' 
##' @return A \code{.txt} file per PFG into the 
##' \code{name.simulation/DATA/PFGS/DISP/} directory with the following 
##' parameters :
##' 
##' \describe{
##'   \item{DISPERS_DIST}{dispersal distances (3 values) \emph{(in meters)} 
##'   \cr \cr}
##' }
##' 
##' 
##' A \file{DISP_COMPLETE_TABLE.csv} file summarizing information for all 
##' groups into the \file{name.simulation/DATA/PFGS/} directory.
##' 
##' If the \code{opt.folder.name} has been used, the files will be into the 
##' folder \file{name.simulation/DATA/PFGS/DISP/opt.folder.name/}.
##' 
##' 
##' @keywords FATE, simulation, dispersal distance
##' 
##' @seealso \code{\link{PRE_FATE.skeletonDirectory}}, 
##' \code{\link{PRE_FATE.params_globalParameters}}
##' 
##' @examples
##' 
##' ## Create a skeleton folder with the default name ('FATE_simulation')
##' PRE_FATE.skeletonDirectory()
##' 
##' mat.disp = data.frame(PFG = paste0('PFG', 1:6)
##'                       , d50 = c(10, 25, 100, 100, 500, 1000)
##'                       , d99 = c(500, 600, 300, 300, 1250, 1200)
##'                       , ldd = c(1500, 1500, 900, 900, 1500, 2000))
##' 
##' ## Create PFG dispersal parameter files -----------------------------------------------------
##' PRE_FATE.params_PFGdispersal(name.simulation = 'FATE_simulation'
##'                              , mat.PFG.disp = mat.disp)
##'                                                         
##'                                                         
##' ## -------------------------------------------------------------------------------------------
##'
##' # ## Load example data
##' # Champsaur_PFG = .loadData('Champsaur_PFG', 'RData')
##' #
##' # ## Build PFG traits for dispersal
##' # tab.traits = Champsaur_PFG$PFG.traits
##' # ## Dispersal values
##' # ##   = Short: 0.1-2m;    Medium: 40-100m;    Long: 400-500m
##' # ##   = Vittoz correspondance : 1-3: Short;    4-5: Medium;   6-7:Long
##' # corres = data.frame(dispersal = 1:7
##' #                     , d50 = c(0.1, 0.5, 2, 40, 100, 400, 500)
##' #                     , d99 = c(1, 5, 15, 150, 500, 1500, 5000)
##' #                     , ldd = c(1000, 1000, 1000, 5000, 5000, 10000, 10000))
##' # tab.traits$d50 = corres$d50[tab.traits$dispersal]
##' # tab.traits$d99 = corres$d99[tab.traits$dispersal]
##' # tab.traits$ldd = corres$ldd[tab.traits$dispersal]
##' # str(tab.traits)
##' 
##' 
##' ## Load example data
##' Champsaur_params = .loadData('Champsaur_params', 'RData')
##' 
##' ## Create a skeleton folder
##' PRE_FATE.skeletonDirectory(name.simulation = 'FATE_Champsaur')
##' 
##' 
##' ## PFG traits for dispersal
##' tab.disp = Champsaur_params$tab.DISP
##' str(tab.disp)
##' 
##' ## Create PFG dispersal parameter files ------------------------------------------------------
##' PRE_FATE.params_PFGdispersal(name.simulation = 'FATE_Champsaur'
##'                              , mat.PFG.disp = Champsaur_params$tab.DISP)
##' 
##' 
##' 
##' @export
##' 
##' @importFrom utils write.table
##'
## END OF HEADER ###############################################################


PRE_FATE.params_PFGdispersal = function(
  name.simulation
  , mat.PFG.disp
  , opt.folder.name = NULL
){
  
  #############################################################################
  
  .testParam_existFolder(name.simulation, "DATA/PFGS/DISP/")
  
  ## CHECK parameter mat.PFG.disp
  if (.testParam_notDf(mat.PFG.disp))
  {
    .stopMessage_beDataframe("mat.PFG.disp")
  } else
  {
    if (nrow(mat.PFG.disp) == 0 || ncol(mat.PFG.disp) != 4)
    {
      .stopMessage_numRowCol("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
    } else if (.testParam_notColnames(mat.PFG.disp, c("PFG", "d50", "d99", "ldd")))
    {
      .stopMessage_columnNames("mat.PFG.disp", c("PFG", "d50", "d99", "ldd"))
    }
    mat.PFG.disp$PFG = as.character(mat.PFG.disp$PFG)
    .testParam_samevalues.m("mat.PFG.disp$PFG", mat.PFG.disp$PFG)
    .testParam_notChar.m("mat.PFG.disp$PFG", mat.PFG.disp$PFG)
    .testParam_notNum.m("mat.PFG.disp$d50", mat.PFG.disp$d50)
    .testParam_NAvalues.m("mat.PFG.disp$d50", mat.PFG.disp$d50)
    .testParam_notNum.m("mat.PFG.disp$d99", mat.PFG.disp$d99)
    .testParam_NAvalues.m("mat.PFG.disp$d99", mat.PFG.disp$d99)
    .testParam_notNum.m("mat.PFG.disp$ldd", mat.PFG.disp$ldd)
    .testParam_NAvalues.m("mat.PFG.disp$ldd", mat.PFG.disp$ldd)
  }
  ## CHECK parameter opt.folder.name
  opt.folder.name = .getParam_opt.folder.name(opt.folder.name
                                              , paste0(name.simulation, "/DATA/PFGS/DISP/"))

    
  #############################################################################
  
  ## GET PFG NAME
  NAME = as.character(mat.PFG.disp$PFG)

  ## GET DISPERSAL DISTANCES
  DISPERS_DIST = mat.PFG.disp[ , c("d50", "d99", "ldd"), drop = F]
  
  #############################################################################
  
  names.params.list = mat.PFG.disp$PFG
  names.params.list.sub = c("NAME", "DISPERS_DIST")
  
  params.csv = mat.PFG.disp
  colnames(params.csv) = c("NAME"
                           , paste0("DISPERS_DIST_", c("d50", "d99", "ldd")))
  for (i in grep("DIST", colnames(params.csv))) params.csv[, i] = as.integer(params.csv[,i])
  
  write.table(params.csv
              , file = paste0(name.simulation
                              , "/DATA/PFGS/"
                              , ifelse(opt.folder.name == ""
                                       , ""
                                       , sub("/$", "_", opt.folder.name))
                              , "DISP_COMPLETE_TABLE.csv")
              , row.names = FALSE
              , col.names = TRUE)
  
  #############################################################################
  
  params.list = lapply(1:nrow(mat.PFG.disp), function(x) {
    lapply(names.params.list.sub, function(y) {
      val = get(y)
      if (is.null(nrow(val))){
        val = val[x]
      } else {
        val = as.integer(val[x, ])
      }
      return(val)
    })
  })
  
  for (i in 1:length(params.list)) {
    params = params.list[[i]]
    names(params) = names.params.list.sub
    
    .createParams(params.file = paste0(name.simulation,
                                       "/DATA/PFGS/DISP/"
                                       , opt.folder.name
                                       , "DISP_",
                                       names.params.list[i],
                                       ".txt")
                  , params.list = params)
  }

  cat("\n> Done!\n")
  cat("\n  Complete table of information about PFG dispersal parameters can be find in "
      , paste0(name.simulation, "/DATA/PFGS/"), "folder.")
  cat("\n")
}
