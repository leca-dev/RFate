### HEADER #####################################################################
##' @title Create binary maps for each Plant Functional Group for one (or 
##' several) specific year of a \code{FATE} simulation
##' 
##' @name POST_FATE.binaryMaps
##'
##' @author Maya Guéguen
##' 
##' @description This script is designed to produce raster maps of PFG presence 
##' / absence for one (or several) specific \code{FATE} simulation year.
##' 
##' @param name.simulation a \code{string} corresponding to the main directory 
##' or simulation name of the \code{FATE} simulation
##' @param file.simulParam default \code{NULL}. \cr A \code{string} 
##' corresponding to the name of a parameter file that will be contained into 
##' the \code{PARAM_SIMUL} folder of the \code{FATE} simulation
##' @param years an \code{integer}, or a \code{vector} of \code{integer}, 
##' corresponding to the simulation year(s) that will be used to extract PFG 
##' abundance maps
##' @param method an \code{integer} to choose the transformation method : \cr 
##' \code{1} (relative abundance) or \code{2} (optimizing TSS) (see 
##' \href{POST_FATE.binaryMaps#details}{\code{Details}})
##' @param method1.threshold default \code{0.05}. \cr If \code{method = 1}, 
##' minimum relative abundance required for each PFG to be considered as present 
##' in the concerned pixel 
##' @param method2.cutoff default \code{NULL}. \cr If \code{method = 2}, a 
##' \code{data.frame} with 3 columns : \code{year}, \code{PFG}, \code{cutoff} \cr
##' (see \href{POST_FATE.binaryMaps#details}{\code{Details}})
##' @param opt.no_CPU (\emph{optional}) default \code{1}. \cr The number of 
##' resources that can be used to parallelize the \code{unzip/zip} of raster 
##' files
##' 
##' 
##' @details 
##' 
##' This function allows to obtain, for a specific \code{FATE} simulation and 
##' a specific parameter file within this simulation, \strong{raster maps of PFG 
##' presence / absence}. \cr \cr
##' 
##' 
##' For each PFG and each selected simulation year, raster maps are retrieved 
##' from the results folder \code{ABUND_REL_perPFG_allStrata} and lead to the 
##' production of as many maps as those found :
##' 
##' \describe{
##'   \item{\code{1} fixed threshold}{relative abundance maps are transformed 
##'   into binary maps according to the threshold given by 
##'   \code{method1.threshold} : 
##'   \deqn{abund\_rel_{\text{ PFG}_i} > \text{method1.threshold} \;\; 
##'   \Leftrightarrow \;\; 1}}
##'   \item{\code{2} optimizing TSS}{relative abundance maps are transformed 
##'   into binary maps according to the \href{.getCutoff}{\code{cutoff}} found 
##'   with the \code{\link{POST_FATE.graphic_validationStatistics}} function : 
##'   \deqn{abund\_rel_{\text{ PFG}_i} > \text{method2.cutoff}_{\text{ PFG}_i} \;\; 
##'   \Leftrightarrow \;\; 1}}
##' }
##' 
##' Binary maps per stratum are obtained by multiplying raster maps from 
##' \code{ABUND_perPFG_perStrata} folder by corresponding raster maps from 
##' \code{BIN_perPFG_allStrata} folder.
##' 
##' \strong{It requires} that the \code{\link{POST_FATE.relativeAbund}} 
##' function has been run and that the folder \code{ABUND_REL_perPFG_allStrata} 
##' exists. \cr If \code{method = 2}, it requires that the 
##' \code{\link{POST_FATE.graphic_validationStatistics}} function has been run. 
##' \cr \cr
##' 
##' \strong{These binary \code{raster} files can then be used by other 
##' functions} :
##' 
##' \itemize{
##'   \item to produce graphics of \emph{PFG modelled presence} \code{vs} 
##'   \emph{PFG Habitat Suitability} maps \cr (see 
##'   \code{\link{POST_FATE.graphic_mapPFGvsHS}})
##' }
##' 
##' 
##' @return 
##' Two folders are created :
##' \describe{
##'   \item{\file{BIN_perPFG \cr_allStrata}}{containing presence / absence 
##'   raster maps for each PFG across all strata}
##'   \item{\file{BIN_perPFG \cr_perStrata}}{containing presence / absence 
##'   raster maps for each PFG for each stratum}
##' }
##' 
##' 
##' @keywords FATE, outputs, binary
##' 
##' @seealso \code{\link{POST_FATE.relativeAbund}},
##' \code{\link{POST_FATE.graphic_validationStatistics}}
##' \code{\link{.getCutoff}},
##' \code{\link{POST_FATE.graphic_mapPFGvsHS}}
##' 
##' @examples
##' 
##'                                                         
##' ## ----------------------------------------------------------------------------------------- ##
##' ## Load example data
##'                                                                          
##' 
##' @export
##' 
##' @importFrom foreach foreach %do%
##' @importFrom raster stack writeRaster
##'
## END OF HEADER ###############################################################


POST_FATE.binaryMaps = function(
  name.simulation
  , file.simulParam = NULL
  , years
  , method
  , method1.threshold = 0.05
  , method2.cutoff = NULL
  , opt.no_CPU = 1
){
  
  #############################################################################
  
  ## CHECK parameter name.simulation
  .testParam_existFolder(name.simulation, "PARAM_SIMUL/")
  .testParam_existFolder(name.simulation, "RESULTS/")
  .testParam_existFolder(name.simulation, "DATA/")
  name.simulation = sub("/", "", name.simulation)
  ## CHECK parameter file.simulParam
  abs.simulParams = .getParam_abs.simulParams(file.simulParam, name.simulation)
  ## CHECK parameter years
  .testParam_notInteger.m("years", years)
  ## CHECK parameter method
  .testParam_notInValues.m("method", method, c(1, 2))
  ## CHECK parameter method1.threshold
  if (method == 1)
  {
    .testParam_notBetween.m("method1.threshold", method1.threshold, 0, 1)
  }
  ## CHECK parameter method2.cutoff
  if (method == 2)
  {
    if (is.null(method2.cutoff))
    {
      valid.files = list.files(paste0(name.simulation, "/RESULTS/")
                               , pattern = paste0("POST_FATE_TABLE_YEAR_"
                                                  , years, "_validationStatistics"
                                                  , collapse = "|")
                               , full.names = FALSE)
      if (length(valid.files) == 0)
      {
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/RESULTS/ does not contain adequate files "
                    , "(starting by `POST_FATE_TABLE_YEAR_[...]_validationStatistics`)"))
      }
    } else if (is.data.frame(method2.cutoff))
    {
      if (nrow(method2.cutoff) == 0 || ncol(method2.cutoff) != 3)
      {
        .stopMessage_numRowCol("method2.cutoff", c("year", "PFG", "cutoff"))
      } else
      {
        if (.testParam_notColnames(method2.cutoff, c("year", "PFG", "cutoff"))){
          .stopMessage_columnNames("method2.cutoff", c("year", "PFG", "cutoff"))
        }
      }
      mat.cutoff = method2.cutoff
    } else
    {
      stop("Wrong type of data!\n `method2.cutoff` must be either NULL or a data.frame")
    }
  }
  
  cat("\n\n #------------------------------------------------------------#")
  cat("\n # POST_FATE.binaryMaps")
  cat("\n #------------------------------------------------------------# \n")
  
  
  #############################################################################
  
  res = foreach (abs.simulParam = abs.simulParams) %do%
  {
    
    cat("\n+++++++\n")
    cat("\n  Simulation name : ", name.simulation)
    cat("\n  Simulation file : ", abs.simulParam)
    cat("\n")
    
    ## Get results directories ------------------------------------------------
    .getGraphics_results(name.simulation  = name.simulation
                         , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    .getGraphics_PFG(name.simulation  = name.simulation
                     , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    .getGraphics_mask(name.simulation  = name.simulation
                      , abs.simulParam = abs.simulParam)
    
    ## Get list of arrays and extract years of simulation ---------------------
    years = sort(unique(as.numeric(years)))
    no_years = length(years)
    
    if (method == 1)
    {
      mat.cutoff = expand.grid(year = years
                               , PFG = PFG
                               , cutoff = method1.threshold
                               , stringsAsFactors = FALSE)
    } else if (exists("valid.files"))
    {
      valid.files = valid.files[grep(basename(dir.save), valid.files)]
      if (length(valid.files) > 0)
      {
        mat.cutoff = foreach(fi = valid.files, .combine = "rbind") %do%
        {
          ye = sub("_validationStatistics.*", "", fi)
          ye = sub("POST_FATE_TABLE_YEAR_", "", ye)
          tab = fread(paste0(name.simulation, "/RESULTS/", fi))
		  tab = as.data.frame(tab, stringsAsFactors = FALSE)
          tab = unique(tab[, c("PFG", "cutoff")])
          tab$year = as.numeric(ye)
          return(tab)
        }
      } else
      {
        stop(paste0("Missing data!\n The folder ", name.simulation
                    , "/RESULTS/ does not contain adequate files "
                    , "(starting by `POST_FATE_TABLE_YEAR_[...]_validationStatistics` "
                    , "and corresponding to `", basename(dir.save), "` folder)"))
      }
    }
    
    ## UNZIP the raster saved -------------------------------------------------
    raster.perPFG.perStrata = .getRasterNames(years, "perStrata", "ABUND")
    .unzip(folder_name = dir.output.perPFG.perStrata
           , list_files = raster.perPFG.perStrata
           , no_cores = opt.no_CPU)
    
    
    
    ## get the data inside the rasters ----------------------------------------
    cat("\n ---------- GETTING PRESENCE/ABSENCE maps for")
    for (y in years)
    {
      cat("\n> year", y)
      
      file_name = paste0(dir.output.perPFG.allStrata.REL
                         , "Abund_relative_YEAR_"
                         , y
                         , "_"
                         , PFG
                         , "_STRATA_all.tif")
      gp = PFG[which(file.exists(file_name))]
      file_name = file_name[which(file.exists(file_name))]
      
      if (length(file_name) > 0)
      {
        ras = stack(file_name) * ras.mask
        names(ras) = gp
        
        for (fg in gp)
        {
          ## Produce binary maps ------------------------------------
          ## ALL STRATA
          new_name = paste0(dir.output.perPFG.allStrata.BIN
                            , "Binary_YEAR_"
                            , y
                            , "_"
                            , fg
                            , "_STRATA_all.tif")
          ind.cutoff = which(mat.cutoff$year == y & mat.cutoff$PFG == fg)
          if (length(ind.cutoff) > 0)
          {
            cutoff = mat.cutoff$cutoff[ind.cutoff]
            ras.bin = ras[[fg]]
            ras.bin[] = ifelse(ras.bin[] >= cutoff, 1, 0)
            writeRaster(x = ras.bin
                        , filename = new_name
                        , overwrite = TRUE)
            
            ## SEPARATED STRATA
            prev_names = list.files(path = dir.output.perPFG.perStrata
                                    , pattern = paste0("Abund_YEAR_"
                                                       , y
                                                       , "_"
                                                       , fg
                                                       , "_STRATA")
                                    , full.names = TRUE)
            prev_names = prev_names[grep(".tif$", prev_names)]
            if (length(prev_names) > 0)
            {
              new_names = sub(dir.output.perPFG.perStrata
                              , dir.output.perPFG.perStrata.BIN
                              , prev_names)
              new_names = sub("Abund_YEAR_", "Binary_YEAR_", new_names)
              ras.bin.str = stack(prev_names)
              ras.bin.str = ras.bin.str * ras.bin
              writeRaster(x = ras.bin.str
                          , filename = new_names
                          , overwrite = TRUE
                          , bylayer = TRUE)
              
              message(paste0("\n The output files \n"
                             , paste0(" > ", basename(new_names), " \n"
                                      , collapse = "")
                             , "have been successfully created !\n"))
            }
          } else
          {
            warning(paste0("Missing data!\n No cutoff for year ", y, ", PFG ", fg
                           , ".\n No binary maps will be produced!"))
          }
        } ## END loop on PFG
      } ## END condition file_name
    } ## END loop on years
    cat("\n")
    
    
    ## ZIP the raster saved ---------------------------------------------------
    .zip(folder_name = dir.output.perPFG.perStrata
         , list_files = raster.perPFG.perStrata
         , no_cores = opt.no_CPU)
    
    cat("\n> Done!\n")
    
  } ## END loop on abs.simulParams
}
