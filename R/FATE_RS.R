### HEADER #####################################################################
##' @title S
##' 
##' @name FATE_RS
##'
##' @author Maya Guéguen
##' 
##' 
##' @description ...
##'              
##'  
##' @keywords RangeShifter
##'  
##' @export
##' 
##' @importFrom RangeShiftR RunRS readPop
##' @importFrom raster stack rasterFromXYZ writeRaster
##' 
## END OF HEADER ###############################################################


FATE_RS = function(name.simulation, file.simulParam, opt.no_CPU = 1, verbose.level = 2
                   , name.simulation.RS, params.RS
                   , year.start, year.end, year.step)
{
  ## + argument pour savoir si on commence par FATE ou RS
  
  ## LOOP over simulation years
  for (ye in seq(year.start, year.end, 1))
  {
    .setParam(params.lines = abs.simulParam
              , flag = "SAVING_DIR"
              , flag.split = "^--.*--$"
              , value = paste0(name.simulation, "/RESULTS/SIMUL_YEAR_", ye, ""))
    
    ## Run FATE ###################################################################################
    FATE(simulParam = abs.simulParam, no_CPU = opt.no_CPU, verboseLevel = verbose.level)
      
    ## Get resulting FATE vegetation maps #########################################################
    ## Get results directories ------------------------------------------------
    GLOB_DIR = .getGraphics_results(name.simulation  = name.simulation
                                    , abs.simulParam = abs.simulParam)
    
    ## Get number of PFGs -----------------------------------------------------
    ## Get PFG names ----------------------------------------------------------
    GLOB_SIM = .getGraphics_PFG(name.simulation  = name.simulation
                                , abs.simulParam = abs.simulParam)
    
    ## Get raster mask --------------------------------------------------------
    GLOB_MASK = .getGraphics_mask(name.simulation  = name.simulation
                                  , abs.simulParam = abs.simulParam)
    
    ## UNZIP the raster saved -------------------------------------------------
    raster.perPFG.allStrata = .getRasterNames(year.step, "allStrata", "ABUND", GLOB_DIR)
    .unzip(folder_name = GLOB_DIR$dir.output.perPFG.allStrata
           , list_files = raster.perPFG.allStrata
           , no_cores = opt.no_CPU)
    
    ## GET PFG abundance maps (all strata) ------------------------------
    file_name = paste0(GLOB_DIR$dir.output.perPFG.allStrata
                       , "Abund_YEAR_"
                       , year.step
                       , "_"
                       , GLOB_SIM$PFG
                       , "_STRATA_all.tif")
    gp = GLOB_SIM$PFG[which(file.exists(file_name))]
    file_name = file_name[which(file.exists(file_name))]
    
    if (length(file_name) > 0)
    {
      ras.PFG = stack(file_name) * GLOB_MASK$ras.mask
      names(ras.PFG) = gp
    } else
    {
      stop(paste0("Missing data!\n The folder "
                  , GLOB_DIR$dir.output.perPFG.allStrata
                  , " does not contain adequate files"))
    }
    
    ## Transform them into habitat map ############################################################
    
    ## Transform them into RS species distribution map ############################################
    
    ## Update RS disp-cost and patch maps #########################################################
    
    ## Run RangeShifter ###########################################################################
    RunRS(RSparams = params.RS, dirpath = name.simulation.RS)
    
    ## Get resulting RS species density ###########################################################
    ## Code from RangeShiftR help
    # read population output file into a dataframe
    pop_df <- readPop(s = params.RS, dirpath = name.simulation.RS)
    
    # Make stack of different raster layers for each year and for only one repetition (Rep==0):
    pop_wide_rep0 <- reshape(subset(pop_df, Rep == 0)[, c('Year', 'x', 'y', 'NInd')]
                             , timevar = 'Year'
                             , v.names = c('NInd')
                             , idvar = c('x', 'y')
                             , direction = 'wide')
    
    # use raster package to make a raster from the data frame
    stack_years_rep0 <- rasterFromXYZ(pop_wide_rep0)
    name.dist = paste0(name.simulation, "/DATA/MASK/MASK_DIST_YEAR_", ye, ".tif")
    writeRaster(stack_years_rep0, filename = name.dist)

    ## Update FATE disturbance maps ###############################################################
    .setParam(params.lines = abs.simulParam
              , flag = "DIST_MASK"
              , flag.split = "^--.*--$"
              , value = paste0(name.simulation, "/DATA/MASK/MASK_DIST_YEAR_", ye, ".tif"))
    
    ## Update FATE simulation to load #############################################################
    .setParam(params.lines = abs.simulParam
              , flag = "SAVED_STATE"
              , flag.split = "^--.*--$"
              , value = "XXX")
    
  }
}

