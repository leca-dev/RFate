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
                   # , TRANS.PFG_var, TRANS.ras.var, mod.obj
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
                       , TRANS.PFG_var
                       , "_STRATA_all.tif")
    gp = TRANS.PFG_var[which(file.exists(file_name))]
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
    
    ## Combine with other variables ###############################################################
    ras.all = stack(ras.PFG, TRANS.ras.var)
    
    env.data = as.data.frame(ras.all)
    env.data = na.exclude(env.data)
    for(pfg in TRANS.PFG_var) {
      env.data[which(env.data[, pfg] < 0), pfg] = 0
    }
    
    ### Create all explanatory variables (with interactions between vegetation and environment)
    mess = paste0(c(paste0("env.data$", names(TRANS.ras.var))
                    , paste0("sqrt(env.data$", TRANS.PFG_var, ")")), collapse = ", ")
    eval(parse(text = paste0("X.des = poly(", mess, ", degree = 2, raw = TRUE)")))
    col_power_1 = which(str_count(colnames(X.des), "1") == 1)
    col_power_2 = which(str_count(colnames(X.des), "1") == 0)
    col_inter_var1 = grep("^1.", colnames(X.des))
    col_inter_var2 = grep("^0.1.", colnames(X.des))
    col_toKeep = sort(unique(c(
      col_power_1, col_power_2, col_inter_var1, col_inter_var2
    )))
    X.des = X.des[, col_toKeep]
    
    ### Change explanatory variables into images
    ux = sort(unique(TRANS.quad[, 1])) #x ordinates
    uy = sort(unique(TRANS.quad[, 2])) #y ordinates
    nx = length(ux)
    ny = length(uy)
    col.ref = match(TRANS.quad[, 1], ux) # indice de l'abscisse de quad dans ux
    row.ref = match(TRANS.quad[, 2], uy)
    
    int.list = list()
    for (n in 1:dim(X.des)[2]) {
      all.vec = rep(NA, max(row.ref) * max(col.ref))
      vec.ref = (col.ref - 1) * max(row.ref) + row.ref
      all.vec[vec.ref] = X.des[, n]
      int.list[[n]] = im(matrix(all.vec, max(row.ref), max(col.ref), dimnames = list(uy, ux)),
                         xcol = ux,
                         yrow = uy)
    }
    names(int.list) = paste0("V", 1:dim(X.des)[2])
    
    ## Transform them into RS species distribution map ############################################
    mod.pred = predict(TRANS.ppm, covariates = int.list, ngrid = c(ny, nx))
    ras.pred = raster(mod.pred)
    ras.quality = ras.pred / max(na.exclude(values(ras.pred))) * 100
    tmp = log(ras.quality) + abs(min(log(ras.quality)[], na.rm = TRUE))
    ras.log = tmp / max(tmp[], na.rm = TRUE) * 100
    
    # name.distrib = paste0(name.simulation.RS, "BLABLABLA.asc")
    # writeRaster(ras.log, filename = name.distrib)
    ## SAVE  as ASCII, voir script Emmanuel ?
    
    ## Update RS disp-cost maps ###################################################################
    
    
    ## Update RS patch maps #######################################################################
    ## ADD script
    
    
    ## Run RangeShifter ###########################################################################
    RunRS(RSparams = params.RS, dirpath = name.simulation.RS)
    
    ## RENAME output with year
    
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
    ## MISSING conversion into 0-1 scale (what is the max Nind for which the dist is max ?)
    ## parameter user-defined ?
    name.dist = paste0(name.simulation, "/DATA/MASK/MASK_DIST_YEAR_", ye + 1, ".tif")
    writeRaster(stack_years_rep0, filename = name.dist)

    ## Update FATE disturbance maps ###############################################################
    .setParam(params.lines = abs.simulParam
              , flag = "DIST_MASK"
              , flag.split = "^--.*--$"
              , value = paste0(name.simulation, "/DATA/MASK/MASK_DIST_YEAR_", ye + 1, ".tif"))
    
    ## Update FATE simulation to load #############################################################
    .setParam(params.lines = abs.simulParam
              , flag = "SAVED_STATE"
              , flag.split = "^--.*--$"
              , value = "XXX")
    
  }
}

