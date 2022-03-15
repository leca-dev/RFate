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
##' @importFrom raster stack rasterFromXYZ writeRaster rasterToPolygons disaggregate rasterize
##' @importFrom sf st_as_sf st_buffer st_union st_area st_write
##' @importFrom rmapshaper ms_explode
##' @importFrom spatstat im predict
##' @importFrom stringr str_count
##' @importFrom scales rescale
##' 
## END OF HEADER ###############################################################


FATE_RS = function(name.simulation, file.simulParam, opt.no_CPU = 1, verbose.level = 2
                   , PPM.names_PFG, PPM.ras_env, PPM.xy, PPM.mod
                   , PATCH.threshold = 80, PATCH.buffer = 500, PATCH.min_m2 = 300000
                   , COST.ras_elev, COST.ras_barr, COST.wei = c(0.43, 0.53, 0.04), COST.range = c(1, 10)
                   , name.simulation.RS, params.RS
                   , POP.carrying_capacity = 0.115
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
    
    ## GET PFG abundance maps (all strata) ------------------------------------
    file_name = paste0(GLOB_DIR$dir.output.perPFG.allStrata
                       , "Abund_YEAR_"
                       , year.step
                       , "_"
                       , PPM.names_PFG
                       , "_STRATA_all.tif")
    gp = PPM.names_PFG[which(file.exists(file_name))]
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
    ras.all = stack(ras.PFG, PPM.ras_env)
    
    env.data = as.data.frame(ras.all)
    env.data = na.exclude(env.data)
    for(pfg in PPM.names_PFG) {
      env.data[which(env.data[, pfg] < 0), pfg] = 0
    }
    
    ### Create all explanatory variables (with interactions between vegetation and environment)
    mess = paste0(c(paste0("env.data$", names(PPM.ras_env))
                    , paste0("sqrt(env.data$", PPM.names_PFG, ")")), collapse = ", ")
    eval(parse(text = paste0("X.des = poly(", mess, ", degree = 2, raw = TRUE)")))
    col_power_1 = which(str_count(colnames(X.des), "1") == 1)
    col_power_2 = which(str_count(colnames(X.des), "1") == 0)
    col_inter_var1 = grep("^1.", colnames(X.des))
    col_inter_var2 = grep("^0.1.", colnames(X.des))
    col_toKeep = sort(unique(c(
      col_power_1, col_power_2, col_inter_var1, col_inter_var2
    )))
    X.des = X.des[, col_toKeep]
    
    ### Change explanatory variables into images ------------------------------
    ux = sort(unique(PPM.xy[, 1])) #x ordinates
    uy = sort(unique(PPM.xy[, 2])) #y ordinates
    nx = length(ux)
    ny = length(uy)
    col.ref = match(PPM.xy[, 1], ux) # indice de l'abscisse de quad dans ux
    row.ref = match(PPM.xy[, 2], uy)
    
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
    mod.pred = predict(PPM.mod, covariates = int.list, ngrid = c(ny, nx))
    ras.pred = raster(mod.pred)
    ras.quality = ras.pred / max(na.exclude(values(ras.pred))) * 100
    tmp = log(ras.quality) + abs(min(log(ras.quality)[], na.rm = TRUE))
    ras.log = tmp / max(tmp[], na.rm = TRUE) * 100

    
    ## Update RS patch maps #######################################################################
    ras.patch = ras.log > PATCH.threshold # defining patches
    ras.patch[ras.patch[] < 1] = NA # keep only patches
    
    pol.patch = rasterToPolygons(ras.patch, dissolve = TRUE) # converting into polygons
    pol.patch = disaggregate(pol.patch) # separating into multiple polygons
    
    pol.patch = st_as_sf(pol.patch)
    pol.patch_buffer = st_buffer(pol.patch, dist = PATCH.buffer)
    pol.patch_buffer = st_union(pol.patch_buffer) # merging overlapping polygons
    pol.patch_buffer = ms_explode(pol.patch_buffer) # separating into multiple polygons
    
    pol.patch_buffer$area_m2 = as.numeric(st_area(pol.patch_buffer))
    pol.patch_buffer = pol.patch_buffer[which(pol.patch_buffer$area_m2 > PATCH.min_m2)] # excluding too small patches
    
    st_write(pol.patch_buffer
             , paste0("tampons", PATCH.buffer, "m_suit_HS", PATCH.threshold, "percent_filtre", PATCH.min_m2 / 10000, "ha_Lambert93")
             , dsn = paste0(name.simulation.RS, "Inputs"), driver = "ESRI Shapefile")
    
    
    ## SAVE HS RASTER MAP AS ASCII ################################################################
    ras.patch_buffer = rasterize(pol.patch_buffer, ras.log)
    ras.log[which(is.na(ras.patch_buffer[]))] = NA
    
    val.log = ras.log[]
    val.log[is.na(val.log)] = -9
    val.log = as.character(val.log)
    seq_linestarts <- seq(from = nrow(ras.log), to = ncell(ras.log) - nrow(ras.log) + 1, by = nrow(ras.log))
    val.log[seq_linestarts] = paste0(val.log[seq_linestarts], "\n") # delineating rows for future character string
    cha.log = paste(val.log, collapse = ' ') # concatenating in 1 character
    cha.log = gsub("\n ", "\n", cha.log) # removing blank from line starts
    
    file.rename(from = paste0(name.simulation.RS, "Inputs/LandscapeFile_HabSuit_ChamoisBauges.txt")
                , to = paste0(name.simulation.RS, "Inputs/LandscapeFile_HabSuit_ChamoisBauges_YEAR_", ye - 1, ".txt"))
    
    fileConn <- file(paste0(name.simulation.RS, "Inputs/LandscapeFile_HabSuit_ChamoisBauges.txt"))
    writeLines(c(paste("ncols", ncol(ras.log), sep = " "),
                 paste("nrows", nrow(ras.log), sep = " "),
                 paste("xllcorner", extent(ras.log)[1], sep = " "),
                 paste("yllcorner", extent(ras.log)[3], sep = " "),
                 paste("cellsize", resolution(ras.log)[1], sep = " "),
                 "NODATA_value -9",
                 cha.log), 
               fileConn)
    close(fileConn)
    
    
    ## Update RS disp-cost maps ###################################################################
    ## Function to rescale habitat suitability maps
    fun_rescale = function(a = 1, b = 100, ras, ext) {
      mini = min(values(ras), na.rm = TRUE)
      maxi = max(values(ras), na.rm = TRUE)
      res = (((b - a) * (ras - mini)) / (maxi - mini)) + a
      res = mask(res, ext)
      return(res)
    }
    
    ras.log_ = fun_rescale(a = a, b = b, ras = ras.log, ext = ras.log)
    ras.HS = fun_rescale(a = a, b = b, ras = 100 + (-1 * ras.log_) ^ 1, ext = ras.log_)
    
    # Compute cost map --------------------------------------------------------
    ras.cost <- COST.wei[1] * ras.HS + COST.wei[2] * COST.ras_elev + COST.wei[3] * COST.ras_barr
    ras.cost[] <- rescale(ras.cost[], c(COST.range[1], COST.range[2]))
    ras.cost[] <- round(ras.cost[]) # round for SMS module
    
    ## SAVE COST RASTER MAP AS ASCII ##############################################################
    val.cost = ras.cost[]
    val.cost[is.na(val.cost)] = -9
    val.cost = as.character(val.cost)
    seq_linestarts <- seq(from = nrow(ras.cost), to = ncell(ras.cost) - nrow(ras.cost) + 1, by = nrow(ras.cost))
    val.cost[seq_linestarts] = paste0(val.cost[seq_linestarts], "\n") # delineating rows for future character string
    cha.cost = paste(val.cost, collapse = ' ') # concatenating in 1 character
    cha.cost = gsub("\n ", "\n", cha.cost) # removing blank from line starts
    
    file.rename(from = paste0(name.simulation.RS, "Inputs/LandscapeFile_CostMap_ChamoisBauges.txt")
                , to = paste0(name.simulation.RS, "Inputs/LandscapeFile_CostMap_ChamoisBauges_YEAR_", ye - 1, ".txt"))
    
    fileConn <- file(paste0(name.simulation.RS, "Inputs/LandscapeFile_CostMap_ChamoisBauges.txt"))
    writeLines(c(paste("ncols", ncol(ras.cost), sep = " "),
                 paste("nrows", nrow(ras.cost), sep = " "),
                 paste("xllcorner", extent(ras.cost)[1], sep = " "),
                 paste("yllcorner", extent(ras.cost)[3], sep = " "),
                 paste("cellsize", resolution(ras.cost)[1], sep = " "),
                 "NODATA_value -9",
                 cha.cost), 
               fileConn)
    close(fileConn)
    
    
    ## Run RangeShifter ###########################################################################
    RunRS(RSparams = params.RS, dirpath = name.simulation.RS)
    
    
    ## Get resulting RS species density ###########################################################
    ## Code from RangeShiftR help
    # read population output file into a dataframe
    pop_df = readPop(s = params.RS, dirpath = name.simulation.RS)
    
    file.rename(from = paste0(name.simulation.RS, "Outputs/Batch1_Sim1_Land1_Pop.txt")
                , to = paste0(name.simulation.RS, "Outputs/Batch1_Sim1_Land1_Pop_YEAR_", ye, ".txt"))
    
    # Make stack of different raster layers for each year and for only one repetition (Rep==0):
    pop_wide_rep0 = reshape(subset(pop_df, Rep == 0)[, c('Year', 'x', 'y', 'NInd')]
                            , timevar = 'Year'
                            , v.names = c('NInd')
                            , idvar = c('x', 'y')
                            , direction = 'wide')
    tmp_ncells = reshape(subset(pop_df, Rep == 0)[, c('Year', 'x', 'y', 'Ncells')]
                  , timevar = 'Year'
                  , v.names = c('Ncells')
                  , idvar = c('x', 'y')
                  , direction = 'wide')
    
    # use raster package to make a raster from the data frame
    stack_years_rep0 = rasterFromXYZ(pop_wide_rep0)
    stack_ncells = rasterFromXYZ(tmp_ncells)
    stack_years_rep0 = stack_years_rep0 / stack_ncells
    stack_years_rep0 = stack_years_rep0 / POP.carrying_capacity
    stack_years_rep0[which(stack_years_rep0[] > 1)] = 1
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
              , value = paste0("SIMUL_YEAR_", ye))
    
  }
}

