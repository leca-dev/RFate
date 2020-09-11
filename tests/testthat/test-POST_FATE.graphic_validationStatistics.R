library(RFate)
context("POST_FATE.graphic_validationStatistics() function")


## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with missing data", {
  expect_error(POST_FATE.graphic_validationStatistics()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_validationStatistics(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_validationStatistics(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : years", {
  
  ## TEST years : integer
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , years = "a")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , years = factor("a"))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , years = factor(1))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , years = NULL)
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , file.simulParam = "ParamSimul.txt"
                                                      , years = NA)
               , "`years` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : mat.PFG.obs", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
  
  
  ## TEST mat.PFG.obs : data.frame
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10)
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = NA)
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = NULL)
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = "")
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = 1)
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = factor(1))
               , "`mat.PFG.obs` must be a data.frame")
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = matrix(1))
               , "`mat.PFG.obs` must be a data.frame")
  
  ## TEST mat.PFG.obs : correct number of rows and columns
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame())
               , "`mat.PFG.obs` does not have the appropriate number of rows (>0) or columns (PFG, X, Y, obs)"
               , fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(1))
               , "`mat.PFG.obs` does not have the appropriate number of rows (>0) or columns (PFG, X, Y, obs)"
               , fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(1,2,3,4,5))
               , "`mat.PFG.obs` does not have the appropriate number of rows (>0) or columns (PFG, X, Y, obs)"
               , fixed = TRUE)
  
  ## TEST mat.PFG.obs : correct names of columns
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(1,2,3,4))
               , "Column names of `mat.PFG.obs` must be `PFG`, `X`, `Y` and `obs`"
               , fixed = TRUE)
  
  
  ## TEST mat.PFG.obs$PFG : length > 0
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = "", X = 1, Y = 1, obs =0))
               , "`mat.PFG.obs$PFG` must contain a character value of length > 0", fixed = TRUE)
  
  
  ## TEST mat.PFG.obs$X : numeric values
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = NA, Y = 1, obs = 0))
               , "Missing data!\n Too many NA values. Please check.", fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = "a", Y = 1, obs = 0))
               , "`mat.PFG.obs$X` must contain numeric values", fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = factor(1), Y = 1, obs = 0))
               , "`mat.PFG.obs$X` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.PFG.obs$Y : numeric values
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = NA, obs = 0))
               , "Missing data!\n Too many NA values. Please check.", fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = "a", obs = 0))
               , "`mat.PFG.obs$Y` must contain numeric values", fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = factor(1), obs = 0))
               , "`mat.PFG.obs$Y` must contain numeric values", fixed = TRUE)
  
  
  ## TEST mat.PFG.obs$obs : correct values
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = 1, obs = -1))
               , "`mat.PFG.obs$obs` must be either `0` or `1`", fixed = TRUE)
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 10
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = 1, obs = 1.5))
               , "`mat.PFG.obs$obs` must be either `0` or `1`", fixed = TRUE)
})


## INPUTS
test_that("POST_FATE.graphic_validationStatistics gives error with wrong data : rasters", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  library(raster)
  map_mask = raster(nrows = 5, ncols = 5)
  map_mask[] = 1
  writeRaster(map_mask, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
  
  PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                   , required.no_PFG = 6
                                   , required.no_strata = 5
                                   , required.simul_duration = 100
                                   , required.seeding_duration = 10
                                   , required.seeding_timestep = 1
                                   , required.seeding_input = 100
                                   , required.max_abund_low = 3000
                                   , required.max_abund_medium = 5000
                                   , required.max_abund_high = 9000)
  
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG", 1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
  
  PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                  , name.MASK = "map_mask.tif")
  
  dir.create("FATE_simulation/RESULTS/SIMUL_V1")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
  
  ## TEST PFG names
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 1
                                                      , mat.PFG.obs = data.frame(PFG = 1, X = 1, Y = 1, obs = 1))
               , "The names of PFG within `mat.PFG.obs` is different from the names of PFG contained from FATE_simulation/DATA/PFGS/SUCC/")
  
  
  ## TEST RESULTS folder
  expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 1
                                                      , mat.PFG.obs = data.frame(PFG = "PFG1", X = 1, Y = 1, obs = 1))
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/ does not contain adequate files")
  
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif")
  # expect_error(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
  #                                                     , years = 1
  #                                                     , mat.PFG.obs = data.frame(PFG = "PFG1", X = 1, Y = 1, obs = 1))
  #              , "Cannot create a RasterLayer object from this file.")
})





## OUTPUTS
test_that("POST_FATE.graphic_validationStatistics gives correct outputs : warnings", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  library(raster)
  map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
  map_0[] = 0
  map_1 = map_0
  map_1[] = 1
  map_hab = map_0
  map_hab[] = sample(c(1, 5, 10), ncell(map_hab), replace = TRUE)
  writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
  writeRaster(map_hab, filename = "FATE_simulation/DATA/MASK/map_hab.tif", overwrite = TRUE)
  
  PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                   , required.no_PFG = 6
                                   , required.no_strata = 5
                                   , required.simul_duration = 100
                                   , required.seeding_duration = 10
                                   , required.seeding_timestep = 1
                                   , required.seeding_input = 100
                                   , required.max_abund_low = 3000
                                   , required.max_abund_medium = 5000
                                   , required.max_abund_high = 9000)
  
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
  
  PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                  , name.MASK = "map_mask.tif")
  
  dir.create("FATE_simulation/RESULTS/SIMUL_V1")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
  dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
  
  for (i in 1:5)
  {
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
  }
  writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                       , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
  
  POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
  
  
  ## TEST output
  expect_warning(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                        , years = 1
                                                        , mat.PFG.obs = data.frame(PFG = paste0("PFG",1:6)
                                                                                   , X = -144
                                                                                   , Y = 72
                                                                                   , obs = 1))
                 , "No values for PFG PFG1.\n No validation statistics will be produced!")
  expect_warning(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                        , years = 1
                                                        , mat.PFG.obs = data.frame(PFG = paste0("PFG",1:6)
                                                                                   , X = -144
                                                                                   , Y = 72
                                                                                   , obs = 1))
                 , "No validation has been calculated for year 1!")
  
  
  ## TEST habitat
  expect_warning(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                        , years = 1
                                                        , mat.PFG.obs = data.frame(PFG = paste0("PFG",1:6)
                                                                                   , X = -144
                                                                                   , Y = 72
                                                                                   , obs = 1)
                                                        , opt.ras_habitat = "FATE_simulation/DATA/MASK/map_hab.tif")
                 , "No validation has been calculated for year 1!")
  
})


## OUTPUTS
test_that("POST_FATE.graphic_validationStatistics gives correct outputs : correct", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    map_hab = map_0
    map_hab[] = sample(c(1, 5, 10), ncell(map_hab), replace = TRUE)
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    writeRaster(map_hab, filename = "FATE_simulation/DATA/MASK/map_hab.tif", overwrite = TRUE)
    
    PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                     , required.no_PFG = 6
                                     , required.no_strata = 5
                                     , required.simul_duration = 100
                                     , required.seeding_duration = 10
                                     , required.seeding_timestep = 1
                                     , required.seeding_input = 100
                                     , required.max_abund_low = 3000
                                     , required.max_abund_medium = 5000
                                     , required.max_abund_high = 9000)
    
    PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                  , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                              , type = c("C", "C", "H", "H", "P", "P")
                                                              , height = c(10, 250, 36, 68, 1250, 550)
                                                              , maturity = c(5, 5, 3, 3, 8, 9)
                                                              , longevity = c(12, 200, 25, 4, 110, 70)))
    
    PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                    , name.MASK = "map_mask.tif")
    
    dir.create("FATE_simulation/RESULTS/SIMUL_V1")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/LIGHT")
    dir.create("FATE_simulation/RESULTS/SIMUL_V1/SOIL")
    
    for (i in 1:5)
    {
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    
    POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
    
    
    mat.PFG.obs = xyFromCell(map_0, 1:ncell(map_0))
    mat.PFG.obs = expand.grid(PFG = paste0("PFG",1:6)
                              , X = mat.PFG.obs[, 1]
                              , Y = mat.PFG.obs[, 2])
    mat.PFG.obs$obs = sample(c(0, 1), nrow(mat.PFG.obs), prob = c(0.6, 0.4), replace = TRUE)
  }
  
  ## TEST output
  expect_message(POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                        , years = 1
                                                        , mat.PFG.obs = mat.PFG.obs)
                 , "The output file POST_FATE_TABLE_YEAR_1_validationStatistics_SIMUL_V1.csv has been successfully created !")
  
  
  validStats = POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                      , years = 1
                                                      , mat.PFG.obs = mat.PFG.obs)
  
  expect_output(str(validStats), "List")
  expect_equal(length(validStats), 1)
  
  expect_output(str(validStats[[1]]), "List")
  expect_equal(length(validStats[[1]]), 2)
  expect_output(str(validStats[[1]]$tab), "List")
  expect_equal(length(validStats[[1]]$tab), 1)
  expect_output(str(validStats[[1]]$tab[[1]]), "data.frame")
  expect_output(str(validStats[[1]]$tab[[1]]), "7 variables")
  
})

