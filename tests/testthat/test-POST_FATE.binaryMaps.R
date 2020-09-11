library(RFate)
context("POST_FATE.binaryMaps() function")


## INPUTS
test_that("POST_FATE.binaryMaps gives error with missing data", {
  expect_error(POST_FATE.binaryMaps()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.binaryMaps(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.binaryMaps(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.binaryMaps(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.binaryMaps("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.binaryMaps(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.binaryMaps(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : years", {
  
  ## TEST years : integer
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = "a")
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = factor("a"))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = factor(1))
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = NULL)
               , "`years` must be an integer > 0")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = NA)
               , "`years` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : method", {
  
  ## TEST method : correct value
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = "")
               , "`method` must be either `1` or `2`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = "a")
               , "`method` must be either `1` or `2`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = factor("a"))
               , "`method` must be either `1` or `2`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = NULL)
               , "`method` must be either `1` or `2`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = NA)
               , "`method` must be either `1` or `2`")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : method1.threshold", {
  
  ## TEST method1.threshold : correct value
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 1
                                    , method1.threshold = "")
               , "`method1.threshold` must contain values between `0` and `1`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 1
                                    , method1.threshold = "a")
               , "`method1.threshold` must contain values between `0` and `1`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 1
                                    , method1.threshold = factor("a"))
               , "`method1.threshold` must contain values between `0` and `1`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 1
                                    , method1.threshold = -1)
               , "`method1.threshold` must contain values between `0` and `1`")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 1
                                    , method1.threshold = 1.5)
               , "`method1.threshold` must contain values between `0` and `1`")
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : method2.cutoff", {
  
  ## TEST method2.cutoff : NULL case
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = NULL)
               , "The folder FATE_simulation/RESULTS/ does not contain adequate files")
  
  ## TEST method2.cutoff : not NULL or data.frame case
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = NA)
               , "`method2.cutoff` must be either NULL or a data.frame")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = 1)
               , "`method2.cutoff` must be either NULL or a data.frame")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = matrix(1))
               , "`method2.cutoff` must be either NULL or a data.frame")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = "a")
               , "`method2.cutoff` must be either NULL or a data.frame")
  
  ## TEST method2.cutoff : data.frame case
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = data.frame())
               , "`method2.cutoff` does not have the appropriate number of rows (>0) or columns (year, PFG, cutoff)"
               , fixed = TRUE)
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = data.frame(1,2))
               , "`method2.cutoff` does not have the appropriate number of rows (>0) or columns (year, PFG, cutoff)"
               , fixed = TRUE)
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , file.simulParam = "ParamSimul.txt"
                                    , years = 1
                                    , method = 2
                                    , method2.cutoff = data.frame(1, 2, 3))
               , "Column names of `method2.cutoff` must be `year`, `PFG` and `cutoff`"
               , fixed = TRUE)
})

## INPUTS
test_that("POST_FATE.binaryMaps gives error with wrong data : rasters", {
  {
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
  }
  
  
  ## TEST RESULTS folder
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , years = 1
                                    , method = 1)
               , "The folder FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/ does not contain adequate files")
  file.create("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/Abund_YEAR_1_PFG1_STRATA_1.tif")
  
  # expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
  #                                   , years = 1
  #                                   , method = 1)
  #              , "Cannot create a RasterLayer object from this file.")
})



## OUTPUTS
test_that("POST_FATE.binaryMaps gives correct outputs : warnings", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    
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
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_1.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_1.tif"), overwrite = TRUE)
    
    POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
  }
  
  ## TEST output : method 2
  file.create("FATE_simulation/RESULTS/POST_FATE_TABLE_YEAR_1_validationStatistics_SIMUL_V0.csv")
  expect_error(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                    , years = 1
                                    , method = 2)
               , "The folder FATE_simulation/RESULTS/ does not contain adequate files")
})


## OUTPUTS
test_that("POST_FATE.binaryMaps gives correct outputs : correct", {
  {
    if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
    PRE_FATE.skeletonDirectory()
    
    library(raster)
    map_0 = raster(nrows = 5, ncols = 5, resolution = 50)
    map_0[] = 0
    map_1 = map_0
    map_1[] = 1
    writeRaster(map_1, filename = "FATE_simulation/DATA/MASK/map_mask.tif", overwrite = TRUE)
    
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
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_1.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_2.tif"), overwrite = TRUE)
    }
    writeRaster(map_0, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_all.tif"), overwrite = TRUE)
    writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_perStrata/"
                                         , "Abund_YEAR_1_PFG6_STRATA_1.tif"), overwrite = TRUE)
    
    POST_FATE.relativeAbund(name.simulation = "FATE_simulation", years = 1)
    
    
    mat.PFG.obs = xyFromCell(map_0, 1:ncell(map_0))
    mat.PFG.obs = expand.grid(PFG = paste0("PFG",1:6)
                              , X = mat.PFG.obs[, 1]
                              , Y = mat.PFG.obs[, 2])
    mat.PFG.obs$obs = sample(c(0, 1), nrow(mat.PFG.obs), prob = c(0.6, 0.4), replace = TRUE)
    
    validStats = POST_FATE.graphic_validationStatistics(name.simulation = "FATE_simulation"
                                                        , years = 1
                                                        , mat.PFG.obs = mat.PFG.obs)
  }
  
  ## TEST output : method 1
  expect_message(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                      , years = 1
                                      , method = 1)
                 , "> Binary_YEAR_1_PFG1_STRATA_1.tif")
  
  ## TEST output : method 2, NULL
  file.remove(list.files("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/"
                         , full.names = TRUE))
  file.remove(list.files("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata/"
                         , full.names = TRUE))
  expect_message(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                      , years = 1
                                      , method = 2)
                 , "> Binary_YEAR_1_PFG1_STRATA_1.tif")
  
  ## TEST output : method 2, data.frame
  file.remove(list.files("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_allStrata/"
                         , full.names = TRUE))
  file.remove(list.files("FATE_simulation/RESULTS/SIMUL_V1/BIN_perPFG_perStrata/"
                         , full.names = TRUE))
  expect_message(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                      , years = 1
                                      , method = 2
                                      , method2.cutoff = data.frame(year = 1, PFG = "PFG1", cutoff = 1))
                 , "> Binary_YEAR_1_PFG1_STRATA_1.tif")
  expect_warning(POST_FATE.binaryMaps(name.simulation = "FATE_simulation"
                                      , years = 1
                                      , method = 2
                                      , method2.cutoff = data.frame(year = 1, PFG = "PFG1", cutoff = 1))
                 , "No cutoff for year 1, PFG PFG2.\n No binary maps will be produced!")
  
})

