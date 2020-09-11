library(RFate)
context("POST_FATE.graphic_evolutionStability() function")


## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with missing data", {
  expect_error(POST_FATE.graphic_evolutionStability()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionStability(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionStability(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : name.simulation", {
  expect_error(POST_FATE.graphic_evolutionStability(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionStability("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionStability(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(POST_FATE.graphic_evolutionStability(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : folders", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  dir.create("FATE_simulation/")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL/")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS/")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/ folder")
  dir.create("FATE_simulation/DATA/")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : file.simulParam", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST file.simulParam : correct content
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = NULL)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = NA)
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "")
               , "The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
  
  ## TEST file.simulParam : correct value
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "toto")
               , "`FATE_simulation/PARAM_SIMUL/toto` does not exist")
  file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : movingWindow_size", {
  
  ## TEST movingWindow_size : integer
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_size = "a")
               , "`movingWindow_size` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_size = factor("a"))
               , "`movingWindow_size` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_size = factor(1))
               , "`movingWindow_size` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_size = NULL)
               , "`movingWindow_size` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_size = NA)
               , "`movingWindow_size` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : movingWindow_step", {
  
  ## TEST movingWindow_step : integer
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_step = "a")
               , "`movingWindow_step` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_step = factor("a"))
               , "`movingWindow_step` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_step = factor(1))
               , "`movingWindow_step` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_step = NULL)
               , "`movingWindow_step` must be an integer > 0")
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                    , file.simulParam = "ParamSimul.txt"
                                                    , movingWindow_step = NA)
               , "`movingWindow_step` must be an integer > 0")
})

## INPUTS
test_that("POST_FATE.graphic_evolutionStability gives error with wrong data : csv file", {
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
  }
  
  ## TEST RESULTS folder
  expect_error(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
               , "FATE_simulation/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_SIMUL_V1.csv` does not exist")
})




## OUTPUTS
test_that("POST_FATE.graphic_evolutionStability gives correct outputs :", {
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
    
    for (i in 1:6)
    {
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_1_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
    }
    tempEvol = POST_FATE.temporalEvolution(name.simulation = "FATE_simulation"
                                           , no_years = 10
                                           , opt.ras_habitat = "FATE_simulation/DATA/MASK/map_hab.tif")
  }
  
  expect_message(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                      , opt.doPlot = FALSE)
                 , "> POST_FATE_TABLE_HAB_evolution_stability1_SIMUL_V1")
  expect_warning(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                      , opt.doPlot = FALSE)
                 , "No moving window can be defined with the current parameters and available data")
  
  
  {
    for (i in 1:6)
    {
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_2_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_3_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
      writeRaster(map_1, filename = paste0("FATE_simulation/RESULTS/SIMUL_V1/ABUND_perPFG_allStrata/"
                                           , "Abund_YEAR_5_PFG", i, "_STRATA_all.tif"), overwrite = TRUE)
    }
    tempEvol = POST_FATE.temporalEvolution(name.simulation = "FATE_simulation"
                                           , no_years = 10
                                           , opt.ras_habitat = "FATE_simulation/DATA/MASK/map_hab.tif")
  }
  
  expect_message(POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation"
                                                      , opt.doPlot = FALSE)
                 , "> POST_FATE_TABLE_HAB_evolution_stability2_SIMUL_V1")
  
  resStab = POST_FATE.graphic_evolutionStability(name.simulation = "FATE_simulation")
  expect_output(str(resStab), "List")
  expect_equal(length(resStab), 1)
  
  expect_output(str(resStab[[1]]), "List")
  expect_equal(length(resStab[[1]]), 3)
  expect_output(str(resStab[[1]]$tab.hab), "data.frame")
  expect_output(str(resStab[[1]]$tab.hab), "5 variables")
  expect_output(str(resStab[[1]]$tab.stab), "data.frame")
  expect_output(str(resStab[[1]]$tab.stab), "9 variables")
  expect_output(str(resStab[[1]]$plot.stab), "List")
})
