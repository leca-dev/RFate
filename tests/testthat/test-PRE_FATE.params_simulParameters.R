library(RFate)
context("PRE_FATE.params_simulParameters() function")

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with missing data", {
  expect_error(PRE_FATE.params_simulParameters()
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NA)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(NULL)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.simulation", {
  expect_error(PRE_FATE.params_simulParameters(1)
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters("a")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(factor(1))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  expect_error(PRE_FATE.params_simulParameters(matrix(seq(2), ncol=2))
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
})

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : name.MASK", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  PRE_FATE.skeletonDirectory()
  
  ## TEST name.MASK : length > 0
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.MASK` must contain a character value of length > 0")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = NULL)
               , "`name.MASK` must contain a character value of length > 0")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = NA)
               , "`name.MASK` must contain a character value of length > 0")
  
  ## TEST name.MASK : exist
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask")
               , "`FATE_simulation/DATA/MASK/mask` does not exist")
})



## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : folders and files 1", {
  if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
  
  ## TEST folders existence
  dir.create("FATE_simulation")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
  dir.create("FATE_simulation/PARAM_SIMUL")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/GLOBAL_PARAMETERS/ folder")
  dir.create("FATE_simulation/DATA")
  dir.create("FATE_simulation/DATA/GLOBAL_PARAMETERS")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/SAVE/ folder")
  dir.create("FATE_simulation/DATA/SAVE")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/SCENARIO/ folder")
  dir.create("FATE_simulation/DATA/SCENARIO")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/MASK/ folder")
  dir.create("FATE_simulation/DATA/MASK")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SUCC/ folder")
  dir.create("FATE_simulation/DATA/PFGS")
  dir.create("FATE_simulation/DATA/PFGS/SUCC")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.simulation` does not exist or does not contain a RESULTS/ folder")
  dir.create("FATE_simulation/RESULTS")
  
})

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : folders and files 2", {
  
  ## TEST name.MASK : length > 0
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation")
               , "`name.MASK` must contain a character value of length > 0")
  file.create("FATE_simulation/DATA/MASK/mask.tif")
  
  
  ## Create a Global_parameters file
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is no adequate file (`.txt` file starting with `Global_parameters`) into the DATA/GLOBAL_PARAMETERS/ folder"
               , fixed = TRUE)
  PRE_FATE.params_globalParameters(name.simulation = "FATE_simulation"
                                   , required.no_PFG = 6
                                   , required.no_strata = 5
                                   , required.simul_duration = 100
                                   , required.seeding_duration = c(10,50)
                                   , required.seeding_timestep = 1
                                   , required.seeding_input = 100
                                   , required.max_abund_low = 3000
                                   , required.max_abund_medium = 5000
                                   , required.max_abund_high = 9000
                                   , doHabSuitability = TRUE
                                   , HABSUIT.mode = 1
                                   , doDisturbances = TRUE
                                   , DIST.no = 1
                                   , DIST.no_sub = 4
                                   , DIST.freq = 1
                                   , doDispersal = TRUE
                                   , DISPERSAL.mode = 1
                                   , doLight = TRUE
                                   , LIGHT.thresh_medium = 3000
                                   , LIGHT.thresh_low = 9000
                                   , doSoil = TRUE
                                   , SOIL.init = 2.5
                                   , SOIL.retention = 0.5
                                   , doDrought = TRUE
                                   , DROUGHT.no_sub = 2
                                   , doAliens = TRUE
                                   , ALIEN.no = 1
                                   , ALIEN.freq = 1
                                   , doFire = TRUE
                                   , FIRE.no = 1
                                   , FIRE.no_sub = 4
                                   , FIRE.freq = 1
                                   , FIRE.ignit_mode = 1
                                   , FIRE.ignit_no = 10
                                   , FIRE.neigh_mode = 1
                                   , FIRE.prop_mode = 1
                                   , FIRE.prop_intensity = 0.5
                                   , FIRE.quota_mode = 4)
  
  
  ## Create PFG succession parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `SUCC`) into the FATE_simulation/DATA/PFGS/SUCC/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGsuccession(name.simulation = "FATE_simulation"
                                , mat.PFG.succ = data.frame(PFG = paste0("PFG",1:6)
                                                            , type = c("C", "C", "H", "H", "P", "P")
                                                            , height = c(10, 250, 36, 68, 1250, 550)
                                                            , maturity = c(5, 5, 3, 3, 8, 9)
                                                            , longevity = c(12, 200, 25, 4, 110, 70)))
  
  
  ## Create PFG light parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/LIGHT/ folder")
  dir.create("FATE_simulation/DATA/PFGS/LIGHT")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `LIGHT`) into the FATE_simulation/DATA/PFGS/LIGHT/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGlight(name.simulation = "FATE_simulation"
                           , mat.PFG.light = data.frame(PFG = paste0("PFG",1:6)
                                                        , type = c("C", "C", "H", "H", "P", "P")
                                                        , light_need = c(1, 1, 1, 2, 2, 3)))
  
  
  ## Create PFG soil parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/SOIL/ folder")
  dir.create("FATE_simulation/DATA/PFGS/SOIL")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `SOIL`) into the FATE_simulation/DATA/PFGS/SOIL/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGsoil(name.simulation = "FATE_simulation"
                          , mat.PFG.soil = data.frame(PFG = paste0("PFG",1:6)
                                                      , type = c("C", "C", "H", "H", "P", "P")
                                                      , soil_contrib = c(1.5, 1.2, 1, 2.6, 2.3, 3.9)
                                                      , soil_tol_min = c(1, 1, 1, 2, 2, 3)
                                                      , soil_tol_max = c(3, 3, 3, 3, 3, 4)))
  
  
  ## Create PFG dispersal parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DISP/ folder")
  dir.create("FATE_simulation/DATA/PFGS/DISP")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `DISP`) into the FATE_simulation/DATA/PFGS/DISP/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGdispersal(name.simulation = "FATE_simulation"
                               , mat.PFG.disp = data.frame(PFG = paste0("PFG", 1:6)
                                                           , d50 = rep(c(500, 500, 100),2)
                                                           , d99 = rep(c(10000, 15000, 20000),2)
                                                           , ldd = rep(c(100000, 50000, 100000),2)))
  
  
  ## Create PFG dist parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DIST/ folder")
  dir.create("FATE_simulation/DATA/PFGS/DIST")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `DIST`) into the FATE_simulation/DATA/PFGS/DIST/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                 , mat.PFG.tol = data.frame(nameDist = "graz"
                                                            , responseStage = rep(c(1,2), 6)
                                                            , PFG = paste0("PFG", 1:6)
                                                            , killedIndiv = rep(c(1, 3), 6)
                                                            , resproutIndiv = rep(c(4, 9), 6)))
  
  ## Create PFG drought parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/DROUGHT/ folder")
  dir.create("FATE_simulation/DATA/PFGS/DROUGHT")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `DROUGHT`) into the FATE_simulation/DATA/PFGS/DROUGHT/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGdrought(name.simulation = "FATE_simulation"
                             , mat.PFG.tol = data.frame(nameDist = "immediate"
                                                        , responseStage = rep(c(1,2), 6)
                                                        , PFG = paste0("PFG", 1:6)
                                                        , killedIndiv = rep(c(1, 3), 6)
                                                        , resproutIndiv = rep(c(4, 9), 6))
                             , mat.PFG.drought = data.frame(PFG = paste0("PFG", 1:6)
                                                            , threshold_moderate = c(2, 5, 3, 5, 1, 4)
                                                            , threshold_severe = c(1, 2, 2, 4, 1, 3)
                                                            , counter_recovery = 1
                                                            , counter_sens = 2
                                                            , counter_cum = 3))
  
  ## Create PFG fire parameter files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/FIRE/ folder")
  dir.create("FATE_simulation/DATA/PFGS/FIRE")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is not the same number of files (`.txt` file starting with `FIRE`) into the FATE_simulation/DATA/PFGS/FIRE/ folder as the number of PFG indicated into the file"
               , fixed = TRUE)
  PRE_FATE.params_PFGdisturbance(name.simulation = "FATE_simulation"
                                 , opt.folder.name = "FIRE"
                                 , mat.PFG.tol = data.frame(nameDist = "fire"
                                                            , responseStage = rep(c(1,2), 6)
                                                            , PFG = paste0("PFG", 1:6)
                                                            , killedIndiv = rep(c(1, 3), 6)
                                                            , resproutIndiv = rep(c(4, 9), 6)))
  for(i in list.files("FATE_simulation/DATA/PFGS/DIST/FIRE/"))
  {
    file.copy(paste0("FATE_simulation/DATA/PFGS/DIST/FIRE/", i)
              , paste0("FATE_simulation/DATA/PFGS/FIRE/", sub("^DIST_", "FIRE_", i)))
    file.remove(paste0("FATE_simulation/DATA/PFGS/DIST/FIRE/", i))
  }
  if (dir.exists("FATE_simulation/DATA/PFGS/DIST/FIRE")) unlink("FATE_simulation/DATA/PFGS/DIST/FIRE", recursive = TRUE)
  
  
  ## TEST name.DIST : length > 0
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "`name.DIST` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST name.DIST : exist
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif")
               , "Wrong name file given!\n `FATE_simulation/DATA/MASK/dist.tif` does not exist"
               , fixed = TRUE)
  file.create("FATE_simulation/DATA/MASK/dist.tif")
  
  ## TEST name.DROUGHT : length > 0
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif")
               , "`name.DROUGHT` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST name.DROUGHT : exist
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif")
               , "Wrong name file given!\n `FATE_simulation/DATA/MASK/drought.tif` does not exist"
               , fixed = TRUE)
  file.create("FATE_simulation/DATA/MASK/drought.tif")
  
  ## TEST name.FIRE : length > 0
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif")
               , "`name.FIRE` must contain a character value of length > 0"
               , fixed = TRUE)
  
  ## TEST name.FIRE : exist
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif")
               , "Wrong name file given!\n `FATE_simulation/DATA/MASK/fire.tif` does not exist"
               , fixed = TRUE)
  file.create("FATE_simulation/DATA/MASK/fire.tif")
  
  
  ## TEST ..._changingmask_years : no files
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/HABSUIT/ folder")
  dir.create("FATE_simulation/DATA/PFGS/HABSUIT")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif")
               , "`name.simulation` does not exist or does not contain a DATA/PFGS/ALIENS/ folder")
  dir.create("FATE_simulation/DATA/PFGS/ALIENS")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "There is no adequate file(s) into some folder(s)"
                 , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.params_simulParameters gives correct outputs 1", {
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt has been successfully created !")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V2.1.txt has been successfully created !")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "`params.file` (FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt) already exists. It will be replaced."
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "`params.file` (FATE_simulation/PARAM_SIMUL/Simul_parameters_V2.1.txt) already exists. It will be replaced."
                 , fixed = TRUE)
})


## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : folders and files 3", {
  
  ## Create a SAVE_year_maps or/and SAVE_year_objects parameter file
  PRE_FATE.params_savingYears(name.simulation = "FATE_simulation"
                              , years.maps = c(100, 150, 200)
                              , years.objects = 200)
  file.create("FATE_simulation/DATA/SAVE/SAVE_YEARS_maps_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `SAVE_YEARS_maps`) into the folder FATE_simulation/DATA/SAVE"
               , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SAVE/SAVE_YEARS_maps_BIS.txt")
  file.create("FATE_simulation/DATA/SAVE/SAVE_YEARS_objects_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `SAVE_YEARS_objects`) into the folder FATE_simulation/DATA/SAVE"
               , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SAVE/SAVE_YEARS_objects_BIS.txt")
  
  PRE_FATE.params_savingYears(name.simulation = "FATE_simulation"
                              , years.maps = c(100, 150, 200)
                              , years.objects = 200
                              , opt.folder.name = "Scen1")
  PRE_FATE.params_savingYears(name.simulation = "FATE_simulation"
                              , years.maps = c(100, 150, 200)
                              , years.objects = 200
                              , opt.folder.name = "Scen2")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt has been successfully created !")
})

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : folders and files 4", {
  
  ## Create a changingmask_years parameter file
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "MASK"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  file.create("FATE_simulation/DATA/SCENARIO/MASK_changingmask_years_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `MASK_changingmask_years`) into the folder FATE_simulation/DATA/SCENARIO"
               , fixed = TRUE) 
  file.remove("FATE_simulation/DATA/SCENARIO/MASK_changingmask_years_BIS.txt")
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "HABSUIT"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  file.create("FATE_simulation/DATA/SCENARIO/HABSUIT_changingmask_years_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif")
               , "There is too many adequate files (`.txt` file starting with `HABSUIT_changingmask_years`) into the folder FATE_simulation/DATA/SCENARIO"
               , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SCENARIO/HABSUIT_changingmask_years_BIS.txt")
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "DIST"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif")))
  file.create("FATE_simulation/DATA/SCENARIO/DIST_changingmask_years_BIS.txt")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation", name.MASK = "mask.tif")
               , "There is too many adequate files (`.txt` file starting with `DIST_changingmask_years`) into the folder FATE_simulation/DATA/SCENARIO"
               , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SCENARIO/DIST_changingmask_years_BIS.txt")
  
  file.remove(list.files("FATE_simulation/DATA/SCENARIO/"))
  dir.create("FATE_simulation/DATA/SCENARIO/Scen1")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "MASK_changingmask_years[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "HABSUIT_changingmask_years[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "DIST_changingmask_years[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  
  file.create("FATE_simulation/DATA/SCENARIO/Scen1/MASK_changingmask_years.txt")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "MASK_changingmask_files[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SCENARIO/Scen1/MASK_changingmask_years.txt")
  file.create("FATE_simulation/DATA/SCENARIO/Scen1/HABSUIT_changingmask_years.txt")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "HABSUIT_changingmask_files[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SCENARIO/Scen1/HABSUIT_changingmask_years.txt")
  file.create("FATE_simulation/DATA/SCENARIO/Scen1/DIST_changingmask_years.txt")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "DIST_changingmask_files[...].txt (folder FATE_simulation/DATA/SCENARIO/Scen1)"
                 , fixed = TRUE)
  file.remove("FATE_simulation/DATA/SCENARIO/Scen1/DIST_changingmask_years.txt")
  
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "MASK"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_MASK")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt has been successfully created !")
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "HABSUIT"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_HS")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt has been successfully created !")
  PRE_FATE.params_changingYears(name.simulation = "FATE_simulation"
                                , type.changing = "DIST"
                                , mat.changing = data.frame(year = c(50,50,80,80)
                                                            , order = c(1,2,1,2)
                                                            , new.value = c("MASK_50.tif"
                                                                            , "MASK_50.tif"
                                                                            , "MASK_80.tif"
                                                                            , "MASK_80.tif"))
                                , opt.folder.name = "Scen_DIST")
  expect_message(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "The parameter file FATE_simulation/PARAM_SIMUL/Simul_parameters_V1.1.txt has been successfully created !")
})

## INPUTS
test_that("PRE_FATE.params_simulParameters gives error with wrong data : folders and files 5", {  
  dir.create("FATE_simulation/DATA/PFGS/HABSUIT/Scen1")
  file.create("FATE_simulation/DATA/PFGS/HABSUIT/Scen1/Mask_PFG1.tif")
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif")
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
  
})


## OUTPUTS
test_that("PRE_FATE.params_simulParameters gives correct outputs 2 : opt.global.name", {
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif"
                                                 , opt.global.name = "")
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
  
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif"
                                                 , opt.global.name = 1)
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
})


## OUTPUTS
test_that("PRE_FATE.params_simulParameters gives correct outputs 3 : opt.folder.name", {
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif"
                                                 , opt.folder.name = "")
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
  
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif"
                                                 , opt.folder.name = 1)
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
  
  expect_warning(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                                 , name.MASK = "mask.tif"
                                                 , name.DIST = "dist.tif"
                                                 , name.DROUGHT = "drought.tif"
                                                 , name.FIRE = "fire.tif"
                                                 , opt.folder.name = "CASTOR")
                 , "There is not the same number of files into the DATA/PFGS/HABSUIT/ folder as the number of PFG indicated into the file")
  
  dir.create("FATE_simulation/DATA/PFGS/SUCC/fraise")
  dir.create("FATE_simulation/DATA/PFGS/SUCC/framboise")
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif"
                                               , opt.folder.name = "")
               , "There is not the same number of files (`.txt` file starting with `SUCC`) into the FATE_simulation/DATA/PFGS/SUCC/fraise/ folder as the number of PFG indicated into the file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt"
               , fixed = TRUE)
  
  expect_error(PRE_FATE.params_simulParameters(name.simulation = "FATE_simulation"
                                               , name.MASK = "mask.tif"
                                               , name.DIST = "dist.tif"
                                               , name.DROUGHT = "drought.tif"
                                               , name.FIRE = "fire.tif"
                                               , opt.folder.name = "CASTOR")
               , "There is not the same number of files (`.txt` file starting with `SUCC`) into the FATE_simulation/DATA/PFGS/SUCC/fraise/ folder as the number of PFG indicated into the file FATE_simulation/DATA/GLOBAL_PARAMETERS/Global_parameters_V1.txt"
               , fixed = TRUE)
})

