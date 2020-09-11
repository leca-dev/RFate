# library(RFate)
# context("POST_FATE.graphic_evolutionCoverage() function")
# 
# 
# ## INPUTS
# test_that("POST_FATE.graphic_evolutionCoverage gives error with missing data", {
#   expect_error(POST_FATE.graphic_evolutionCoverage()
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   expect_error(POST_FATE.graphic_evolutionCoverage(NA)
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   expect_error(POST_FATE.graphic_evolutionCoverage(NULL)
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
# })
# 
# ## INPUTS
# test_that("POST_FATE.graphic_evolutionCoverage gives error with wrong data : name.simulation", {
#   expect_error(POST_FATE.graphic_evolutionCoverage(1)
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   expect_error(POST_FATE.graphic_evolutionCoverage("a")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   expect_error(POST_FATE.graphic_evolutionCoverage(factor("A"))
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   expect_error(POST_FATE.graphic_evolutionCoverage(data.frame(1))
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   
#   if (dir.exists("FATE_simulation")) unlink("FATE_simulation", recursive = TRUE)
#   dir.create("FATE_simulation/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a PARAM_SIMUL/ folder")
#   dir.create("FATE_simulation/PARAM_SIMUL/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/ folder")
#   dir.create("FATE_simulation/RESULTS/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a DATA/ folder")
#   dir.create("FATE_simulation/DATA/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation")
#                , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
# })
# 
# ## INPUTS
# test_that("POST_FATE.graphic_evolutionCoverage gives error with wrong data : file.simulParam", {
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = NULL)
#                , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = NA)
#                , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
#   
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "")
#                , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "")
#                , "Missing data!\n The folder FATE_simulation/PARAM_SIMUL/ does not contain adequate files")
#   
#   
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "toto")
#                , "Wrong name file given!\n `FATE_simulation/PARAM_SIMUL/toto` does not exist")
#   file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
# })
# 
# ## INPUTS
# test_that("POST_FATE.graphic_evolutionCoverage gives error with wrong data : files", {
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (--END_OF_FILE--) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   cat("--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)  
#   cat("--T--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (SAVE_DIR) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   cat("SAVE_DIR\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag.split` (^--.*--$) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   cat("--SAVE_DIR--\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (SAVE_DIR) does not contain any value"
#                , fixed = TRUE)
#   cat("--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ folder"
#                , fixed = TRUE)
#   dir.create("FATE_simulation/RESULTS/Hello/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_allStrata/ folder"
#                , fixed = TRUE)
#   dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/ABUND_perPFG_perStrata/ folder"
#                , fixed = TRUE)
#   dir.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_perStrata/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/LIGHT/ folder"
#                , fixed = TRUE)
#   dir.create("FATE_simulation/RESULTS/Hello/LIGHT/")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name folder given!\n `name.simulation` does not exist or does not contain a RESULTS/Hello/SOIL/ folder"
#                , fixed = TRUE)
#   dir.create("FATE_simulation/RESULTS/Hello/SOIL/")
#   
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   
#   
#   if (file.exists("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")) file.remove("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   file.create("FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   cat("GLOBAL_PARAMS\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n", file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   
#   cat("--GLOBAL_PARAMS--\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
#       , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (GLOBAL_PARAMS) does not contain any value"
#                , fixed = TRUE)
#   cat("--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
#       , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name file given!\n `FATE_simulation/GlobalParam.txt` does not exist"
#                , fixed = TRUE)
#   
#   file.create("FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag.split` ( ) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
#                , fixed = TRUE)
#   cat("HOP \n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (NB_FG) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
#                , fixed = TRUE)
#   cat("NB_FG \n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
#                , fixed = TRUE)
#   cat("NB_FG a\n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt does not contain any value"
#                , fixed = TRUE)
#   cat("NB_FG 3\n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   cat("--PFG_LIFE_HISTORY_PARAMS--\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
#       , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (PFG_LIFE_HISTORY_PARAMS) does not contain any value"
#                , fixed = TRUE)
#   cat("--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
#       , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Missing data!\n The number of PFG (NB_FG) within FATE_simulation/GlobalParam.txt is different from the number of PFG files contained in FATE_simulation/DATA/PFGS/SUCC/"
#                , fixed = TRUE)
#   cat("NB_FG 1\n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (NB_STRATUM) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
#                , fixed = TRUE)
#   cat("NB_FG 1\nNB_STRATUM 3\n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (DO_LIGHT_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
#                , fixed = TRUE)
#   cat("NB_FG 1\nNB_STRATUM 3\nDO_LIGHT_COMPETITION 1\n", file = "FATE_simulation/GlobalParam.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (DO_SOIL_COMPETITION) is not found within `params.lines` (FATE_simulation/GlobalParam.txt)"
#                , fixed = TRUE)
#   cat("NB_FG 1\nNB_STRATUM 3\nDO_LIGHT_COMPETITION 1\nDO_SOIL_COMPETITION 1\n", file = "FATE_simulation/GlobalParam.txt")
#   
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong type of data!\n `flag` (MASK) is not found within `params.lines` (FATE_simulation/PARAM_SIMUL/ParamSimul.txt)"
#                , fixed = TRUE)
#   
#   cat("--MASK--\nFATE_simulation/Mask.tif\n--PFG_LIFE_HISTORY_PARAMS--\nHop\n--GLOBAL_PARAMS--\nFATE_simulation/GlobalParam.txt\n--SAVE_DIR--\nHello\n--END_OF_FILE--\n"
#       , file = "FATE_simulation/PARAM_SIMUL/ParamSimul.txt")
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name file given!\n `FATE_simulation/Mask.tif` does not exist"
#                , fixed = TRUE)
#   
#   PNE_PARAM = .loadData("PNE_PARAM")
#   writeRaster(PNE_PARAM$masks$maskEcrins, filename = "FATE_simulation/Mask.tif", overwrite = TRUE)
#   
#   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
#                                                    , file.simulParam = "ParamSimul.txt")
#                , "Wrong name file given!\n `FATE_simulation/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_Hello.csv` does not exist")
#   file.create("FATE_simulation/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_Hello.csv")
#   cat("PFG,ID,HAB,10\nA,1,all,5\n"
#       , file = "FATE_simulation/RESULTS/POST_FATE_TABLE_PIXEL_evolution_abundance_Hello.csv")
#   
# })
# 
# 
# ## INPUTS
# # test_that("POST_FATE.graphic_evolutionCoverage gives error with wrong data : rasters", {
# #   file.create("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif")
# #   
# #   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                    , file.simulParam = "ParamSimul.txt")
# #                , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
# #                , fixed = TRUE)
# #   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation")
# #                , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
# #                , fixed = TRUE)
# #   
# #   file.remove("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_PFG1_STRATA_all.tif")
# #   
# #   PNE_RESULTS = .loadData("PNE_RESULTS")
# #   PFG.names = names(PNE_RESULTS$abund_str.equilibrium)
# #   PFG.names = sub("PNE_year_800_", "", PFG.names)
# #   PFG.names = sapply(PFG.names, function(x) strsplit(x, "_")[[1]][1])
# #   for (pfg in PFG.names[1])
# #   {
# #     ind = grep(pfg, names(PNE_RESULTS$abund_str.equilibrium))
# #     stk = PNE_RESULTS$abund_str.equilibrium[[ind]]
# #     ras = sum(stk)
# #     writeRaster(ras
# #                 , filename = paste0("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_800_", pfg, "_STRATA_all.tif")
# #                 , overwrite = TRUE)
# #   }
# #   
# #   expect_error(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                    , file.simulParam = "ParamSimul.txt")
# #                , "Missing data!\n The names of PFG extracted from files within FATE_simulation/DATA/PFGS/SUCC/"
# #                , fixed = TRUE)
# #   
# #   file.rename(from = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_800_C1_STRATA_all.tif"
# #               , to = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif")
# #   
# #   expect_message(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                      , file.simulParam = "ParamSimul.txt")
# #                  , "have been successfully created !"
# #                  , fixed = TRUE)
# #   
# #   expect_output(str(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                         , file.simulParam = "ParamSimul.txt")), "list")
# #   
# #   system("gunzip FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif.gz")
# #   file.copy(from = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif"
# #             , to = "FATE_simulation/Hab.tif")
# #   ras = raster("FATE_simulation/Hab.tif")
# #   ras = raster::cut(ras, breaks = seq(100, 4000, length.out = 10))
# #   writeRaster(ras, filename = "FATE_simulation/Hab.tif", overwrite = TRUE)
# #   
# #   expect_output(str(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                         , file.simulParam = "ParamSimul.txt"
# #                                                         , opt.ras_habitat = "FATE_simulation/Hab.tif")), "list")
# #   
# #   
# #   system("gunzip FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif.gz")
# #   ras = raster("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif") 
# #   writeRaster(ras, filename = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.img")
# #   file.remove("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.tif")
# #   expect_message(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                      , file.simulParam = "ParamSimul.txt")
# #                  , "have been successfully created !"
# #                  , fixed = TRUE)
# #   
# #   system("gunzip FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.img.gz")
# #   ras = raster("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.img") 
# #   writeRaster(ras, filename = "FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.asc")
# #   file.remove("FATE_simulation/RESULTS/Hello/ABUND_perPFG_allStrata/Abund_YEAR_1_Hop_STRATA_all.img")
# #   expect_message(POST_FATE.graphic_evolutionCoverage(name.simulation = "FATE_simulation"
# #                                                      , file.simulParam = "ParamSimul.txt")
# #                  , "have been successfully created !"
# #                  , fixed = TRUE)
# #   
# # })
# 
