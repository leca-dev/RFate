
## .getELLIPSE
utils::globalVariables(names = c("fac.i"))

## .getGraphics_results
utils::globalVariables(names = c("dir.save", "dir.output.perPFG.allStrata"
                                 , "dir.output.perPFG.perStrata", "dir.output.light"
                                 , "dir.output.soil", "dir.output.perPFG.allStrata.REL"
                                 , "dir.output.perPFG.allStrata.BIN"
                                 , "dir.output.perPFG.perStrata.BIN"))

## .getGraphics_PFG
utils::globalVariables(names = c("no_PFG", "PFG", "no_STRATA"
                                 , "doLight", "doSoil", "doHabsuit"))

## .getGraphics_mask
utils::globalVariables(names = c("file.mask", "ras.mask", "ind_1_mask"
                                 , "no_1_mask", "xy.1"))

## PRE_FATE.selectDominant
utils::globalVariables(names = c("i.type", "i.percent", "i.dat", "i.hab", "i.rule"
                                 , "i.rep", "i.iter", "i.axis", "i.subset"))

## PRE_FATE.speciesclustering...
utils::globalVariables(names = c("group", "no.clusters", "variable", "tab", "tr"
                                 , "isThere.longevity", "isThere.maturity"
                                 , "isThere.soil_contrib", "isThere.soil_tolerance"
                                 , "isThere.soil_tol_min", "isThere.soil_tol_max"
                                 , "isThere.height", "isThere.light"))

## PRE_FATE.params_simulParameters
utils::globalVariables(names = c("dirs.SCENARIO.MASK", "dirs.SCENARIO.HABSUIT"
                                 , "dirs.SCENARIO.DIST", "dirs.SCENARIO.DROUGHT"
                                 , "dirs.SCENARIO.ALIENS", "dirs.SCENARIO.FIRE"
                                 , "dirs.HABSUIT", "dirs.ALIENS", "di.mod"
                                 , "SCENARIO.MASK", "SCENARIO.HABSUIT", "SCENARIO.DIST"
                                 , "SCENARIO.DROUGHT", "SCENARIO.ALIENS", "SCENARIO.FIRE"
                                 , "files.PFG.SUCC", "files.PFG.LIGHT", "files.PFG.SOIL"
                                 , "files.PFG.DISP", "files.PFG.HABSUIT", "files.PFG.DIST"
                                 , "files.PFG.DROUGHT", "files.PFG.ALIENS", "files.PFG.FIRE"
                                 , "no.DIST", "no.FIRE"))

## POST_FATE...
utils::globalVariables(names = c("abs.simulParam", "i", "fi", "mw", "metric"
                                 , "fg", "qq", "cutoff", "vari", "pfg", "stra"))


