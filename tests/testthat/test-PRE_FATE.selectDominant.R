library(RFate)
context("PRE_FATE.selectDominant() function")

## INPUTS
test_that("PRE_FATE.selectDominant gives error with missing values", {
  expect_error(PRE_FATE.selectDominant()
               , "`mat.observations` must be a data.frame")
  expect_error(PRE_FATE.selectDominant(NA)
               , "`mat.observations` must be a data.frame")
  expect_error(PRE_FATE.selectDominant(NULL)
               , "`mat.observations` must be a data.frame")
  # expect_warning(PRE_FATE.selectDominant(mat.observations = data.frame(sites = "A", species = "1", abund = NA))
  #                , "Species with NO abundance information can only be selected with the criteria based on number of presences...")
  # expect_warning(PRE_FATE.selectDominant(mat.observations = data.frame(sites = "A", species = "1", abund = NA))
  #                , "NO abundance information in your data. Dominant species selection will only be done with the criteria based on number of presences...")
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : mat.observations", {
  
  ## TEST mat.observations : data.frame
  expect_error(PRE_FATE.selectDominant(mat.observations = matrix(1))
               , "`mat.observations` must be a data.frame")
  expect_error(PRE_FATE.selectDominant(mat.observations = c(1, 89, 3))
               , "`mat.observations` must be a data.frame")
  
  ## TEST mat.observations : correct number of rows and columns
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame())
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(1))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(1,2,3,4,5))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, species, abund, (habitat))"
               , fixed = TRUE)
  
  ## TEST mat.observations : correct names of columns
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(1,2,3))
               , "Column names of `mat.observations` must be `sites`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(1,2,3,4))
               , "Column names of `mat.observations` must be `sites`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  
  ## TEST mat.observations$abund : numeric values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = "a"))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = factor(1)))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : doRuleA / doRuleC", {
  
  ## TEST rule.A1 : numeric values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = "a"
                                       , rule.A2_quantile = 0.9)
               , "`rule.A1` must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = factor(10)
                                       , rule.A2_quantile = 0.9)
               , "`rule.A1` must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = FALSE
                                       , rule.A1 = factor(10)
                                       , rule.A2_quantile = 0.9
                                       , doRuleC = TRUE)
               , "`rule.A1` must contain numeric values")
  

  ## TEST rule.A2_quantile : correct values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = 10
                                       , rule.A2_quantile = "a")
               , "`rule.A2_quantile` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = 10
                                       , rule.A2_quantile = factor(0.9))
               , "`rule.A2_quantile` must contain values between `0` and `1`")
  
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = 10
                                       , rule.A2_quantile = -1)
               , "`rule.A2_quantile` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleA = TRUE
                                       , rule.A1 = 10
                                       , rule.A2_quantile = 1.5)
               , "`rule.A2_quantile` must contain values between `0` and `1`")
  
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : doRuleB", {
  
  ## TEST rule.B1_percentage : correct values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = "a"
                                       , rule.B1_number = 5
                                       , rule.B2 = 0.5)
               , "`rule.B1_percentage` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = factor(0.25)
                                       , rule.B1_number = 5
                                       , rule.B2 = 0.5)
               , "`rule.B1_percentage` must contain values between `0` and `1`")
  
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = -1
                                       , rule.B1_number = 5
                                       , rule.B2 = 0.5)
               , "`rule.B1_percentage` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 1.5
                                       , rule.B1_number = 5
                                       , rule.B2 = 0.5)
               , "`rule.B1_percentage` must contain values between `0` and `1`")
  
  
  ## TEST rule.B1_number : numeric values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = "a"
                                       , rule.B2 = 0.5)
               , "`rule.B1_number` must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = factor(5)
                                       , rule.B2 = 0.5)
               , "`rule.B1_number` must contain numeric values")
  
  
  ## TEST rule.B2 : correct values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = 5
                                       , rule.B2 = "a")
               , "`rule.B2` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = 5
                                       , rule.B2 = factor(0.5))
               , "`rule.B2` must contain values between `0` and `1`")
  
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = 5
                                       , rule.B2 = -1)
               , "`rule.B2` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , doRuleB = TRUE
                                       , rule.B1_percentage = 0.25
                                       , rule.B1_number = 5
                                       , rule.B2 = 1.5)
               , "`rule.B2` must contain values between `0` and `1`")
  
})

## INPUTS
test_that("PRE_FATE.selectDominant gives error with wrong type of data : opt.doRobustness", {
  
  ## TEST opt.robustness_percent : correct values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = "a"
                                       , opt.robustness_rep = 10)
               , "`opt.robustness_percent` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = factor(0.5)
                                       , opt.robustness_rep = 10)
               , "`opt.robustness_percent` must contain values between `0` and `1`")
  
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = -1
                                       , opt.robustness_rep = 10)
               , "`opt.robustness_percent` must contain values between `0` and `1`")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = 1.5
                                       , opt.robustness_rep = 10)
               , "`opt.robustness_percent` must contain values between `0` and `1`")
  
  
  ## TEST opt.robustness_rep : numeric values
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = 0.5
                                       , opt.robustness_rep = "a")
               , "`opt.robustness_rep` must contain numeric values")
  expect_error(PRE_FATE.selectDominant(mat.observations = data.frame(species = "1"
                                                                     ,sites = "A"
                                                                     , abund = 5)
                                       , opt.doRobustness = TRUE
                                       , opt.robustness_percent = 0.5
                                       , opt.robustness_rep = factor(10))
               , "`opt.robustness_rep` must contain numeric values")
  
})



## OUTPUTS
test_that("PRE_FATE.selectDominant right results", {
  mat.obs = data.frame(sites = "A", species = "1", abund = 3)
  tmp1 = PRE_FATE.selectDominant(mat.observations = mat.obs
                                 , opt.doPlot = FALSE)
  
  expect_output(str(tmp1), "List")
  expect_equal(length(tmp1), 2)
  expect_output(str(tmp1$tab.rules), "data.frame")
  expect_output(str(tmp1$tab.rules), "7 variables")
  
  mat.obs = data.frame(sites = "A", species = "1", abund = 3, habitat = "Landes")
  tmp2 = PRE_FATE.selectDominant(mat.observations = mat.obs
                                 , doRuleC = TRUE
                                 , opt.doPlot = FALSE)
  
  expect_output(str(tmp2), "List")
  expect_equal(length(tmp2), 2)
  expect_output(str(tmp2$tab.rules), "data.frame")
  expect_output(str(tmp2$tab.rules), "7 variables")
  
  mat.obs = data.frame(sites = "A", species = "1", abund = 3, habitat = c("Landes", "Grassland"))
  tmp3 = PRE_FATE.selectDominant(mat.observations = mat.obs
                                 , doRuleC = TRUE
                                 , opt.doPlot = FALSE)
  
  expect_output(str(tmp3), "List")
  expect_equal(length(tmp3), 2)
  expect_output(str(tmp3$tab.rules), "data.frame")
  expect_output(str(tmp3$tab.rules), "9 variables")
  

  
  mat.obs = data.frame(sites = c(rep("A", 50), rep("B", 50))
                       , species = as.character(sample(1:5, 100, replace = T))
                       , abund = sample(c(rep(1, 20), seq(30, 50)), 100, replace = T)
                       , habitat = c("Landes", "Grassland")
                       , stringsAsFactors = FALSE)
  tmp4 = PRE_FATE.selectDominant(mat.observations = mat.obs
                                 , doRuleC = TRUE
                                 , opt.doPlot = TRUE)
  
  expect_output(str(tmp4), "List")
  # expect_equal(length(tmp4), 5)
  expect_output(str(tmp4$tab.rules), "data.frame")
  expect_output(str(tmp4$tab.rules), "9 variables")
  
  tmp5 = PRE_FATE.selectDominant(mat.observations = mat.obs
                                 , doRuleC = TRUE
                                 , opt.doPlot = TRUE
                                 , opt.doRobustness = TRUE)
  
  expect_output(str(tmp5), "List")
  # expect_equal(length(tmp5), 7)
  expect_output(str(tmp5$tab.rules), "data.frame")
  expect_output(str(tmp5$tab.rules), "9 variables")
  expect_output(str(tmp5$tab.robustness), "data.frame")
  expect_output(str(tmp5$tab.robustness), "12 variables")
  
})
