library(RFate)
context("SAVE_FATE.step1_PFG() function")

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with missing values", {
  expect_error(SAVE_FATE.step1_PFG()
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(NULL)
               , "`name.dataset` must contain a character value of length > 0")
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : name.dataset", {
  expect_error(SAVE_FATE.step1_PFG(name.dataset = NA)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = 1)
               , "`name.dataset` must contain a character value of length > 0")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = factor("a"))
               , "`name.dataset` must contain a character value of length > 0")
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : mat.observations", {
  
  ## TEST mat.observations : data.frame
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = matrix(1))
               , "`mat.observations` must be a data.frame")
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = c(1, 89, 3))
               , "`mat.observations` must be a data.frame")
  
  ## TEST mat.observations : correct number of rows and columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame())
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3,4,5,6,7))
               , "`mat.observations` does not have the appropriate number of rows (>0) or columns (sites, (x), (y), species, abund, (habitat))"
               , fixed = TRUE)
  
  ## TEST mat.observations : correct names of columns
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3))
               , "Column names of `mat.observations` must be `sites`, `(x)`, `(y)`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(1,2,3,4))
               , "Column names of `mat.observations` must be `sites`, `(x)`, `(y)`, `species`, `abund` and `(habitat)`"
               , fixed = TRUE)
  
  ## TEST mat.observations$x : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , x = "a"
                                                                                     , y = 2))
               , "`mat.observations$x` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , x = factor(1)
                                                                                     , y = 2))
               , "`mat.observations$x` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.observations$y : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , y = "a"
                                                                                     , x = 2))
               , "`mat.observations$y` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = 1
                                                                                     , y = factor(1)
                                                                                     , x = 2))
               , "`mat.observations$y` must contain numeric values", fixed = TRUE)
  
  ## TEST mat.observations$abund : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = "a"))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A", mat.observations = data.frame(species = "1"
                                                                                     , sites = "A"
                                                                                     , abund = factor(1)))
               , "`mat.observations$abund` must contain numeric values", fixed = TRUE)
})

## INPUTS
test_that("SAVE_FATE.step1_PFG gives error with wrong type of data : doRuleA / doRuleB / doRuleC", {
  
  ## TEST doRuleA : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = TRUE
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleA']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST doRuleB : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleB']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST doRuleC : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['doRuleC']` must be either `0` or `1`"
               , fixed = TRUE)
  
  ## TEST rule.A1 : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = "a"
                                                              , "rule.A2_quantile" = 0.9))
               , "`rules.selectDominant['rule.A1']` must contain numeric values"
               , fixed = TRUE)
  

  ## TEST rule.A2_quantile : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = 10
                                                              , "rule.A2_quantile" = "a"))
               , "`rules.selectDominant['rule.A2_quantile']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 1
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 1
                                                              , "rule.A1" = 10
                                                              , "rule.A2_quantile" = 1.5))
               , "`rules.selectDominant['rule.A2_quantile']` must contain values between `0` and `1`"
               , fixed = TRUE)

  ## TEST rule.B1_number : numeric values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = "a"))
               , "`rules.selectDominant['rule.B1_number']` must contain numeric values"
               , fixed = TRUE)
  
  ## TEST rule.B1_percentage : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = "a"))
               , "`rules.selectDominant['rule.B1_percentage']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = 1.5))
               , "`rules.selectDominant['rule.B1_percentage']` must contain values between `0` and `1`"
               , fixed = TRUE)
  
  
  ## TEST rule.B2 : correct values
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" = 0.5
                                                              , "rule.B2" = "a"))
               , "`rules.selectDominant['rule.B2']` must contain values between `0` and `1`"
               , fixed = TRUE)
  expect_error(SAVE_FATE.step1_PFG(name.dataset = "A"
                                   , mat.observations = data.frame(species = "1"
                                                                   ,sites = "A"
                                                                   , abund = 5)
                                   , rules.selectDominant = c("doRuleA" = 0
                                                              , "doRuleB" = 1
                                                              , "doRuleC" = 0
                                                              , "rule.B1_number" = 10
                                                              , "rule.B1_percentage" =  0.5
                                                              , "rule.B2" = 1.5))
               , "`rules.selectDominant['rule.B2']` must contain values between `0` and `1`"
               , fixed = TRUE)
})


