library(RFate)
context("PRE_FATE.speciesClustering_step2() function")


## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step2()
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2(NA)
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2(NULL)
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : clust.dendrograms", {
  
  ## TEST clust.dendrograms : hclust or list of hclust
  expect_error(PRE_FATE.speciesClustering_step2(1)
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2("a")
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2(factor("A"))
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2(data.frame(1))
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = list())
               , "(missing `clust.dendrograms` information which must be of class `hclust` or a list `hclust` objects)")
  
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = list(1))
               , "each element of `clust.dendrograms` must be of class `hclust`")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : no.clusters", {
  
  ## TEST no.clusters : missing / numeric values
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2))))
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = NA)
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = NULL)
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = list())
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = data.frame(1))
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = factor(1))
               , "(missing `no.clusters` information)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = "a")
               , "(missing `no.clusters` information)")
  
  ## TEST no.clusters : correct length
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = c(1,2))
               , "`no.clusters` must have the same length than `clust.dendrograms`")
})

## INPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error with wrong data : mat.species.DIST", {
  
  ## TEST mat.species.DIST : dist or list of dist
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1)
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = NA)
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = NULL)
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = 1)
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = "a")
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = factor(1))
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = matrix(seq(4), ncol = 2))
               , "(missing `mat.species.DIST` information which must be a dist object, or a list of dist objects)")
  
  ## TEST mat.species.DIST : correct length
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = list(matrix(seq(4), ncol = 2), 1))
               , "`mat.species.DIST` must have the same length than `clust.dendrograms`")
})


## OUTPUTS
test_that("PRE_FATE.speciesClustering_step2 gives error : no determinant species", {
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = hclust(as.dist(matrix(seq(4), ncol = 2)))
                                                , no.clusters = 1
                                                , mat.species.DIST = as.dist(matrix(seq(4), ncol = 2)))
               , "No determinant species have been selected. Please check your data")
  expect_error(PRE_FATE.speciesClustering_step2(clust.dendrograms = list(hclust(as.dist(matrix(seq(4), ncol = 2))))
                                                , no.clusters = 1
                                                , mat.species.DIST = list(as.dist(matrix(seq(4), ncol = 2))))
               , "No determinant species have been selected. Please check your data")
})


## OUTPUTS
test_that("PRE_FATE.speciesClustering_step2 gives right output", {
  mat.traits = data.frame(species = paste0("SP", 1:100)
                          , TRAIT_1 = sample(1:5, 100, replace = TRUE)
                          , TRAIT_2 = rnorm(100)
                          , TRAIT_3 = runif(100)
                          , GROUP = sample(c("A", "B", "C"), 100, replace = TRUE))
  mat.overlap = matrix(runif(100*100)
                       , ncol = 100
                       , dimnames = list(paste0("SP", 1:100), paste0("SP", 1:100)))
  
  sp.DIST = PRE_FATE.speciesDistance(mat.traits = mat.traits
                                     , mat.overlap = mat.overlap)
  sp.CLUST1 = PRE_FATE.speciesClustering_step1(mat.species.DIST = sp.DIST)
  
  
  sp.CLUST2 = PRE_FATE.speciesClustering_step2(clust.dendrograms = sp.CLUST1$clust.dendrograms[[1]]
                                               , no.clusters = 3
                                               , mat.species.DIST = sp.DIST[[1]])
  
  expect_output(str(sp.CLUST2), "List")
  expect_equal(length(sp.CLUST2), 4)
  expect_output(str(sp.CLUST2$determ.all), "10 variables")
  
  
  sp.CLUST2 = PRE_FATE.speciesClustering_step2(clust.dendrograms = sp.CLUST1$clust.dendrograms
                                               , no.clusters = c(3,3,3)
                                               , mat.species.DIST = sp.DIST)
  
  expect_output(str(sp.CLUST2), "List")
  expect_equal(length(sp.CLUST2), 4)
  expect_output(str(sp.CLUST2$determ.all), "10 variables")
})
