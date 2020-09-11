library(RFate)
context("PRE_FATE.speciesClustering_step1() function")


## INPUT
test_that("PRE_FATE.speciesClustering_step1 gives error with missing data", {
  expect_error(PRE_FATE.speciesClustering_step1()
               , "(missing `mat.species.DIST` information)", fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(NULL)
               , "(missing `mat.species.DIST` information)", fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(NA)
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
})

## INPUT
test_that("PRE_FATE.speciesClustering_step1 gives error with wrong data", {
  
  ## TEST mat.species.DIST : length > 0
  expect_error(PRE_FATE.speciesClustering_step1(list())
               , "`mat.species.DIST` must be of length > 0")
  
  ## TEST mat.species.DIST : dissimilarity object (list)
  expect_error(PRE_FATE.speciesClustering_step1(1)
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1("a")
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(factor("A"))
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`) or a list of dissimilarity objects"
               , fixed = TRUE)
  
  ## TEST mat.species.DIST : dissimilarity object
  expect_error(PRE_FATE.speciesClustering_step1(data.frame(1))
               , "`mat.species.DIST` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(NA))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(1))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list("a"))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(list()))
               , "`mat.species.DIST[[1]]` must be a dissimilarity object (`dist`, `niolap`, `matrix`)"
               , fixed = TRUE)
  
  ## TEST mat.species.DIST : correct number of rows and columns
  expect_error(PRE_FATE.speciesClustering_step1(mat.species.DIST = matrix(1:8, ncol = 4))
               , "`mat.species.DIST` does not have the same number of rows (2) and columns (4)"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(mat.species.DIST = list(matrix(1:9, ncol=3)
                                                                        , matrix(1:8, ncol=4)))
               , "`mat.species.DIST[[2]]` does not have the same number of rows (2) and columns (4)"
               , fixed = TRUE)
  
  ## TEST mat.species.DIST : no NA values
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3))
               , "`mat.species.DIST` contain NA values (1), clustering with `hclust` function might have problems dealing with this data"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,2,3,NA,NA,6,7,8,9), ncol=3))
               , "`mat.species.DIST` contain NA values (2), clustering with `hclust` function might have problems dealing with this data"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3)))
               , "`mat.species.DIST` contain NA values (1), clustering with `hclust` function might have problems dealing with this data"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(1:9, ncol=3)
                                                     , matrix(c(1,2,3,4,NA,6,7,8,9), ncol=3)))
               , "`mat.species.DIST` contain NA values (0, 1), clustering with `hclust` function might have problems dealing with this data"
               , fixed = TRUE)
  
  
  ## TEST enough values to run hclust
  expect_error(PRE_FATE.speciesClustering_step1(matrix(c(1,5,2,2), ncol=2))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(c(1,5,2,2), ncol=2)))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure"
               , fixed = TRUE)
  expect_error(PRE_FATE.speciesClustering_step1(list(matrix(1:9, ncol=3)
                                                     , matrix(c(1,5,2,2), ncol=2)))
               , "All clustering methods (maybe for a specific group) give NA values for Mouchet measure"
               , fixed = TRUE)
})


## OUTPUTS
test_that("PRE_FATE.speciesClustering_step1 gives right output", {
  tmp1 = PRE_FATE.speciesClustering_step1(matrix(seq(9), ncol=3))
  
  expect_output(str(tmp1), "List")
  expect_equal(length(tmp1), 4)
  
  expect_output(str(tmp1$clust.dendrograms), "List")
  expect_equal(length(tmp1$clust.dendrograms), 1)
  
  expect_output(str(PRE_FATE.speciesClustering_step1(as.dist(matrix(seq(9), ncol=3)))$clust.evaluation), "data.frame")
  expect_output(str(tmp1$clust.evaluation), "data.frame")
  expect_output(str(tmp1$clust.evaluation), "4 variables")
  
  tmp2 = PRE_FATE.speciesClustering_step1(list(matrix(seq(9), ncol=3)
                                               , matrix(seq(9), ncol=3)))
  
  expect_output(str(tmp2$clust.dendrograms), "List")
  expect_equal(length(tmp2$clust.dendrograms), 2)
  
  expect_output(str(tmp2$clust.evaluation), "data.frame")
  expect_output(str(tmp2$clust.evaluation), "4 variables")
})
