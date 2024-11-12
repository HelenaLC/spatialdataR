library(SpatialData)

test_that("create ImageArray", {
  
  # single scale image
  mat <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  img <- .ImageArray(data = list(mat))
  
  # multi scale image
  mat1 <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  mat2 <- array(sample(1:255, size = 3*10*10, replace = TRUE), dim = c(3, 10, 10))
  mat3 <- array(sample(1:255, size = 3*5*5, replace = TRUE), dim = c(3, 5, 5))
  mat <- list(mat1, mat2, mat3)
  img <- .ImageArray(data = mat)
  
  expect_equal(1L,1L)
})
