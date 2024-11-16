library(SpatialData)

test_that("create ImageArray", {
  
  # single scale image
  mat <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  img <- .ImageArray(data = list(mat))
  img_object <- ImageArray(data = list(mat))
  img_object <- ImageArray(data = list())
  img_object <- ImageArray(data = list(), meta = Zattrs())
  
  # single scale image error
  expect_error(ImageArray(data = mat))
  expect_error(ImageArray(data = mat, meta = list()))
  expect_error(ImageArray(data = mat, meta = 1))
  
  # multi scale image
  mat1 <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  mat2 <- array(sample(1:255, size = 3*10*10, replace = TRUE), dim = c(3, 10, 10))
  mat3 <- array(sample(1:255, size = 3*5*5, replace = TRUE), dim = c(3, 5, 5))
  mat <- list(mat1, mat2, mat3)
  img <- .ImageArray(data = mat)
  img <- ImageArray(data = mat)
  
  expect_equal(1L,1L)
})

test_that("ImageArray data method", {
  
  # single scale image
  mat <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  img <- .ImageArray(data = list(mat))
  expect_equal(mat, data(img))
  expect_equal(mat, data(img, 1))
  
  # error
  expect_error(data(img, 2))

  expect_equal(1L,1L)
})

test_that("ImageArray ploting", {
  
  # single scale image
  mat1 <- array(sample(1:255, size = 3*20*20, replace = TRUE), dim = c(3, 20, 20))
  mat2 <- array(sample(1:255, size = 3*10*10, replace = TRUE), dim = c(3, 10, 10))
  mat <- list(mat1, mat2)
  img <- ImageArray(data = mat)
  
  # checks
  expect_equal(.get_plot_data(img), mat1)
  expect_equal(.get_plot_data(img,1), mat1)
  expect_equal(.get_plot_data(img,2), mat2)
  expect_true(all(dim(.get_plot_data(img,1)) == c(3,20,20)))
  expect_true(all(dim(.get_plot_data(img,2)) == c(3,10,10)))
  
  
})