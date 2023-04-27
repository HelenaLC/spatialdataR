test_that("check that reading a zarr image file works", {
  path <- system.file("extdata/mibitof", package = "SpatialData", mustWork = TRUE)
  im <- readImageArray(file.path(path, "images/point8_image"))
  expect_equal(dim(data(im)), c(3, 1024, 1024))
  expect_true(is.list(metadata(im)))
})
