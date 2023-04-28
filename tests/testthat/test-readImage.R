# test_that("check that reading a zarr image file works", {
#   path <- system.file("extdata/mibitof", package="SpatialData", mustWork=TRUE)
#   im <- readImageArray(file.path(path, "images/point8_image"))
#   expect_equal(dim(im), c(3, 1024, 1024))
#   expect_true(is.list(metadata(im)))
# })

test_that("check that reading a full spatial dataset works", {

  path <- system.file("extdata/raccoon", package="SpatialData", mustWork=TRUE)
  sd <- readSpatialData(path)
  for(im in images(sd)){
    expect_s4_class(im, "ImageArray")
  }
  for(lab in labels(sd)){
    expect_s4_class(im, "ImageArray")
  }
  expect_s4_class(label(sd, 1), "ImageArray")
  expect_s4_class(image(sd, 1), "ImageArray")
})

test_that("readShapes works", {

  path <- system.file("extdata/raccoon", package="SpatialData", mustWork=TRUE)
  shape_data <- readShapes(file.path(path, "shapes/circles"))
  df <- shapes(readSpatialData(path))
  expect_s4_class(df$circles, "DFrame")
})

# test_that("readTable works", {
#   path <- system.file("extdata/raccoon", package="SpatialData", mustWork=TRUE)
#   sce <- SpatialData:::readTable(file.path(path, "table/table"))
#   expect_s4_class(sce, "SingleCellExperiment")
# })

