path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)

test_that("readZarr", {
  path <- file.path(path, "images", "blobs_image")
  zarr <- list.dirs(path, recursive=FALSE)
  md <- Rarr::zarr_overview(zarr, as_data_frame=TRUE)
  ia <- readArray(path)
  expect_s4_class(ia, "ImageArray")
  expect_true(is.list(metadata(ia)))
  expect_equal(dim(ia), md$dim[[1]])
})

test_that("readShapes", {
  df <- readShapes(file.path(path, "shapes", "blobs_shapes"))
  expect_s4_class(df, "DFrame")
  nms <- c("data", "index", "type")
  expect_equal(names(df), nms)
  expect_type(df$data, "list")
  expect_type(df$index, "integer")
  expect_type(df$type, "character")
})

test_that("readTable", {
  sce <- readTable(file.path(path, "table", "table"))
  expect_s4_class(sce, "SingleCellExperiment")
})

test_that("readSpatialData", {
  sd <- readSpatialData(path)
  expect_s4_class(table(sd), "SingleCellExperiment")
  expect_s4_class(image(sd), "ImageArray")
  expect_s4_class(label(sd), "LabelArray")
  expect_s4_class(shape(sd), "DFrame")
  expect_s3_class(point(sd), "R6")
  for (. in images(sd)) expect_s4_class(., "ImageArray")
  for (. in labels(sd)) expect_s4_class(., "LabelArray")
  for (. in shapes(sd)) expect_s4_class(., "DFrame")
  for (. in points(sd)) expect_s3_class(., "R6")
})
