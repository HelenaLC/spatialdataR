path <- file.path("extdata", "blobs.zarr")
path <- system.file(path, package="SpatialData")
spd <- readSpatialData(path)

test_that("elementNames", {
  x <- elementNames(spd)
  expect_type(x, "character")
  layers <- attributes(spd)
  layers <- layers[setdiff(names(layers), c("metadata", "class"))]
  expect_length(x, sum(vapply(layers, length, integer(1)) != 0))
})

test_that("imageNames", {
  x <- imageNames(spd)
  n <- length(attr(spd, "images"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("labelNames", {
  x <- labelNames(spd)
  n <- length(attr(spd, "labels"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("shapeNames", {
  x <- shapeNames(spd)
  n <- length(attr(spd, "shapes"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("pointNames", {
  x <- pointNames(spd)
  n <- length(attr(spd, "points"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

is_ia <- \(.) is(., "ImageArray")
is_df <- \(.) is(., "DFrame")

test_that("images", {
  x <- images(spd)
  n <- length(attr(spd, "images"))
  expect_type(x, "list")
  if (n > 0) expect_true(vapply(x, is_ia, logical(1)))
})

test_that("labels", {
  x <- labels(spd)
  n <- length(attr(spd, "labels"))
  expect_type(x, "list")
  if (n > 0) expect_true(vapply(x, is_ia, logical(1)))
})

test_that("shapes", {
  x <- shapes(spd)
  n <- length(attr(spd, "shapes"))
  expect_type(x, "list")
  if (n > 0) expect_true(vapply(x, is_df, logical(1)))
})

test_that("points", {
  x <- points(spd)
  n <- length(attr(spd, "points"))
  expect_type(x, "list")
  if (n > 0) expect_true(vapply(x, is_df, logical(1)))
})