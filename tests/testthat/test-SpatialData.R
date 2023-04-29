test_that("SpatialData,empty", {
  x <- SpatialData()
  expect_s4_class(x, "SpatialData")
  expect_length(elementNames(x), 0)
})

test_that("SpatialData,images", {
  one <- SpatialData(images=ImageArray())
  two <- SpatialData(images=replicate(2, ImageArray()))
  for (x in list(one, two)) {
    expect_s4_class(x, "SpatialData")
    y <- elementNames(x)
    expect_length(y, 1)
    expect_type(y, "character")
    expect_identical(y, "images")
  }
})

test_that("SpatialData,labels", {
  one <- SpatialData(labels=ImageArray())
  two <- SpatialData(labels=replicate(2, ImageArray()))
  for (x in list(one, two)) {
    expect_s4_class(x, "SpatialData")
    y <- elementNames(x)
    expect_length(y, 1)
    expect_type(y, "character")
    expect_identical(y, "labels")
  }
})

# ------------------------------------------------------------------------------

path <- file.path("extdata", "blobs")
path <- system.file(path, package="SpatialData")
spd <- readSpatialData(path)

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
is_r6 <- \(.) is(., "R6")

test_that("image", {
  expect_error(image(spd, 00))
  expect_error(image(spd, -1))
  expect_error(image(spd, 99))
  expect_error(image(spd, ""))
  i <- imageNames(spd)[1]
  expect_s4_class(image(spd, 1), "ImageArray")
  expect_s4_class(image(spd, i), "ImageArray")
})

test_that("label", {
  expect_error(label(spd, 00))
  expect_error(label(spd, -1))
  expect_error(label(spd, 99))
  expect_error(label(spd, ""))
  i <- labelNames(spd)[1]
  expect_s4_class(label(spd, 1), "ImageArray")
  expect_s4_class(label(spd, i), "ImageArray")
})

test_that("shape", {
  expect_error(shape(spd, 00))
  expect_error(shape(spd, -1))
  expect_error(shape(spd, 99))
  expect_error(shape(spd, ""))
  i <- shapeNames(spd)[1]
  expect_s4_class(shape(spd, 1), "DFrame")
  expect_s4_class(shape(spd, i), "DFrame")
})

test_that("point", {
  expect_error(point(spd, 00))
  expect_error(point(spd, -1))
  expect_error(point(spd, 99))
  expect_error(point(spd, ""))
  i <- pointNames(spd)[1]
  expect_s3_class(point(spd, 1), "R6")
  expect_s3_class(point(spd, i), "R6")
})

test_that("table", {
  expect_s4_class(table(spd), "SingleCellExperiment")
  expect_error(table(spd) <- "")
  expect_error(table(spd) <- NA)
  expect_silent(table(spd) <- NULL)
})

test_that("images", {
  x <- images(spd)
  n <- length(attr(spd, "images"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_ia, logical(1))))
})

test_that("labels", {
  x <- labels(spd)
  n <- length(attr(spd, "labels"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_ia, logical(1))))
})

test_that("shapes", {
  x <- shapes(spd)
  n <- length(attr(spd, "shapes"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_df, logical(1))))
})

test_that("points", {
  x <- points(spd)
  n <- length(attr(spd, "points"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_r6, logical(1))))
})

test_that("elementNames", {
  x <- elementNames(spd)
  expect_type(x, "character")
  layers <- attributes(spd)
  layers <- layers[setdiff(names(layers), c("metadata", "class"))]
  .na <- \(.) length(.) == 0 || is(., "name")
  expect_length(x, sum(!vapply(layers, .na, logical(1))))
})

test_that("element", {
  expect_error(element(spd, elementName="foo"))
  expect_error(element(spd, which="foo"))
  expect_error(element(spd, which=12345))

  expect_true(is_ia(element(spd, elementName="images", which=1)))
  expect_true(is_ia(element(spd, elementName="labels", which=1)))
  expect_true(is_df(element(spd, elementName="shapes", which=1)))
  expect_true(is_r6(element(spd, elementName="points", which=1)))
})
