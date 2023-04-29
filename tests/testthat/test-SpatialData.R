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
  one <- SpatialData(labels=LabelArray())
  two <- SpatialData(labels=replicate(2, LabelArray()))
  for (x in list(one, two)) {
    expect_s4_class(x, "SpatialData")
    y <- elementNames(x)
    expect_length(y, 1)
    expect_type(y, "character")
    expect_identical(y, "labels")
  }
})

# ------------------------------------------------------------------------------

path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
sd <- readSpatialData(path)

test_that("imageNames", {
  x <- imageNames(sd)
  n <- length(attr(sd, "images"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("labelNames", {
  x <- labelNames(sd)
  n <- length(attr(sd, "labels"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("shapeNames", {
  x <- shapeNames(sd)
  n <- length(attr(sd, "shapes"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

test_that("pointNames", {
  x <- pointNames(sd)
  n <- length(attr(sd, "points"))
  expect_length(x, n)
  if (n > 0) expect_type(x, "character")
})

is_ia <- \(.) is(., "ImageArray")
is_la <- \(.) is(., "LabelArray")
is_df <- \(.) is(., "DFrame")
is_r6 <- \(.) is(., "R6")

test_that("image", {
  expect_error(image(sd, 00))
  expect_error(image(sd, -1))
  expect_error(image(sd, 99))
  expect_error(image(sd, ""))
  i <- imageNames(sd)[1]
  expect_s4_class(image(sd, 1), "ImageArray")
  expect_s4_class(image(sd, i), "ImageArray")
})

test_that("label", {
  expect_error(label(sd, 00))
  expect_error(label(sd, -1))
  expect_error(label(sd, 99))
  expect_error(label(sd, ""))
  i <- labelNames(sd)[1]
  expect_s4_class(label(sd, 1), "LabelArray")
  expect_s4_class(label(sd, i), "LabelArray")
})

test_that("shape", {
  expect_error(shape(sd, 00))
  expect_error(shape(sd, -1))
  expect_error(shape(sd, 99))
  expect_error(shape(sd, ""))
  i <- shapeNames(sd)[1]
  expect_s4_class(shape(sd, 1), "DFrame")
  expect_s4_class(shape(sd, i), "DFrame")
})

test_that("point", {
  expect_error(point(sd, 00))
  expect_error(point(sd, -1))
  expect_error(point(sd, 99))
  expect_error(point(sd, ""))
  i <- pointNames(sd)[1]
  expect_s3_class(point(sd, 1), "R6")
  expect_s3_class(point(sd, i), "R6")
})

test_that("table", {
  expect_s4_class(table(sd), "SingleCellExperiment")
  expect_error(table(sd) <- "")
  expect_error(table(sd) <- NA)
  expect_silent(table(sd) <- NULL)
})

test_that("images", {
  x <- images(sd)
  n <- length(attr(sd, "images"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_ia, logical(1))))
})

test_that("labels", {
  x <- labels(sd)
  n <- length(attr(sd, "labels"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_la, logical(1))))
})

test_that("shapes", {
  x <- shapes(sd)
  n <- length(attr(sd, "shapes"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_df, logical(1))))
})

test_that("points", {
  x <- points(sd)
  n <- length(attr(sd, "points"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_r6, logical(1))))
})

test_that("elementNames", {
  x <- elementNames(sd)
  expect_type(x, "character")
  layers <- attributes(sd)
  layers <- layers[setdiff(names(layers), c("metadata", "class"))]
  .na <- \(.) length(.) == 0 || is(., "name")
  expect_length(x, sum(!vapply(layers, .na, logical(1))))
})

test_that("element", {
  expect_error(element(sd, elementName="foo"))
  expect_error(element(sd, i="foo"))
  expect_error(element(sd, i=12345))

  expect_true(is_ia(element(sd, elementName="images", i=1)))
  expect_true(is_la(element(sd, elementName="labels", i=1)))
  expect_true(is_df(element(sd, elementName="shapes", i=1)))
  expect_true(is_r6(element(sd, elementName="points", i=1)))
})
