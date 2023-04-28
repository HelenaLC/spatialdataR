path <- file.path("extdata", "mibitof")
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
  expect_length(x, sum(vapply(layers, length, integer(1)) != 0))
})

test_that("element", {
  expect_error(element(spd, elementName="foo"))
  expect_error(element(spd, which="foo"))
  expect_error(element(spd, which=12345))

  expect_true(is_ia(element(spd, elementName="images", which=1)))
  expect_true(is_ia(element(spd, elementName="labels", which=1)))
  # expect_true(is_df(element(spd, elementName="shapes", which=1)))
  # expect_true(is_r6(element(spd, elementName="points", which=1)))
})
