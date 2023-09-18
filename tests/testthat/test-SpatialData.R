is_ia <- \(.) is(., "ImageArray")
is_la <- \(.) is(., "LabelArray")
is_sf <- \(.) is(., "ShapeFrame")
is_pf <- \(.) is(., "PointFrame")

# constructor ----

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

path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)

# names ----

test_that("imageNames", {
  y <- imageNames(x)
  n <- length(attr(x, "images"))
  expect_length(y, n)
  if (n > 0) expect_type(y, "character")
})

test_that("labelNames", {
  y <- labelNames(x)
  n <- length(attr(x, "labels"))
  expect_length(x, n)
  if (n > 0) expect_type(y, "character")
})

test_that("shapeNames", {
  y <- shapeNames(x)
  n <- length(attr(x, "shapes"))
  expect_length(y, n)
  if (n > 0) expect_type(y, "character")
})

test_that("pointNames", {
  y <- pointNames(x)
  n <- length(attr(x, "points"))
  expect_length(y, n)
  if (n > 0) expect_type(y, "character")
})

test_that("elementNames", {
    y <- elementNames(x)
    expect_type(y, "character")
    layers <- attributes(x)
    layers <- layers[setdiff(names(layers), c("metadata", "class"))]
    .na <- \(.) length(.) == 0 || is(., "name")
    expect_length(y, sum(!vapply(layers, .na, logical(1))))
})

# access single ----

test_that("image", {
  expect_error(image(x, 00))
  expect_error(image(x, -1))
  expect_error(image(x, 99))
  expect_error(image(x, ""))
  i <- imageNames(x)[1]
  expect_s4_class(image(x, 1), "ImageArray")
  expect_s4_class(image(x, i), "ImageArray")
})

test_that("label", {
  expect_error(label(x, 00))
  expect_error(label(x, -1))
  expect_error(label(x, 99))
  expect_error(label(x, ""))
  i <- labelNames(x)[1]
  expect_s4_class(label(x, 1), "LabelArray")
  expect_s4_class(label(x, i), "LabelArray")
})

test_that("shape", {
  expect_error(shape(x, 00))
  expect_error(shape(x, -1))
  expect_error(shape(x, 99))
  expect_error(shape(x, ""))
  i <- shapeNames(x)[1]
  expect_s4_class(shape(x, 1), "DFrame")
  expect_s4_class(shape(x, i), "DFrame")
})

test_that("point", {
  expect_error(point(x, 00))
  expect_error(point(x, -1))
  expect_error(point(x, 99))
  expect_error(point(x, ""))
  i <- pointNames(x)[1]
  expect_s4_class(point(x, 1), "PointFrame")
  expect_s4_class(point(x, i), "PointFrame")
})

test_that("table", {
  expect_s4_class(table(x), "SingleCellExperiment")
  expect_error(table(x) <- "")
  expect_error(table(x) <- NA)
  expect_silent(table(x) <- NULL)
})

test_that("element", {
    expect_error(element(x, elementName="foo"))
    expect_error(element(x, i="foo"))
    expect_error(element(x, i=12345))

    expect_true(is_ia(element(x, elementName="images", i=1)))
    expect_true(is_la(element(x, elementName="labels", i=1)))
    expect_true(is_sf(element(x, elementName="shapes", i=1)))
    expect_true(is_pf(element(x, elementName="points", i=1)))
})

# access multiple ----

test_that("images", {
  x <- images(x)
  n <- length(attr(x, "images"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_ia, logical(1))))
})

test_that("labels", {
  x <- labels(x)
  n <- length(attr(x, "labels"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_la, logical(1))))
})

test_that("shapes", {
  x <- shapes(x)
  n <- length(attr(x, "shapes"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_sf, logical(1))))
})

test_that("points", {
  x <- points(x)
  n <- length(attr(x, "points"))
  expect_type(x, "list")
  if (n > 0) expect_true(all(vapply(x, is_pf, logical(1))))
})

# replace multiple ----

test_that("images<-", {
    y <- x
    expect_silent(images(y) <- list())
    expect_length(images(y), 0)
    y <- x
    ia <- ImageArray()
    expect_silent(images(y) <- list(ia))
    expect_identical(image(y), ia)
    expect_null(imageNames(y))
})
test_that("labels<-", {
    y <- x
    expect_silent(labels(y) <- list())
    expect_length(labels(y), 0)
    y <- x
    la <- LabelArray()
    expect_silent(labels(y) <- list(la))
    expect_identical(label(y), la)
    expect_null(labelNames(y))
})

# replace single ----

test_that("image<-", {
    y <- ImageArray()
    expect_error(image(x, 99) <- y)
    expect_silent(image(x, "foo") <- y)
    expect_true("foo" %in% imageNames(x))
})

test_that("label<-", {
    y <- LabelArray()
    expect_error(label(x, 99) <- y)
    expect_silent(label(x, "foo") <- y)
    expect_true("foo" %in% labelNames(x))
})

test_that("shape<-", {
    y <- ShapeFrame()
    expect_error(shape(x, 99) <- y)
    expect_silent(shape(x, "foo") <- y)
    expect_true("foo" %in% shapeNames(x))
})

test_that("point<-", {
    expect_error(point(x) <- "x")
    expect_error(point(x) <- 123)

    y <- x; point(y) <- NULL
    expect_length(points(y), 0)

    y <- x; point(y, 1) <- NULL
    expect_length(points(y), 0)

    y <- PointFrame()
    expect_error(point(x, 99) <- y)
    expect_silent(point(x, "foo") <- y)
    expect_true("foo" %in% pointNames(x))
})
