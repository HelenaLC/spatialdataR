path <- file.path("extdata", "raccoon", "images", "raccoon")
path <- system.file(path, package="SpatialData", mustWork=TRUE)
i <- readArray(path)

test_that("coords", {
  df <- coords(i)
  md <- metadata(i)
  expect_s4_class(df, "DFrame")
  expect_equal(nrow(df), nrow(md$multiscales))
})

test_that("coords", {
  expect_error(coord(i, 99))
  expect_error(coord(i, ""))
  nm <- coords(i)$output.name[1]
  expect_s4_class(coord(i, nm), "DFrame")
})

test_that("scaleElement,ImageArray", {
  d <- length(dim(i))
  expect_s4_class(scaleElement(i), "ImageArray")
  expect_equal(dim(scaleElement(i, rep(1, d))), dim(i))
  expect_equal(dim(scaleElement(i, rep(2, d))), c(dim(i)[1], 2*dim(i)[-1]))
})

test_that("rotateElement,ImageArray", {
  expect_s4_class(rotateElement(i), "ImageArray")
  expect_equal(dim(rotateElement(i, 000)), dim(i))
  expect_equal(dim(rotateElement(i, 180)), dim(i))
  expect_equal(dim(rotateElement(i, 360)), dim(i))
  expect_equal(dim(rotateElement(i,  90)), dim(i)[c(1, 3, 2)])
  expect_equal(dim(rotateElement(i, 270)), dim(i)[c(1, 3, 2)])
})

test_that("translateElement,ImageArray", {
  j <- translateElement(i)
  expect_s4_class(j, "ImageArray")
  expect_equal(dim(i), dim(j))
})

test_that("transformElement,ImageArray", {
  j <- transformElement(i)
  expect_s4_class(j, "ImageArray")
  expect_identical(metadata(i), metadata(j))
})
