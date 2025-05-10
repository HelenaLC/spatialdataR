arr <- seq_len(12)

test_that("LabelArray()", {
  val <- sample(arr, 20*20, replace=TRUE)
  mat <- array(val, dim=c(20, 20))
  # invalid
  expect_error(LabelArray(mat))
  expect_error(LabelArray(mat, 1))
  expect_error(LabelArray(mat, list()))
  # single scale
  expect_silent(LabelArray(list()))
  expect_silent(LabelArray(list(mat)))
  expect_silent(LabelArray(list(mat), Zattrs()))
  # multiscale
  dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
  lys <- lapply(dim, \(.) array(sample(arr, prod(.), replace=TRUE), dim=.))
  expect_silent(LabelArray(lys))
})

test_that("data(),LabelArray", {
  dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
  lys <- lapply(dim, \(.) array(0, dim=.))
  lab <- LabelArray(lys)
  for (. in seq_along(lys))
    expect_identical(data(lab, .), lys[[.]])
  expect_identical(data(lab, Inf), lys[[3]])
  expect_error(data(lab, 0))
  expect_error(data(lab, -1))
  expect_error(data(lab, 99))
  expect_error(data(lab, ""))
  expect_error(data(lab, c(1,2)))
})

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("[,LabelArray", {
  y <- label(x, i <- "blobs_labels")
  y <- y[,seq_len(32)] # subset to make things harder
  y <- label(x, i <- "blobs_multiscale_labels")
  y <- y[,seq_len(32)] # subset to make things harder
})