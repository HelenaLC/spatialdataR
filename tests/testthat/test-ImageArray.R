test_that("ImageArray", {
  ia <- ImageArray()
  expect_s4_class(ia, "ImageArray")
  expect_true(is.na(as.array(ia)))
  expect_length(dim(ia), 1)
  expect_equal(dim(ia), 1)
})

path <- system.file("extdata", "raccoon", package="SpatialData", mustWork=TRUE)
path <- file.path(path, "images", "raccoon")
md <- Rarr::zarr_overview(
  list.dirs(path, recursive=FALSE),
  as_data_frame=TRUE)
ia <- readArray(path)

test_that("dim", {
  x <- dim(ia)
  expect_type(x, "integer")
  expect_identical(x, md$dim[[1]])
})

test_that("dimnames", {
  x <- dimnames(ia)
  expect_type(x, "list")
})

test_that("[", {
  expect_error(ia[1,])
  expect_error(ia[1,,,])
  expect_equal(dim(ia[1,,])[1], 1)
  expect_equal(dim(ia[,1,])[2], 1)
  expect_equal(dim(ia[,,1])[3], 1)
})

test_that("as.array", {
  x <- as.array(ia)
  expect_true(is(x, "array"))
  expect_equal(dim(x), dim(ia))
})

test_that("aperm", {
  expect_equal(dim(aperm(ia)), rev(dim(ia)))
  expect_equal(dim(aperm(ia)), dim(aperm(as.array(ia))))
})
