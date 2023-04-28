path <- file.path("extdata", "raccoon", "images", "raccoon")
path <- system.file(path, package="SpatialData")
ia <- readImageArray(path)

test_that("dim", {
  x <- dim(ia)
  expect_type(x, "integer")
})

test_that("dimnames", {
  x <- dimnames(ia)
  expect_type(x, "list")
})

test_that("subsetting", {
  expect_error(ia[1,])
  expect_error(ia[1,,,])
  expect_equal(dim(ia[1,,])[1], 1)
  expect_equal(dim(ia[,1,])[2], 1)
  expect_equal(dim(ia[,,1])[3], 1)
})

# test_that("as.array", {
#   x <- as.array(ia)
#   expect_true(is(x, "array"))
#   expect_equal(dim(x), dim(ia))
# })
