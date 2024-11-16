x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("axes", {
    # image
    y <- axes(image(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), c(3, 2))
    # label
    y <- axes(label(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), c(2, 2))
    # shape
    y <- axes(shape(x))
    expect_is(y, "character")
    expect_length(y, 2)
    # point
    y <- axes(point(x))
    expect_is(y, "character")
    expect_length(y, 2)
})