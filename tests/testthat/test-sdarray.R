x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

test_that("data_type()", {
    # image
    za <- data(image(x))
    dt <- data_type(za)
    expect_length(dt, 1)
    expect_is(dt, "character")
    expect_identical(dt, "float64")
    expect_identical(dt, data_type(za[1,,]))
    expect_identical(dt, data_type(image(x)))
    # label
    za <- data(label(x))
    dt <- data_type(za)
    expect_length(dt, 1)
    expect_is(dt, "character")
    expect_identical(dt, "int16")
    expect_identical(dt, data_type(head(za)))
    expect_identical(dt, data_type(label(x)))
})
