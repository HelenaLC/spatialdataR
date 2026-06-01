x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

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

test_that("SpatialDataImage()", {
    rgb <- \(n) sample(seq_len(255), n, replace=TRUE)
    mat <- array(rgb(3*20*20), dim=c(3,20,20))
    # invalid
    expect_error(SpatialDataImage(mat, 1))
    expect_error(SpatialDataImage(mat, list()))
    # single scale
    expect_silent(SpatialDataImage(list()))
    expect_silent(SpatialDataImage(mat))
    expect_silent(SpatialDataImage(list(mat)))
    expect_silent(SpatialDataImage(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(rgb(prod(.)), dim=.))
    expect_silent(SpatialDataImage(lys))
})

test_that("data(),SpatialDataImage", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- SpatialDataImage(lys)
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})

test_that("SpatialDataLabel()", {
    val <- sample(seq_len(12), 20*20, replace=TRUE)
    mat <- array(val, dim=c(20, 20))
    # invalid
    expect_error(SpatialDataLabel(mat, 1))
    expect_error(SpatialDataLabel(mat, list()))
    # single scale
    expect_silent(SpatialDataLabel(list()))
    expect_silent(SpatialDataLabel(mat))
    expect_silent(SpatialDataLabel(list(mat)))
    expect_silent(SpatialDataLabel(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(sample(seq_len(12), prod(.), replace=TRUE), dim=.))
    expect_silent(SpatialDataLabel(lys))
})

test_that("data(),SpatialDataLabel", {
    dim <- lapply(c(8, 4, 2), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(0L, dim=.))
    lab <- SpatialDataLabel(lys)
    for (. in seq_along(lys))
        expect_identical(data(lab, .), lys[[.]])
    expect_identical(data(lab, Inf), lys[[3]])
    expect_error(data(lab, 0))
    expect_error(data(lab, -1))
    expect_error(data(lab, 99))
    expect_error(data(lab, ""))
    expect_error(data(lab, c(1,2)))
})
