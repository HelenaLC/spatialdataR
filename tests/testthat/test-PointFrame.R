path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
path <- file.path(path, "points", "blobs_points")
x <- readPoints(path)

pq <- list.files(path, "*\\.parquet$", recursive=TRUE, full.names=TRUE)
y <- read_parquet(pq, as_data_frame=TRUE)
y <- y[setdiff(names(y), "__null_dask_index__")]

test_that("length,PointFrame", {
    z <- length(x)
    expect_type(z, "integer")
    expect_identical(z, nrow(y))
})

test_that("dim,PointFrame", {
    z <- dim(x)
    expect_type(z, "integer")
    expect_identical(z, dim(y))
})

test_that("names,PointFrame", {
    z <- names(x)
    expect_type(z, "character")
    expect_identical(z, names(y))
})

test_that("$,PointFrame", {
    # need 'do.call' here to work
    # around 'dplyr'-like evaluation
    for (. in names(y))
        expect_identical(
            do.call(`$`, list(x, .)),
            do.call(`$`, list(y, .)))
})

test_that("[[,PointFrame", {
    for (. in names(y))
        expect_identical(x[[.]], y[[.]])
})

test_that("df,PointFrame", {
    expect_identical(as.data.frame(x), y)
    expect_identical(as(x, "data.frame"), y)
})
