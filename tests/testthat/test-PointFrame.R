require(dplyr, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("names", {
    y <- names(p <- point(x))
    expect_is(y, "character")
    expect_true(!any(grepl("_dask_", y)))
    expect_identical(y, (. <- names(data(p)))[!grepl("dask", .)])
})

test_that("$,[[", {
    # names
    nms <- .DollarNames(p <- point(x))
    expect_is(nms, "character")
    expect_length(nms, ncol(p))
    expect_identical(nms, (. <- names(data(p)))[!grepl("dask", .)])
    # valid
    lapply(seq_len(ncol(p)), \(i) {
        j <- names(p)[i]
        y <- do.call(`$`, list(p, j))
        z <- pull(data(p), j, as_vector=TRUE)
        expect_identical(y, z)
        expect_identical(y, z <- do.call(`[[`, list(p, i)))
        expect_identical(z, do.call(`[[`, list(p, j)))
    })
    # invalid
    expect_error(p[[0]])
    expect_error(p[[ncol(p) + 1]])
    i <- (. <- names(data(p)))[grepl("dask", .)]
    expect_error(do.call(`$`, list(p, i)))
    expect_error(do.call(`[[`, list(p, i)))
})

test_that("filter", {
    n <- length(p <- point(x))
    expect_length(filter(p), n)
    expect_length(filter(p, x > Inf), 0)
    expect_error(filter(p, z == 1))
})

test_that("select", {
    p <- point(x)
    replicate(3, {
        n <- sample(ncol(p), 1)
        i <- sample(names(p), n)
        y <- select(p, i); z <- data(p)[, i]
        expect_equal(collect(data(y)), collect(z))
    })
})

test_that("as.data.frame", {
    y <- as.data.frame(p <- point(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), dim(p))
    expect_equal(names(y), names(p))
    expect_identical(y, (. <- collect(data(p)))[, !grepl("dask", names(.))])
})