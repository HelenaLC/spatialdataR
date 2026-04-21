require(dplyr, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("names", {
    y <- names(p <- point(x))
    expect_is(y, "character")
    expect_true(!any(grepl("_dask_", y)))
    expect_identical(y, (. <- colnames(data(p)))[!grepl("dask", .)])
})

test_that("$,[[", {
    # names
    nms <- .DollarNames(p <- point(x))
    expect_is(nms, "character")
    expect_length(nms, ncol(p))
    expect_identical(nms, (. <- colnames(data(p)))[!grepl("dask", .)])
    # valid
    lapply(seq_len(ncol(p)), \(i) {
        j <- names(p)[i]
        y <- do.call(`$`, list(p, j))
        z <- pull(data(p), j)
        expect_identical(y, z)
        expect_identical(y, z <- do.call(`[[`, list(p, i)))
        expect_identical(z, do.call(`[[`, list(p, j)))
    })
    # invalid
    # expect_error(p[[0]])
    expect_error(p[[ncol(p) + 1]])
    i <- (. <- colnames(data(p)))[grepl("dask", .)]
    expect_error(do.call(`$`, list(p, i)))
    expect_error(do.call(`[[`, list(p, i)))
})

test_that("filter", {
    n <- length(p <- point(x))
    expect_length(filter(p), n)
    expect_length(filter(p, x > 10000000), 0)
    f <- \() filter(p, z == 1)
    expect_error(show(f()))
})

test_that("select", {
    p <- point(x)
    replicate(3, {
        n <- sample(ncol(p), 1)
        i <- sample(names(p), n)
        y <- select(p, all_of(i))
        z <- data(p) |> select(all_of(i))
        expect_equal(collect(data(y)), collect(z))
    })
})

test_that("as.data.frame", {
    y <- as.data.frame(p <- point(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), dim(p))
    expect_equal(names(y), names(p))
    expect_identical(y, (. <- as.data.frame(collect(data(p))))[, !grepl("dask", names(.))])
})
