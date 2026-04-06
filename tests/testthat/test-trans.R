zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="SpatialData")
sd <- readSpatialData(zs, tables=FALSE)

test_that("translation,imageArray", {
    x <- image(sd, 1)
    y <- translation(x, c(0,0,0))
    expect_identical(x, y)
    expect_error(translation(x, numeric(2)))
    expect_error(translation(x, numeric(4)))
    expect_error(translation(x, character(3)))
    # row
    t <- c(0,n <- sample(77, 1),0)
    y <- translation(x, t)
    expect_equal(dim(y), dim(x)+t)
    expect_is(data(y), "DelayedArray")
    expect_true(sum(data(y)[,seq_len(n),]) == 0)
    # col
    t <- c(0,0,n <- sample(77, 1))
    y <- translation(x, t)
    expect_equal(dim(y), dim(x)+t)
    expect_is(data(y), "DelayedArray")
    expect_true(sum(data(y)[,,seq_len(n)]) == 0)
})

test_that("translation,labelArray", {
    x <- label(sd, 1)
    y <- translation(x, c(0,0))
    expect_identical(x, y)
    expect_error(translation(x, numeric(1)))
    expect_error(translation(x, numeric(3)))
    expect_error(translation(x, character(2)))
    # row
    t <- c(n <- sample(77, 1), 0)
    y <- translation(x, t)
    expect_equal(dim(y), dim(x)+t)
    expect_is(data(y), "DelayedArray")
    expect_true(sum(data(y)[seq_len(n),]) == 0)
    # col
    t <- c(0, n <- sample(77, 1))
    y <- translation(x, t)
    expect_equal(dim(y), dim(x)+t)
    expect_is(data(y), "DelayedArray")
    expect_true(sum(data(y)[,seq_len(n)]) == 0)
    # multiscale
    x <- label(sd, 2)
    t <- c(n <- nrow(x), 0)
    y <- translation(x, t)
    dx <- vapply(data(x, NULL), dim, integer(2))
    dy <- vapply(data(y, NULL), dim, integer(2))
    expect_equal(dx[1,], dy[1,]/2)
    expect_identical(dx[2,], dy[2,])
})

test_that("translation,PointFrame", {
    x <- point(sd, 1)
    y <- translation(x, c(0,0))
    expect_identical(x, y)
    # invalid
    expect_error(translation(x, numeric(1)))
    expect_error(translation(x, numeric(3)))
    expect_error(translation(x, logical(2)))
    expect_error(translation(x, c(Inf, Inf)))
    expect_error(translation(x, character(2)))
    expect_error(translation(x, NA*numeric(2)))
    # valid
    i <- setdiff(names(x), c("x", "y"))
    f <- \() sample(33, 1)*sample(c(-1, 1), 1)
    replicate(10, \() {
        n <- f(); m <- f()
        y <- translation(x, c(n,m))
        expect_identical(x$x, y$x+n)
        expect_identical(x$y, y$y+m)
        expect_identical(x[,i], y[,i])
    })
})

test_that("rotate,PointFrame", {
    x <- point(sd, 1)
    y <- rotate(x, 0)
    expect_identical(x, y)
    # invalid
    expect_error(rotate(x, Inf))
    expect_error(rotate(x, numeric(2)))
    expect_error(rotate(x, logical(1)))
    expect_error(rotate(x, character(1)))
    expect_error(rotate(x, NA*numeric(1)))
    # valid
    i <- setdiff(names(x), c("x", "y"))
    f <- \() sample(360, 1)*sample(c(-1, 1), 1)
    g <- \(x) cbind(x$x, x$y)
    replicate(10, \() {
        y <- rotate(x, t <- f())
        R <- .R(t*base::pi/180)
        expect_identical(g(x) %*% R, g(y))
        for (. in i) expect_identical(x[[.]], y[[.]])
    })
})
