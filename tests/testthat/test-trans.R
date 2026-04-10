zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="SpatialData")
sd <- readSpatialData(zs, tables=FALSE)

test_that("mirror,sdArray", {
    x <- label(sd, 1)[-1,-c(1,2)]
    expect_error(mirror(x, "x"))
    expect_identical(mirror(x, "v"), flip(x))
    expect_identical(mirror(x, "h"), flop(x))
    # vertical reflection
    y <- flip(x)
    expect_identical(dim(y), dim(x))
    expect_equal(data(y)[1, ], rev(data(x)[1, ]))
    expect_equal(data(y)[, 1], data(x)[, ncol(x)])
    # horizontal reflection
    y <- flop(x)
    expect_identical(dim(y), dim(x))
    expect_equal(data(y)[, 1], rev(data(x)[, 1]))
    expect_equal(data(y)[1, ], data(x)[nrow(x), ])
})

test_that("translation,imageArray", {
    x <- image(sd, 1)
    # identity
    y <- translation(x, c(0,0,0))
    expect_identical(x, y)
    expect_null(metadata(y)$wh)
    # invalid
    expect_error(translation(x, numeric(2)))
    expect_error(translation(x, numeric(4)))
    expect_error(translation(x, character(3)))
    # row
    t <- c(0,n <- sample(77, 1),0)
    z <- translation(y <- x[,-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_is(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_is(md, "list")
    expect_is(unlist(md), "numeric")
    expect_equal(md[[1]], c(0, dim(y)[3]))
    expect_equal(md[[2]], c(n, dim(y)[2]+n))
    # col
    t <- c(0,0,n <- sample(77, 1))
    z <- translation(y <- x[,-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_is(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_is(md, "list")
    expect_is(unlist(md), "numeric")
    expect_equal(md[[1]], c(n, dim(y)[3]+n))
    expect_equal(md[[2]], c(0, dim(y)[2]))
})

test_that("translation,labelArray", {
    x <- label(sd, 1)
    # identity
    y <- translation(x, c(0,0))
    expect_identical(x, y)
    expect_null(metadata(y)$wh)
    # invalid
    expect_error(translation(x, numeric(1)))
    expect_error(translation(x, numeric(3)))
    expect_error(translation(x, character(2)))
    # row
    t <- c(n <- sample(77, 1), 0)
    z <- translation(y <- x[-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_is(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_is(md, "list")
    expect_is(unlist(md), "numeric")
    expect_equal(md[[1]], c(0, dim(y)[2]))
    expect_equal(md[[2]], c(n, dim(y)[1]+n))
    # col
    t <- c(0, n <- sample(77, 1))
    z <- translation(y <- x[-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_is(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_is(md, "list")
    expect_is(unlist(md), "numeric")
    expect_equal(md[[1]], c(n, dim(y)[2]+n))
    expect_equal(md[[2]], c(0, dim(y)[1]))
    # TODO: multiscale
    # x <- label(sd, 2)
    # t <- c(n <- nrow(x), 0)
    # y <- translation(x, t)
    # dx <- vapply(data(x, NULL), dim, integer(2))
    # dy <- vapply(data(y, NULL), dim, integer(2))
    # expect_equal(dx[1,], dy[1,]/2)
    # expect_identical(dx[2,], dy[2,])
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
    replicate(5, {
        n <- f(); m <- f()
        y <- translation(x, c(n,m))
        expect_equal(x$x+n, y$x)
        expect_equal(x$y+m, y$y)
        for (. in i) expect_identical(x[[.]], y[[.]])
    })
})

test_that("scale,PointFrame", {
    x <- point(sd, 1)
    y <- scale(x, c(1, 1))
    expect_identical(x, y)
    # invalid
    expect_error(scale(x, -c(1, 1)))
    expect_error(scale(x, c(1, -1)))
    expect_error(scale(x, numeric(1)))
    expect_error(scale(x, numeric(3)))
    expect_error(scale(x, logical(2)))
    expect_error(scale(x, character(2)))
    expect_error(scale(x, NA*numeric(2)))
    expect_error(scale(x, c(Inf, Inf)))
    # valid
    i <- setdiff(names(x), c("x", "y"))
    f <- \() replicate(2, runif(1, 0.1, 2))
    g <- \(.) cbind(.$x, .$y)
    replicate(5, {
        y <- scale(x, t <- f())
        expect_equal(sweep(g(x), 2, t, `*`), g(y))
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
    f <- \() sample(777, 1)*sample(c(-1, 1), 1)
    g <- \(.) cbind(.$x, .$y)
    replicate(5, {
        y <- rotate(x, t <- f())
        R <- .R(t*base::pi/180)
        expect_equal(t(R %*% t(g(x))), g(y))
        for (. in i) expect_identical(x[[.]], y[[.]])
    })
})
