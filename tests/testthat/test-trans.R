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

# point/shape ----

test_that("trans,sdFrame", {
    .xy <- \(x) unname(as.matrix(centroids(x)[unlist(axes(x))]))
    for (x in c(points(sd), shapes(sd))) {
        n <- length(unlist(axes(x)))
        
        # identity
        expect_identical(rotate(x, 0), x)
        expect_identical(scale(x, rep(1, n)), x)
        expect_identical(translation(x, rep(0, n)), x)
        
        # invalid
        expect_error(translation(x, "a"))     # non-numeric
        expect_error(rotate(x, c(1, 2)))      # non-scalar rotation
        expect_error(scale(x, rep(-1, n)))    # negative scale
        expect_error(scale(x, rep(1, n + 1))) # wrong dims
        
        # translation
        t <- runif(n, -10, 10)
        y <- translation(x, t)
        expect_equal(.xy(y), sweep(.xy(x), 2, t, "+"))
        
        # scale
        s <- runif(n, 0.5, 2)
        y <- scale(x, s)
        expect_equal(.xy(y), sweep(.xy(x), 2, s, "*"))
        
        # rotate
        if (n == 2) {
            deg <- sample(360, 1)
            rad <- deg * pi / 180
            y <- rotate(x, deg)
            R <- matrix(c(cos(rad), -sin(rad), sin(rad), cos(rad)), 2, 2)
            expect_equal(.xy(y), .xy(x) %*% R)
        }
    }
})
