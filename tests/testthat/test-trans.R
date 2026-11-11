zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
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
    expect_s4_class(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_type(md, "list")
    expect_type(unlist(md), "double")
    expect_equal(md[[1]], c(0, dim(y)[3]))
    expect_equal(md[[2]], c(n, dim(y)[2]+n))
    # col
    t <- c(0,0,n <- sample(77, 1))
    z <- translation(y <- x[,-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_s4_class(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_type(md, "list")
    expect_type(unlist(md), "double")
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
    expect_s4_class(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_type(md, "list")
    expect_type(unlist(md), "double")
    expect_equal(md[[1]], c(0, dim(y)[2]))
    expect_equal(md[[2]], c(n, dim(y)[1]+n))
    # col
    t <- c(0, n <- sample(77, 1))
    z <- translation(y <- x[-1,-c(1,2)], t)
    expect_equal(dim(z), dim(y))
    expect_s4_class(data(z), "DelayedArray")
    md <- metadata(z)$wh
    expect_type(md, "list")
    expect_type(unlist(md), "double")
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

.xy <- \(x) unname(as.matrix(centroids(x)[unlist(axes(x))]))

test_that("trans,sdFrame", {
    for (x in c(as.list(points(sd)), as.list(shapes(sd)))) {
        n <- length(unlist(axes(x)))
        
        # identity
        expect_identical(rotate(x, 0), x)
        expect_identical(scale(x, rep(1, n)), x)
        expect_identical(translation(x, rep(0, n)), x)
        expect_identical(affine(x, cbind(diag(n), 0)), x)
        
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
        
        # affine
        old <- extent(x)
        # shift x by +7, y by -7
        mtx <- rbind(
            c(1,0,dx <- -7),
            c(0,1,dy <- +7))
        expect_equal(
            extent(affine(x, mtx)),
            within(old, { x <- x+dx; y <- y+dy }))
        # scale x by 33, y by 11
        mtx <- rbind(
            c(sx <- 33,0,0),
            c(0,sy <- 11,0))
        expect_equal(
            lapply(extent(affine(x, mtx)), diff),
            within(lapply(old, diff), { x <- x*sx; y <- y*sy }))
        # rotate by 30/90 degrees; should match 'rotate()'
        if (n == 2) for (deg in c(30, 90)) {
            rad <- deg * pi / 180
            R <- matrix(c(cos(rad), -sin(rad), sin(rad), cos(rad)), 2, 2)
            mtx <- cbind(t(R), 0)
            expect_equal(.xy(affine(x, mtx)), .xy(rotate(x, deg)))
        }
        # dimnames should be ignored
        mtx <- cbind(diag(n) + seq_len(n) / 10, seq_len(n))
        nms <- list(letters[seq_len(n)], LETTERS[seq_len(n+1)])
        y <- affine(x, `dimnames<-`(mtx, nms))
        expect_equal(.xy(y), .xy(affine(x, mtx)))
        # reverse; should invert (not negate) the matrix
        y <- affine(affine(x, mtx), mtx, rev=TRUE)
        expect_equal(.xy(y), .xy(x))
        inv <- solve(rbind(mtx, c(rep(0, n), 1)))[seq_len(n), ]
        expect_equal(.xy(affine(x, mtx, rev=TRUE)), .xy(affine(x, inv)))
        y <- affine(x, `dimnames<-`(mtx, nms), rev=TRUE)
        expect_equal(.xy(y), .xy(affine(x, inv)))
        # (also far from the origin)
        mtx <- cbind(diag(n), 5e7)
        y <- affine(affine(x, mtx), mtx, rev=TRUE)
        expect_equal(.xy(y), .xy(x))
        # singular matrices can't be reversed
        mtx <- cbind(matrix(1, n, n), 0)
        expect_error(affine(x, mtx, rev=TRUE), "not invertible")
        # matrix of ones isn't the identity (all axes become their sum+1)
        y <- affine(x, matrix(1, n, n+1))
        y <- sf::st_coordinates(sf::st_as_sf(data(y)))[, seq_len(n)]
        z <- sf::st_coordinates(sf::st_as_sf(data(x)))[, seq_len(n)]
        expect_equal(unname(y), matrix(rowSums(z)+1, nrow(z), n))
    }
})

test_that("transform,sdFrame w/ affine axes", {
    x <- shape(sd)
    # x' = x + 2y + 3, y' = 4x + 5y + 6
    A <- rbind(c(1, 2, 3), c(4, 5, 6))
    .rows <- \(m) lapply(seq_len(nrow(m)), \(.) m[., ])
    # add affine 'm', with in/output axes modified by 'f'/'g'
    .aff <- \(x, m, f=identity, g=f) {
        x <- addCT(x, "aff", "affine", .rows(m))
        md <- meta(x)
        i <- match("aff", CTname(x))
        ct <- md[["coordinateTransformations"]][[i]]
        ct$input$axes <- f(ct$input$axes)
        ct$output$axes <- g(ct$output$axes)
        md[["coordinateTransformations"]][[i]] <- ct
        meta(x) <- md
        return(x)
    }
    .ok <- \(y) expect_equal(.xy(transform(y, "aff")), .xy(affine(x, A)))
    # axes as for the element, or missing
    .ok(.aff(x, A))
    .ok(.aff(x, A, \(.) NULL))
    # in- and/or output axes in reverse order
    .ok(.aff(x, A[2:1, c(2, 1, 3)], rev))
    .ok(.aff(x, A[2:1, ], identity, rev))
    .ok(y <- .aff(x, A[, c(2, 1, 3)], rev, identity))
    expect_equal(
        .xy(transform(y, "aff", rev=TRUE)),
        .xy(affine(x, A, rev=TRUE)))
    # axes other than the element's
    z <- \(.) `[[<-`(., 1, list(name="z", type="space"))
    expect_error(transform(.aff(x, A, z, identity), "aff"), "don't include")
    expect_error(transform(.aff(x, A, identity, z), "aff"), "don't include")
    # data that don't fit the axes
    y <- .aff(x, rbind(A, c(0, 0, 1)))
    expect_error(transform(y, "aff"), "2x3 matrix")
    y <- .aff(x, cbind(A, 0))
    expect_error(transform(y, "aff"), "2x3 matrix")
    # more output than input axes (rows follow the output axes)
    z <- \(.) c(., list(list(name="z", type="space")))
    .ok(.aff(x, rbind(A, c(0, 0, 1)), identity, z))
    # 3D; 'addCT()' should use the element's (xyz) axes
    p <- data.frame(x=c(1, 2, 3), y=c(4, 5, 6), z=c(7, 8, 9))
    p <- SpatialDataPoint(p, meta=SpatialDataAttrs(type="point", dim=3))
    m <- rbind(c(1, 2, 0, 3), c(4, 5, 0, 6), c(0, 0, 1, 0))
    q <- .aff(p, m)
    ct <- CTlist(q)[[match("aff", CTname(q))]]
    expect_identical(.ax_names(ct$input$axes), axes(p, "name"))
    expect_identical(.ax_names(ct$output$axes), axes(p, "name"))
    expect_equal(extent(transform(q, "aff")), extent(affine(p, m)))
    # (x' = x + 2y + 3 and y' = 4x + 5y + 6)
    expect_equal(extent(affine(p, m)), list(x=c(12, 18), y=c(30, 48)))
    expect_equal(extent(affine(affine(p, m), m, rev=TRUE)), extent(p))
})
