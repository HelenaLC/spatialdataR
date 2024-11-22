x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("plotSpatialData()", {
    p <- plotSpatialData()
    expect_s3_class(p, "ggplot")
    expect_length(p$data, 0)
    expect_length(p$layers, 0)
})

.check_xy <- \(p, d) {
    xy <- p$scales$scales
    expect_equal(xy[[1]]$limits, c(0, d[2]))
    expect_equal(xy[[2]]$limits, c(-d[1], 0))
}

test_that("plotImage()", {
    p <- plotSpatialData()
    # simple
    y <- image(x, "blobs_image")
    y <- y[,,seq_len(32)] # subset to make things harder
    image(x, i <- ".") <- y
    q <- p + plotImage(x, i)
    expect_s3_class(q, "ggplot")
    expect_equal(q$coordinates$ratio, 1)
    .check_xy(q, dim(y)[-1])
    # multiscale
    y <- image(x, "blobs_multiscale_image")
    y <- y[,seq_len(32),] # same thing but different
    image(x, i <- ".") <- y
    q <- lapply(seq_along(y@data), \(.) p + plotImage(x, i, k=.))
    lapply(q, .check_xy, dim(y)[-1])
    lapply(seq_along(q), \(.) {
        l <- q[[.]]$layers[[1]]
        l <- l$geom_params$raster
        expect_equal(dim(l), dim(data(y, .))[-1])
    })
})

test_that("plotLabel()", {
    p <- plotSpatialData()
    # simple
    y <- label(x, i <- "blobs_labels")
    y <- y[,seq_len(32)] # subset to make things harder
    q <- p + plotLabel(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    expect_equal(q$coordinates$ratio, 1)
    expect_is(q$layers[[1]]$mapping$fill, "quosure")
    # alpha
    q <- p + plotLabel(x, i, a=a <- runif(1))
    expect_identical(q$layers[[1]]$aes_params$alpha, a)
    expect_error(show(plotSpatialData() + plotLabel(x, i, a=".....")))
    expect_error(show(plotSpatialData() + plotLabel(x, i, a=c(1, 2))))
    # TODO: use 'annotation_raster'
    # TODO: multiscale plotting
})

test_that("plotPoint(),SpatialData", {
    p <- plotSpatialData()
    y <- point(x, i <- "blobs_points")
    df <- collect(data(y))
    # invalid
    expect_error(plotPoint(x, "."))
    expect_error(plotPoint(x, 100))
    expect_error(plotPoint(x, i, c="."))
    # simple
    q <- p + plotPoint(x, i)
    expect_s3_class(q, "ggplot")
    expect_identical(q$layers[[1]]$data, df)
    expect_null(q$layers[[1]]$mapping$colour)
    # coloring by color
    q <- p + plotPoint(x, i, c=. <- "red")
    expect_identical(q$layers[[1]]$data, df)
    expect_identical(q$layers[[1]]$aes_params$colour, .)
    # coloring by coord
    q <- p + plotPoint(x, i, c="x")
    expect_s3_class(q, "ggplot")
    expect_null(q$guides$guides)
    expect_identical(q$layers[[1]]$data, df)
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # coloring by other
    q <- p + plotPoint(x, i, c="genes")
    expect_s3_class(q, "ggplot")
    expect_is(q$guides$guides, "list")
    expect_identical(q$layers[[1]]$data, df)
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # coloring by 'table'
    f <- list(
        numbers=\(n) runif(n),
        letters=\(n) sample(letters, n, TRUE))
    t <- getTable(z <- setTable(x, i, f), i)
    .test <- \(p, t) {
        expect_s3_class(p, "ggplot")
        df <- p$layers[[1]]$data
        ik <- meta(t)$instance_key
        cs <- match(df[[ik]], t[[ik]])
        expect_identical(df[[.]], t[[.]][cs])
        expect_is(p$layers[[1]]$mapping$colour, "quosure")
    }
    # continuous
    q <- p + plotPoint(z, i, c=. <- "numbers")
    expect_null(q$guides$guides)
    .test(q, t)
    # discrete
    q <- p + plotPoint(z, i, c=. <- "letters")
    expect_is(q$guides$guides, "list")
    .test(q, t)
})

test_that("plotPoint(),PointFrame", {
    # simple
    y <- point(x, 1)
    p <- plotPoint(y)
    expect_s3_class(p, "ggplot")
    # coloring by color
    p <- plotPoint(y, c=. <- "red")
    expect_identical(p$layers[[1]]$aes_params$colour, .)
    # invalid
    expect_error(plotPoint(y, c="."))
})

test_that("plotShape(),circles", {
    p <- plotSpatialData()
    # invalid
    expect_error(plotShape(x, "."))
    expect_error(plotShape(x, 100))
    # simple
    y <- shape(x, i <- "blobs_circles")
    q <- p + plotShape(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    df <- st_coordinates(st_as_sf(data(y)))
    fd <- q$layers[[1]]$data[, c("x", "y")]
    expect_equivalent(as.matrix(df), as.matrix(fd))
    expect_null(q$layers[[1]]$mapping$colour)
    expect_s3_class(q$layers[[1]]$geom, "GeomCircle")
    # size
    q <- p + plotShape(x, i, s=s <- runif(1, 1, 10))
    expect_s3_class(q$layers[[1]]$geom, "GeomPoint")
    expect_identical(q$layers[[1]]$aes_params$size, s)
    expect_error(show(plotSpatialData() + plotShape(x, i, s=".")))
    # color
    expect_error(plotShape(x, i, c="."))
    q <- p + plotShape(x, i, c=NA) # none
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- 1) # numeric
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- "red") # string
    expect_identical(q$layers[[1]]$aes_params$col, c)
})

test_that("plotShape(),polygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_polygons")
    # simple
    q <- p + plotShape(x, i, c=NULL)
    expect_s3_class(q, "ggplot")
    df <- st_coordinates(st_as_sf(data(y)))[, c(1, 2)]
    fd <- q$layers[[1]]$data[, c("x", "y")]
    expect_equivalent(as.matrix(df), as.matrix(fd))
    expect_s3_class(q$layers[[1]]$geom, "GeomPolygon")
    expect_is(q$layers[[1]]$mapping$colour, "quosure")
    # color
    expect_error(plotShape(x, i, c="."))
    q <- p + plotShape(x, i, c=NA) # none
    expect_null(q$layers[[1]]$mapping$colour)
    q <- p + plotShape(x, i, c=c <- 1) # numeric
    expect_identical(q$layers[[1]]$aes_params$col, c)
    q <- p + plotShape(x, i, c=c <- "red") # string
    expect_identical(q$layers[[1]]$aes_params$col, c)
    # coloring by 'table'
    f <- list(
        numbers=\(n) runif(n),
        letters=\(n) sample(letters, n, TRUE))
    t <- getTable(y <- setTable(x, i, f), i)
    q <- p + plotShape(y, i, c=. <- "numbers")
    expect_s3_class(q, "ggplot")
    expect_null(q$guides$guides)
    q <- p + plotShape(y, i, c=. <- "letters")
    expect_s3_class(q, "ggplot")
    df <- q$layers[[1]]$data
    expect_equal(base::table(t[[.]]), base::table(df[[.]])/4)
})

test_that("plotShape(),multipolygons", {
    p <- plotSpatialData()
    y <- shape(x, i <- "blobs_multipolygons")
    # simple
    q <- p + plotShape(x, i)
    expect_s3_class(q, "ggplot")
    df <- q$layers[[1]]$data
    # coloring by string
    q <- p + plotShape(x, i, c=. <- "red")
    expect_identical(q$layers[[1]]$aes_params$col, .)
    fd <- q$layers[[1]]$data
    expect_identical(df, fd)
})
