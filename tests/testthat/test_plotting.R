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

test_that("plotPoint()", {
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
    expect_null(q$layers[[1]]$mapping$colour)
    expect_identical(q$layers[[1]]$data, df)
    # coloring by color
    q <- p + plotPoint(x, i, c=. <- "black")
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
})
