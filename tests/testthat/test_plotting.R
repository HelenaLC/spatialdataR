x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("plotSpatialData()", {
    p <- plotSpatialData()
    expect_s3_class(p, "ggplot")
    expect_length(p$data, 0)
    expect_length(p$layers, 0)
})

test_that("plotImage()", {
    p <- plotSpatialData()
    .check_xy <- \(p, d) {
        xy <- p$scales$scales
        expect_equal(xy[[1]]$limits, c(0, d[3]))
        expect_equal(xy[[2]]$limits, c(-d[2], 0))
    }
    # simple
    y <- image(x, "blobs_image")
    y <- y[,,seq_len(32)] # subset to make things harder
    image(x, i <- ".") <- y
    q <- p + plotImage(x, i)
    expect_s3_class(q, "ggplot")
    expect_length(q$data, 0)
    expect_equal(q$coordinates$ratio, 1)
    .check_xy(q, dim(y))
    # multiscale
    y <- image(x, "blobs_multiscale_image")
    y <- y[,seq_len(32),] # same thing but different
    image(x, i <- ".") <- y
    q <- lapply(seq_along(y@data), \(.) p + plotImage(x, i, k=.))
    lapply(q, .check_xy, dim(y))
    lapply(seq_along(q), \(.) {
        l <- q[[.]]$layers[[1]]
        l <- l$geom_params$raster
        expect_equal(dim(l), dim(data(y, .))[-1])
    })
})