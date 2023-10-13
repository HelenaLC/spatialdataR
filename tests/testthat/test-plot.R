dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")
x <- readSpatialData(dir)

test_that(".rescale", {
    p1 <- plotImage(x, width=NULL)
    p2 <- plotImage(x, width=100)
    p3 <- plotImage(x, width=10)
    .get_lim <- \(.) list(
        .$scales$scales[[1]]$limits,
        .$scales$scales[[2]]$limits)
    ps <- list(p1, p2, p3)
    xy <- dim(image(x))[-1]
    xy <- list(c(-xy[1], 0), c(0, xy[2]))
    for (p in ps) expect_identical(.get_lim(p), xy)
})

test_that("plotImage()", {
    p <- plotImage(x)
    expect_s3_class(p, "gg")
    expect_length(p$data, 0)
    expect_length(p$layers, 1)
})

test_that("plotLabel()", {
    p <- plotLabel(x)
    expect_s3_class(p, "gg")
    expect_length(p$data, 0)
    expect_length(p$layers, 1)
    # simple aes
    p <- plotLabel(x, fill="black")
    expect_identical(
        tabulate(factor(as.array(label(x)) == 0)),
        tabulate(factor(c(p$layers[[1]]$geom_params$raster))))
    # TODO: aes by 'table' data
})

test_that("plotShape,circle", {
    i <- "blobs_circles"
    s <- shape(x, i=i)
    p <- plotShape(x, i=i)
    expect_s3_class(p, "gg")
    df <- p$layers[[1]]$data
    expect_length(unique(df[[1]]), nrow(s))
    xy <- range(do.call(rbind, s$data))+c(-1, 1)*max(s$radius)
    expect_equal(range(df[c("x", "y")]), xy, tolerance=1e-3)
})

test_that("plotShape,polygon", {
    i <- "blobs_polygons"
    s <- shape(x, i=i)
    p <- plotShape(x, i=i)
    expect_s3_class(p, "gg")
    xy <- p$layers[[1]]$data[c("x", "y")]
    expect_equal(as.matrix(xy), as.array(s), ignore_attr=TRUE)
})

test_that("plotPoint", {
    p <- plotPoint(x)
    expect_s3_class(p, "gg")
    p <- plotPoint(x, col="black")
    expect_s3_class(p, "gg")
    expect_identical(p$layers[[1]]$aes_params$colour, "black")
    p <- plotPoint(x, col="x")
    expect_s3_class(p, "gg")
    expect_length(p$layers[[1]]$aes_params, 0)
    expect_s3_class(p$layers[[1]]$mapping$colour, "quosure")
})
