x <- readSpatialData(system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE))

test_that("plotImage", {
    p <- plotImage(x)
    expect_s4_class(p, "ggSD")
})

test_that("plotLabel", {
    p <- plotLabel(x)
    expect_s4_class(p, "ggSD")
})

test_that("plotShape,polygon", {
    p <- plotShape(x)
    expect_s4_class(p, "ggSD")
    a <- as.array(shape(x))
    xy <- p$layers[[1]]$data[c("x", "y")]
    expect_equal(as.matrix(xy), a, ignore_attr=TRUE)
    # TODO: circle
})

test_that("plotPoint", {
    p <- plotPoint(x)
    expect_s4_class(p, "ggSD")
    p <- plotPoint(x, col="black")
    expect_s4_class(p, "ggSD")
    expect_identical(p$layers[[1]]$aes_params$colour, "black")
    p <- plotPoint(x, col="x")
    expect_s4_class(p, "ggSD")
    expect_length(p$layers[[1]]$aes_params, 0)
    expect_s3_class(p$layers[[1]]$mapping$colour, "quosure")
})
