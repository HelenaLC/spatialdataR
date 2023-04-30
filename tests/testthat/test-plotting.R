path <- system.file("extdata", "raccoon",
    package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)

test_that("plotElement,ImageArray", {
    y <- image(x)
    p <- plotElement(y)
    expect_s3_class(p, "ggplot")
    xy <- p$scales$scales
    expect_equal(xy[[1]]$limits, c(0, dim(y)[3]))
    expect_equal(xy[[2]]$limits, c(-dim(y)[2], 0))
})

test_that("plotElement,LabelArray", {
    y <- label(x)
    p <- plotElement(y)
    expect_s3_class(p, "ggplot")
    xy <- p$scales$scales
    expect_equal(xy[[1]]$limits, c(0, dim(y)[2]))
    expect_equal(xy[[2]]$limits, c(-dim(y)[1], 0))
})

test_that("plotElement,ShapeArray", {
    y <- shape(x)
    p <- plotElement(y)
    expect_s3_class(p, "ggplot")
    a <- as.array(y)
    xy <- p$scales$scales
    r <- c(-1, 1)*y$radius[1]
    expect_equal(xy[[1]]$limits, range(a[, 1])+r, tolerance=1e3)
    expect_equal(xy[[2]]$limits, range(a[, 2])+r, tolerance=1e3)
    # TODO: polygon
})

test_that("plotSD", {
    p <- plotSD(x)
    n <- length(elementNames(x))
    expect_s3_class(p, "ggplot")
    expect_equal(length(p$layers), n)
    # w/o image
    p <- plotSD(x, image=NULL)
    expect_equal(length(p$layers), n-1)
    # w/o label
    p <- plotSD(x, label=NULL)
    expect_equal(length(p$layers), n-1)
    # w/o shape
    p <- plotSD(x, shape=NULL)
    expect_equal(length(p$layers), n-1)
})
