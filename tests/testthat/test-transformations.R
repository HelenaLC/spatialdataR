path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)
i <- image(x)
l <- label(x)
s <- shape(x)
p <- point(x)

test_that("coords", {
    md <- zattrs(i)
    df <- coords(i)
    expect_s3_class(df, "data.frame")
    expect_equal(nrow(df), nrow(md$multiscales))
})
test_that("coord", {
    expect_error(coord(i, 99))
    expect_error(coord(i, ""))
    nm <- coords(i)$output$name[1]
    expect_s3_class(coord(i, nm), "data.frame")
})

# translation ----
test_that("translate,t", {
    # should be 2 whole numbers
    expect_error(.translate(i, NULL))
    expect_error(.translate(i, c("", "")))
    expect_error(.translate(i, c(1.1, 1)))
    expect_error(.translate(i, numeric(0)))
    expect_error(.translate(i, numeric(3)))
})
test_that("translateElement,ImageArray", {
    y <- translateElement(i, c(0, 0))
    expect_identical(y, i)
})
test_that("translateElement,LabelArray", {
    y <- translateElement(l, c(0, 0))
    expect_identical(y, l)
})
test_that("translateElement,ShapeFrame", {
    y <- translateElement(s, c(0, 0))
    expect_identical(y, s)
})
test_that("translateElement,PointFrame", {
    y <- translateElement(p, c(0, 0))
    expect_identical(y, p)
    t <- c(t1 <- 1, t2 <- 2)
    y <- translateElement(p, t)
    expect_identical(dim(y), dim(p))
    expect_identical(range(y$x), range(p$x)+t1)
    expect_identical(range(y$y), range(p$y)+t2)
})

# rotation ----
test_that("rotate,t", {
    # should be any ONE number
    expect_error(.rotate(i, "bunny"))
    expect_error(.rotate(i, c(0, 0)))
    expect_silent(.rotate(i, -99))
    expect_silent(.rotate(i, 999))
})
test_that("rotateElement,ImageArray", {
    j <- rotateElement(i, 0)
    expect_identical(j, i)
    expect_equal(dim(rotateElement(i, 000)), dim(i))
    expect_equal(dim(rotateElement(i, 180)), dim(i))
    expect_equal(dim(rotateElement(i, 360)), dim(i))
    expect_equal(dim(rotateElement(i,  90)), dim(i)[c(1, 3, 2)])
    expect_equal(dim(rotateElement(i, 270)), dim(i)[c(1, 3, 2)])
})
test_that("rotateElement,ShapeFrame,polygon", {
    expect_identical(rotateElement(s, 000), s)
    expect_equal(rotateElement(s, 360), s)
    t <- rotateElement(s, 180)
    expect_identical(zattrs(t), zattrs(s))
    expect_identical(metadata(t), metadata(s))
    expect_equal(range(t$data), -rev(range(s$data)))
})
test_that("rotateElement,PointFrame", {
    df <- as.data.frame(p)
    q <- rotateElement(p, 000)
    expect_identical(as.data.frame(q), df)
    q <- rotateElement(p, 360)
    expect_equal(as.data.frame(q), df, ignore_attr=TRUE)
    q <- rotateElement(p, 180)
    expect_identical(zattrs(q), zattrs(p))
    expect_identical(metadata(q), metadata(p))
    expect_equal(range(q$x), -rev(range(p$x)))
    expect_equal(range(q$y), -rev(range(p$y)))
})

# scaling ----
test_that("scale,t", {
    # should be n numbers > 0
    # (n = number of channels)
    n <- length(channels(i))
    expect_error(.scale(i, numeric(n-1)))
    expect_error(.scale(i, numeric(n+1)))
    expect_error(.scale(i, rep(-1.0, n)))
    expect_silent(.scale(i, rep(1.0, n)))
    expect_silent(.scale(i, rep(1.1, n)))
})
test_that("scaleElement,ImageArray", {
    d <- length(dim(i))
    expect_s4_class(scaleElement(i), "ImageArray")
    expect_equal(dim(scaleElement(i, rep(1, d))), dim(i))
    expect_equal(dim(scaleElement(i, rep(2, d))), c(dim(i)[1], 2*dim(i)[-1]))
})
test_that("scaleElement,ShapeFrame,polygon", {
    # no scaling
    t <- scaleElement(s, c(1, 1))
    expect_s4_class(t, "ShapeFrame")
    expect_equal(dim(t), dim(s))
    expect_equal(range(t$data), range(s$data))
    expect_identical(metadata(t), metadata(s))
    # xy scaling
    t <- scaleElement(s, c(f <- 2, g <- 3))
    expect_equal(
        range(as.array(t)[, 1]),
        f*range(as.array(s)[, 1]))
    expect_equal(
        range(as.array(t)[, 2]),
        g*range(as.array(s)[, 2]))
})

# transformation ----
test_that("transformElement,ImageArray", {
    y <- transformElement(i)
    expect_s4_class(y, "ImageArray")
    expect_identical(metadata(i), metadata(y))
})
