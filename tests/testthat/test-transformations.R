path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)
i <- image(x)
l <- label(x)
s <- shape(x)

test_that("coords", {
    df <- coords(i)
    md <- metadata(i)
    expect_s4_class(df, "DFrame")
    expect_equal(nrow(df), nrow(md$multiscales))
})
test_that("coords", {
    expect_error(coord(i, 99))
    expect_error(coord(i, ""))
    nm <- coords(i)$output.name[1]
    expect_s4_class(coord(i, nm), "DFrame")
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
    y <- translateElement(i)
    expect_s4_class(y, "ImageArray")
    expect_equal(dim(i), dim(y))
})
test_that("translateElement,LabelArray", {
    y <- translateElement(l)
    expect_s4_class(y, "LabelArray")
    expect_equal(dim(l), dim(y))
})
test_that("translateElement,ShapeFrame", {
    y <- translateElement(s)
    expect_s4_class(y, "ShapeFrame")
    expect_equal(dim(s), dim(y))
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
    expect_equal(rotateElement(s, 000), s)
    expect_equal(rotateElement(s, 360), s)
    t <- rotateElement(s, 180)
    expect_identical(metadata(t), metadata(s))
    expect_equal(range(t$data), -rev(range(s$data)))
})

# scaling ----
test_that("scale,t", {
    # should be n numbers > 0
    # (n = number of channels)
    n <- length(channels(i))
    expect_error(.scale(i, rep(1, n-1)))
    expect_error(.scale(i, rep(1, n+1)))
    expect_error(.scale(i, rep(-1, n)))
    expect_silent(.scale(i, rep(1, n)))
})
test_that("scaleElement,ImageArray", {
    d <- length(dim(i))
    expect_s4_class(scaleElement(i), "ImageArray")
    expect_equal(dim(scaleElement(i, rep(1, d))), dim(i))
    expect_equal(dim(scaleElement(i, rep(2, d))), c(dim(i)[1], 2*dim(i)[-1]))
})
test_that("scaleElement,ShapeFrame,polygon", {
    t <- scaleElement(s, c(1, 1))
    expect_s4_class(t, "ShapeFrame")
    expect_equal(dim(t), dim(s))
    expect_equal(range(t$data), range(s$data))
    expect_identical(metadata(t), metadata(s))
})

# transformation ----
test_that("transformElement,ImageArray", {
    y <- transformElement(i)
    expect_s4_class(y, "ImageArray")
    expect_identical(metadata(i), metadata(y))
})
