require(sf, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

test_that("crop,SpatialData", {
    y <- list(xmin=10, xmax=50, ymin=10, ymax=50)
    expect_no_message(z <- crop(x, y, "global"))
    expect_is(z, "SpatialData")
    # check that elements were cropped
    expect_true(nrow(point(z)) < nrow(point(x)))
})

test_that("query,.check_box", {
    # valid
    q <- list(
        list(xmin=0, xmax=1, ymin=0, ymax=1),
        list(xmin=-1, xmax=0, ymin=-1, ymax=0),
        list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
    for (. in q) expect_silent(SpatialData:::.check_box(.))
    # invalid
    q <- list(
        list(xmin=0, xmax=1, ymin=0),
        list(xmin=1, xmax=0, ymin=1, ymax=0),
        list(xmin=0, xmax=-1, ymin=0, ymax=-1),
        list(xmin=0, xmax=1, ymin=10, ymax=NA),
        list(xmin=Inf, xmax=-Inf, ymin=Inf, ymax=-Inf))
    for (. in q) expect_error(SpatialData:::.check_box(.))
})

test_that("query,.check_pol", {
    # valid
    q <- list(
        m <- matrix(seq_len(8), 4, 2),
        matrix(seq_len(2), 1, 2), # 1 row
        matrix(seq_len(4), 2, 2), # 2 rows
        rbind(c(1,1), c(2,2), c(3,3)), # open
        rbind(c(1,1), c(2,2), c(3,3), c(1,1)))
    for (. in q) expect_silent(SpatialData:::.check_pol(.))
    # invalid
    q <- list(
        matrix(seq_len(6), 2, 3), # wrong dim.
        `[<-`(m, i=1, j=1, value=Inf), # not finite
        `[<-`(m, i=1, j=1, value=NA))  # missing value
    for (. in q) expect_error(SpatialData:::.check_pol(.))
})

test_that("crop,ImageArray", {
    d <- dim(i <- image(x))
    # polygon query (should use bounding box)
    y <- matrix(c(10, 10, 20, 10, 20, 20, 10, 20), ncol=2, byrow=TRUE)
    expect_silent(z <- crop(i, y))
    expect_equal(dim(z), c(3, 10, 10))
    # bbox query
    y <- st_bbox(c(xmin=10, ymin=10, xmax=20, ymax=20))
    expect_silent(z <- crop(i, y))
    expect_equal(dim(z), c(3, 10, 10))
    y <- list(xmin=0, xmax=d[3], ymin=0, ymax=d[2])
    # allow for metadata difference in 'wh'
    expect_equal(dim(crop(i, y)), dim(i))
    # crop and shift
    y <- list(
        xmin=dx <- 10, xmax=w <- 40,
        ymin=dy <- 10, ymax=h <- 40)
    expect_equal(dim(j <- crop(i, y)), c(3, 30, 30))
    expect_equal(metadata(j)$wh, list(c(10, 40), c(10, 40)))
})

test_that("crop,LabelArray", {
    d <- dim(l <- label(x))
    # crop but don't shift
    y <- list(xmin=0, xmax=w <- d[1]/2, ymin=0, ymax=h <- d[2]/4)
    expect_equal(dim(m <- crop(l, y)), c(h, w))
})

test_that("crop-box,PointFrame", {
    n <- length(p <- point(x))
    # this shouldn't do anything
    q <- crop(p, list(xmin=-1e7, xmax=1e7, ymin=-1e7, ymax=1e7))
    expect_is(data(q), "duckspatial_df")
    expect_identical(collect(data(p)), collect(data(q)))
    # this should drop everything
    q <- crop(p, list(xmin=0, xmax=1e-3, ymin=0, ymax=1e-3))
    expect_equal(nrow(collect(data(q))), 0)
    # st_bbox
    y <- st_bbox(c(xmin=10, xmax=50, ymin=10, ymax=50))
    expect_silent(z <- crop(p, y))
    expect_true(nrow(z) < nrow(p))
    # st_polygon
    y <- c(10,10, 50,10, 50,50, 10,50, 10,10)
    y <- st_polygon(list(matrix(y, ncol=2, byrow=TRUE)))
    expect_silent(z <- crop(p, y))
    expect_true(nrow(z) < nrow(p))
})

test_that("crop-pol,PointFrame", {
    n <- length(p <- point(x))
    f <- \(.) collect(data(.))
    # mock all-inclusive query
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_identical(f(crop(p, xy)), f(p))
})

test_that("crop-box,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock query without any effect
    t <- crop(s, list(xmin=-1e7, xmax=1e7, ymin=-1e7, ymax=1e7))
    expect_equal(nrow(data(t)), nrow(data(s)))
    # this should drop everything
    t <- crop(s, list(xmin=0, xmax=1e-3, ymin=0, ymax=1e-3))
    expect_equal(nrow(t), 0)
})

test_that("crop-pol,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock all-inclusive query
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_equal(crop(s, xy), s, check.attributes = FALSE)
})
