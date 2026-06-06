require(sf, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

test_that("crop,SpatialData", {
    # all-inclusive crop
    y <- list(xmin=-100, xmax=100, ymin=-100, ymax=100)
    expect_equivalent(crop(x, y), x)
    # crop around single point
    xy <- st_coordinates(st_as_sf(data(point(x)[1])))
    bb <- list(
        xmin=xy[1]-1e-3, xmax=xy[1]+1e-3, 
        ymin=xy[2]-1e-3, ymax=xy[2]+1e-3)
    y <- crop(x, bb)
    expect_length(point(y), 1)
    expect_length(shapes(y), 0)
    expect_length(tables(y), 1)
    expect_all_true(c(vapply(labels(y), dim, integer(2))) == 2)
    expect_all_true(c(vapply(images(y), \(.) dim(.)[-1], integer(2))) == 2)
})

test_that("crop,.check_box", {
    # valid
    q <- list(
        list(xmin=0, xmax=1, ymin=0, ymax=1),
        list(xmin=-1, xmax=0, ymin=-1, ymax=0),
        list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
    for (. in q) expect_silent(.check_box(.))
    # invalid
    q <- list(
        list(xmin=0, xmax=1, ymin=0),
        list(xmin=1, xmax=0, ymin=1, ymax=0),
        list(xmin=0, xmax=-1, ymin=0, ymax=-1),
        list(xmin=0, xmax=1, ymin=10, ymax=NA),
        list(xmin=Inf, xmax=-Inf, ymin=Inf, ymax=-Inf))
    for (. in q) expect_error(.check_box(.))
})

test_that("crop,.check_pol", {
    # valid
    q <- list(
        m <- matrix(seq_len(8), 4, 2),
        matrix(seq_len(2), 1, 2), # 1 row
        matrix(seq_len(4), 2, 2), # 2 rows
        rbind(c(1,1), c(2,2), c(3,3)), # open
        rbind(c(1,1), c(2,2), c(3,3), c(1,1)))
    for (. in q) expect_silent(.check_pol(.))
    # invalid
    q <- list(
        matrix(seq_len(6), 2, 3), # wrong dim.
        `[<-`(m, i=1, j=1, value=Inf), # not finite
        `[<-`(m, i=1, j=1, value=NA))  # missing value
    for (. in q) expect_error(.check_pol(.))
})

test_that("crop,sdImage", {
    d <- dim(i <- image(x))
    # polygon crop (should use bounding box)
    y <- matrix(c(10, 10, 20, 10, 20, 20, 10, 20), ncol=2, byrow=TRUE)
    expect_silent(z <- crop(i, y))
    expect_equal(dim(z), c(3, 10, 10))
    # bbox crop
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

test_that("crop,sdImage w/ previous translation", {
    y <- list(xmin=7, xmax=8, ymin=77, ymax=78)
    i <- translation(image(x), c(0, 77, 7))
    j <- crop(i, y)
    expect_equal(dim(j), c(3,1,1))
    expect_identical(data(i)[,1,1], data(j)[,1,1])
})

test_that("crop,sdLabel", {
    d <- dim(l <- label(x))
    # crop but don't shift
    y <- list(xmin=0, xmax=w <- d[1]/2, ymin=0, ymax=h <- d[2]/4)
    expect_equal(dim(m <- crop(l, y)), c(h, w))
})

test_that("crop input 'y' .to_sf()", {
    ok <- \(x) {
        expect_is(x, "sf")
        expect_identical(names(x), "geometry")
        expect_no_error(SpatialDataShape(x))
        expect_equal(as.integer(st_bbox(x)), c(0,-1,2,1))
    }
    # from matrix
    m <- matrix(c(0,-1, 2,-1, 2,1, 0,1, 0,-1), ncol=2, byrow=TRUE)
    ok(.to_sf(m))
    # from 'sf(c)'
    y <- st_sfc(st_polygon(list(m)))
    ok(.to_sf(st_sf(y)))
    ok(.to_sf(y))
    # from 'bbox'
    y <- list(xmin=0, xmax=2, ymin=-1, ymax=1)
    ok(.to_sf(st_bbox(unlist(y))))
    ok(.to_sf(y))
})

test_that("crop-box,sdPoint", {
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

test_that("crop-pol,sdPoint", {
    n <- length(p <- point(x))
    f <- \(.) collect(data(.))
    # mock all-inclusive crop
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_identical(f(crop(p, xy)), f(p))
})

test_that("crop-box,sdShape", {
    n <- length(s <- shape(x))
    # mock crop without any effect
    t <- crop(s, list(xmin=-1e7, xmax=1e7, ymin=-1e7, ymax=1e7))
    expect_equal(nrow(data(t)), nrow(data(s)))
    # this should drop everything
    t <- crop(s, list(xmin=0, xmax=1e-3, ymin=0, ymax=1e-3))
    expect_equal(nrow(t), 0)
})

test_that("crop-pol,sdShape", {
    n <- length(s <- shape(x))
    # mock all-inclusive crop
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_equal(crop(s, xy), s, check.attributes = FALSE)
})

test_that("crop,sdShape w/ table", {
    # mock up table for another shape
    i <- shapeNames(x)[1]
    s <- shape(x, i)
    n <- length(s)
    t <- SingleCellExperiment(matrix(0,0,n))
    y <- setTable(x, i, t, name="x")
    # crop around single shape
    . <- sample(length(s), 1)
    xy <- centroids(s[.])
    xy <- as.numeric(xy)
    bb <- list(
        xmin=xy[1]-1e-3, xmax=xy[1]+1e-3, 
        ymin=xy[2]-1e-3, ymax=xy[2]+1e-3)
    # single-column table should remain
    z <- crop(y, bb)
    expect_length(shape(z), 1)
    expect_equal(dim(table(z, "x")), c(0,1))
    expect_equivalent(shape(z), shape(y)[.])
})

test_that(".box2rev works with real image and injected scale", {
    path <- system.file("extdata", "blobs.zarr", package="spatialdataR")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a scale transformation into global space
    # Axes are c, y, x. Scale by 1, 2, 3.
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "scale"
    m$multiscales[[1]]$coordinateTransformations[[1]]$scale <- c(1, 2, 3)
    meta(img) <- m
    
    y <- list(xmin=30, xmax=60, ymin=20, ymax=40)
    z <- .box2rev(img, y, j=1)
    
    # Expected: x/3, y/2
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 20)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 20)
})

test_that(".box2rev handles j as character", {
    path <- system.file("extdata", "blobs.zarr", package="spatialdataR")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a scale transformation into global space
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "scale"
    m$multiscales[[1]]$coordinateTransformations[[1]]$scale <- c(1, 2, 3)
    meta(img) <- m
    
    y <- list(xmin=30, xmax=60, ymin=20, ymax=40)
    z <- .box2rev(img, y, j="global")
    
    expect_equal(unname(z$xmin), 10)
})

test_that(".box2rev works with identity (default)", {
    path <- system.file("extdata", "blobs.zarr", package="spatialdataR")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    y <- list(xmin=10, xmax=50, ymin=10, ymax=50)
    z <- .box2rev(img, y, j=1)
    
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 50)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 50)
})

test_that(".box2rev handles sequence transformation", {
    path <- system.file("extdata", "blobs.zarr", package="spatialdataR")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a sequence: scale then translation
    # Scale: c=1, y=2, x=3
    # Translation: c=0, y=10, x=5
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "sequence"
    m$multiscales[[1]]$coordinateTransformations[[1]]$transformations <- list(
        list(type="scale", scale=c(1, 2, 3)),
        list(type="translation", translation=c(0, 10, 5))
    )
    meta(img) <- m
    
    # crop in global space
    # (x_array * 3) + 5 = x_global  => x_array = (x_global - 5) / 3
    # (y_array * 2) + 10 = y_global => y_array = (y_global - 10) / 2
    
    y <- list(xmin=35, xmax=65, ymin=30, ymax=50)
    z <- .box2rev(img, y, j=1)
    
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 20)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 20)
})
