xy <- c("x", "y")
require(sf, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

# centroids ----

test_that("centroids,invalid", {
    expect_error(centroids(x), "supported")
    expect_error(centroids(image(x)), "supported")
})
test_that("centroids,sdLabel", {
    y <- label(x)
    z <- centroids(y, "data.frame")
    expect_is(z, "data.frame")
    expect_identical(names(z), c(xy, "i"))
    expect_is(z$i, "factor")
    expect_is(unlist(z[xy]), "numeric")
    .z <- centroids(y, "matrix")
    expect_is(.z, "matrix")
    z$i <- as.integer(as.character(z$i))
    expect_identical(.z, as.matrix(z))
})
test_that("centroids,sdPoint", {
    i <- feature_key(y <- point(x))
    z <- centroids(y, "data.frame")
    expect_is(z, "data.frame")
    expect_identical(names(z), c(xy, i))
    expect_is(z[[i]], "character")
    expect_is(unlist(z[xy]), "numeric")
    .z <- centroids(y, "list")
    expect_is(.z, "list")
    expect_all_true(names(.z) %in% z[[i]])
    expect_length(.z, length(unique(z[[i]])))
    for (. in names(.z)) expect_identical(
        .z[[.]][xy], z[z[[i]] == ., xy])
})
test_that("centroids,sdShape", {
    # circle
    y <- shape(x)
    z <- centroids(y, "data.frame")
    expect_is(z, "data.frame")
    expect_identical(names(z), xy)
    expect_is(unlist(z), "numeric")
    .z <- centroids(y, "matrix")
    expect_identical(.z, as.matrix(z))
    # polygon
    y <- shape(x, 3)
    z <- centroids(y, "data.frame")
    expect_is(z, "data.frame")
    expect_identical(names(z), xy)
    expect_is(unlist(z), "numeric")
    .z <- centroids(y, "matrix")
    expect_is(.z, "matrix")
    expect_identical(.z[, xy], as.matrix(z[xy]))
    # multipolygon
    y <- shape(x, 2)
    z <- centroids(y, "data.frame")
    expect_is(z, "data.frame")
    expect_identical(names(z), xy)
    expect_is(unlist(z), "numeric")
    .z <- centroids(y, "matrix")
    expect_is(.z, "matrix")
    expect_identical(.z, as.matrix(z))
})

# extent ----

test_that("extent,sdImage", {
    z <- extent(y <- image(x)[,-1,-c(1,2)])
    expect_is(z, "list")
    expect_is(unlist(z), "numeric")
    expect_identical(names(z), c("x", "y"))
    expect_identical(z$x, c(0, dim(y)[3]))
    expect_identical(z$y, c(0, dim(y)[2]))
})
test_that("extent,sdLabel", {
    z <- extent(y <- label(x)[,-1,-c(1,2)])
    expect_is(z, "list")
    expect_is(unlist(z), "numeric")
    expect_identical(names(z), c("x", "y"))
    expect_identical(z$y, c(0, dim(y)[1]))
    expect_identical(z$x, c(0, dim(y)[2]))
})
test_that("extent,sdPoint", {
    z <- extent(y <- point(x))
    expect_is(z, "list")
    expect_identical(names(z), xy)
    expect_is(unlist(z), "numeric")
    xy <- st_coordinates(st_as_sf(data(y)))
    expect_identical(z$x, range(xy[, 1]))
    expect_identical(z$y, range(xy[, 2]))
})
test_that("extent,sdShape", {
    z <- extent(y <- shape(x))
    expect_is(z, "list")
    expect_identical(names(z), xy)
    expect_is(unlist(z), "numeric")
    mx <- st_coordinates(st_as_sf(data(y)))
    expect_identical(z$x, range(mx[, 1]))
    expect_identical(z$y, range(mx[, 2]))
})
test_that("extent,SpatialData", {
    # single element
    y <- x["images",1]
    expect_identical(extent(y), extent(image(y,1)))
    expect_identical(extent(y)$x, c(0, dim(image(y,1))[3]))
    expect_identical(extent(y)$y, c(0, dim(image(y,1))[2]))
    
    # two elements w/ different extents
    y <- x[c("images","points"),list(1,1)]
    a <- extent(image(y)); b <- extent(point(y))
    ab <- rbind(data.frame(a), data.frame(b))
    ab <- list(x=range(ab[,1]), y=range(ab[,2]))
    expect_identical(extent(y), ab)
})
test_that("extent w/ transform", {
    # array
    y <- image(x)
    t <- c(1,0.7,7)
    z <- scale(y, t)
    wh <- list(
        x=extent(y)[[1]]*t[3],
        y=extent(y)[[2]]*t[2])
    expect_identical(extent(z), wh)
    # frame
    y <- point(x)
    t <- c(0.3,3)
    z <- scale(y, t)
    wh <- list(
        x=extent(y)[[1]]*t[1],
        y=extent(y)[[2]]*t[2])
    expect_identical(extent(z), wh)
})
