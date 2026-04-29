require(dplyr, quietly=TRUE)
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="SpatialData")
sd <- readSpatialData(zs, tables=FALSE)

test_that("validity,ImageArray", {
    # all resolutions should be numbers
    # (note: logical gets coerced to binary)
    expect_error(ImageArray(list(v <- character(1))))
    x <- image(sd,1); x@data[[1]][1,1,1] <- v; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]][1,1,1] <- v; expect_error(validObject(x))
    # there should be three dimensions (channels + spatial)
    expect_error(ImageArray(list(a <- array(numeric(1), c(1,1)))))
    x <- image(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

test_that("validity,LabelArray", {
    
    # all resolutions should be of type integer
    for (v in list(logical(1), character(1), numeric(1))) {
        expect_error(LabelArray(list(v)))
        x <- label(sd,1); x@data[[1]][1,1] <- v; expect_error(validObject(x))
        x <- label(sd,2); x@data[[2]][1,1] <- v; expect_error(validObject(x))
    }
    # there should be two dimensions
    expect_error(LabelArray(list(a <- array(integer(1), c(1,1,1)))))
    x <- label(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- label(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

# TODO: fix me
# test_that("validity,PointFrame", {
#     x <- point(sd,1)
#     expect_error(validObject(select(x, -x)))
#     expect_error(validObject(select(x, -y)))
#     expect_silent(validObject(select(x, -c(x, y))[0,]))
# })

test_that("validity,ShapeFrame", {
    x <- shape(sd,1)
    data(x) <- select(data(x), -radius)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- filter(data(x), radius == 1e7)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    # data(x) <- select(data(x), -geometry)
    data(x) <- data(x) |> duckspatial::ddbs_drop_geometry()
    # currently the validation method does not check anything
    # expect_error(validObject(x))
})
