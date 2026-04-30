require(dplyr, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="SpatialData")
sd <- readSpatialData(zs)

test_that("validity,ImageArray", {
    expect_error(ImageArray(list(v <- character(1))))
    x <- image(sd,1); x@data[[1]][1,1,1] <- v; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]][1,1,1] <- v; expect_error(validObject(x))
    expect_error(ImageArray(list(a <- array(numeric(1), c(1,1)))))
    x <- image(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

test_that("validity,LabelArray", {
    for (v in list(logical(1), character(1), numeric(1))) {
        expect_error(LabelArray(list(v)))
        x <- label(sd,1); x@data[[1]][1,1] <- v; expect_error(validObject(x))
        x <- label(sd,2); x@data[[2]][1,1] <- v; expect_error(validObject(x))
    }
    expect_error(LabelArray(list(a <- array(integer(1), c(1,1,1)))))
    x <- label(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- label(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

test_that("validity,PointFrame", {
    # valid
    x <- point(sd, 1)
    expect_true(validObject(x))
    # invalid
    df <- duckspatial::ddbs_drop_geometry(data(x))
    expect_error(PointFrame(df, meta(x)))
})

test_that("validity,ShapeFrame", {
    # valid
    x <- shape(sd,1)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- select(data(x), -radius)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- filter(data(x), radius == -1)
    expect_silent(validObject(x))
    # invalid: missing geometry
    x <- shape(sd,1)
    df <- duckspatial::ddbs_drop_geometry(data(x))
    expect_error(ShapeFrame(df, meta(x)))
})

test_that("validity,SCE", {
    # valid
    fn <- SpatialData:::.validateTables
    expect_length(fn(sd), 0)
    # invalid: not a SCE
    x <- sd
    tables(x)[[1]] <- data.frame()
    expect_error(validObject(x))
    # invalid: missing region
    x <- sd
    t <- SpatialData::table(x)
    md <- int_metadata(t)
    md$spatialdata_attrs$region <- NULL
    int_metadata(t) <- md
    tables(x) <- list(table=t)
    expect_error(validObject(x))
    # invalid: non-existent region
    x <- sd
    t <- SpatialData::table(x)
    md <- int_metadata(t)
    md$spatialdata_attrs$region <- "x"
    int_metadata(t) <- md
    tables(x) <- list(table=t)
    expect_error(validObject(x))
})

test_that("validity,Zattrs", {
    za <- meta(label(sd, 1))
    ms <- as.list(za)$multiscales[[1]]
    # multiscales
    fn <- SpatialData:::.validateZattrs_multiscales
    expect_length(fn(as.list(za), c()), 0)
    expect_match(fn(list(), c()), "missing")
    # axes
    fn <- SpatialData:::.validateZattrs_axes
    expect_length(fn(ms, c()), 0)
    bad_ax <- ms; bad_ax$axes <- NULL
    expect_match(fn(bad_ax, c()), "missing")
    # coordinate transformations
    fn <- SpatialData:::.validateZattrs_coordTrans
    expect_length(fn(ms, c()), 0)
    bad_ct <- ms; bad_ct$coordinateTransformations <- NULL
    expect_match(fn(bad_ct, c()), "missing")
})
