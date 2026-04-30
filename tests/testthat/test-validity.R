require(dplyr, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="SpatialData")
sd <- readSpatialData(zs, tables=FALSE)

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

test_that("validity,ShapeFrame", {
    x <- shape(sd,1)
    data(x) <- select(data(x), -radius)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- filter(data(x), radius == 1e7)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- data(x) |> duckspatial::ddbs_drop_geometry()
})

test_that(".validateTables() works correctly", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    
    # Valid
    expect_length(SpatialData:::.validateTables(sd), 0)
    
    # Invalid: Not an SCE
    sd_bad <- sd
    tables(sd_bad)[[1]] <- data.frame(a=1)
    expect_match(SpatialData:::.validateTables(sd_bad), "not a 'SingleCellExperiment'")
    
    # Invalid: Missing metadata
    sd_bad <- sd
    t <- SpatialData::table(sd_bad)
    md <- int_metadata(t)
    md <- NULL
    int_metadata(t) <- md
    tables(sd_bad) <- list(table=t)
    # Invalid: Non-existent region
    sd_bad <- sd
    t <- SpatialData::table(sd_bad)
    md <- int_metadata(t)
    md <- "non_existent"
    int_metadata(t) <- md
    tables(sd_bad) <- list(table=t)
    expect_match(SpatialData:::.validateTables(sd_bad), "table region.s. not found in any layer", all=FALSE)
})

test_that("Zattrs validation helpers work", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    za <- meta(label(sd, 1))
    ms <- as.list(za)[[1]]
    
    # 1. Multiscales
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_multiscales(list(multiscales=list(ms)), msg), 0)
    
    bad_za <- list(multiscales=list(list()))
    msg <- SpatialData:::.validateZattrs_multiscales(bad_za, c())
    expect_true(any(grepl("missing", msg)))
    
    # 2. Axes
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_axes(ms, msg), 0)
    
    bad_ax <- ms
    bad_ax <- NULL
    msg <- SpatialData:::.validateZattrs_axes(bad_ax, c())
    expect_true(any(grepl("missing", msg)))
    
    # 3. Coordinate Transformations
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_coordTrans(ms, msg), 0)
    
    bad_ct <- ms
    bad_ct[[1]] <- NULL
    expect_match(SpatialData:::.validateZattrs_coordTrans(bad_ct, c()), "missing")
})

test_that("Zattrs validation works across element types and versions", {
    za_array <- Zattrs(type="array", ver="0.4")
    za_frame <- Zattrs(type="frame", ver="0.4")
    
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_multiscales(as.list(za_array), msg), 0)
    
    za_array_v3 <- Zattrs(type="array", ver="0.3")
    expect_true("ome" %in% names(as.list(za_array_v3)))
    
    expect_match(SpatialData:::.validateZattrs_multiscales(as.list(za_frame), c()), "missing")
    
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_axes(as.list(za_frame), msg), 0)
})

test_that("validity,PointFrame", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    x <- point(sd, 1)
    
    # Valid
    expect_silent(validObject(x))
    
    # Invalid: missing geometry
    df_bad <- as.data.frame(SpatialData::data(x)) |> select(-geometry)
    x_bad <- PointFrame(data=df_bad, meta=meta(x))
    
    # This should now fail validity
    expect_error(validObject(x_bad), "'PointFrame' missing 'geometry'")
})
