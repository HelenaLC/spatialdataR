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
library(testthat)
library(SpatialData)
library(SingleCellExperiment)

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
    md <- int_metadata(t)$spatialdata_attrs
    md$region <- NULL
    
    # Update metadata correctly
    new_md <- int_metadata(t)
    new_md$spatialdata_attrs <- md
    int_metadata(t) <- new_md
    
    tables(sd_bad) <- list(table=t)
    # Invalid: Missing instance key in colData
    sd_bad <- sd
    t <- SpatialData::table(sd_bad)
    int_colData(t)$instance_id <- NULL
    tables(sd_bad) <- list(table=t)
    expect_match(SpatialData:::.validateTables(sd_bad), "missing 'instance_key' column in 'int_colData'")
    
    # Invalid: Non-existent region
    sd_bad <- sd
    t <- SpatialData::table(sd_bad)
    md <- int_metadata(t)$spatialdata_attrs
    md$region <- "non_existent"
    new_md <- int_metadata(t)
    new_md$spatialdata_attrs <- md
    int_metadata(t) <- new_md
    tables(sd_bad) <- list(table=t)
    expect_match(SpatialData:::.validateTables(sd_bad), "table region\\(s\\) not found in any layer: 'non_existent'", all=FALSE)
})
library(testthat)
library(SpatialData)

test_that("Zattrs validation helpers work", {
    # Valid metadata (blobs)
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    za <- meta(label(sd, 1))
    ms <- as.list(za)$multiscales[[1]]
    
    # 1. Multiscales
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_multiscales(list(multiscales=list(ms)), msg), 0)
    
    bad_za <- list(multiscales=list(list()))
    msg <- SpatialData:::.validateZattrs_multiscales(bad_za, c())
    expect_true(any(grepl("missing 'multiscales.axes'", msg)))
    expect_true(any(grepl("missing 'multiscales.datasets'", msg)))
    
    # 2. Axes
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_axes(ms, msg), 0)
    
    bad_ax <- ms
    bad_ax$axes <- NULL
    msg <- SpatialData:::.validateZattrs_axes(bad_ax, c())
    expect_true(any(grepl("missing or non-list 'axes'", msg)))
    expect_true(any(grepl("missing 'axes.name'", msg)))
    
    # 3. Coordinate Transformations
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_coordTrans(ms, msg), 0)
    
    bad_ct <- ms
    bad_ct$coordinateTransformations[[1]]$output <- NULL
    expect_match(SpatialData:::.validateZattrs_coordTrans(bad_ct, c()), "'coordTrans' 1 missing 'output'")
})
library(testthat)
library(SpatialData)

test_that("Zattrs validation works across element types and versions", {
    # 1. Setup mock Zattrs for different types
    # Array (Image/Label)
    za_array <- Zattrs(type="array", ver="0.4")
    # Frame (Point/Shape)
    za_frame <- Zattrs(type="frame", ver="0.4")
    
    # 2. Test Array (Image)
    # Multiscales validation
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_multiscales(as.list(za_array), msg), 0)
    
    # Version 0.3
    za_array_v3 <- Zattrs(type="array", ver="0.3")
    # This should be wrapped in "ome"
    expect_true("ome" %in% names(as.list(za_array_v3)))
    
    # 3. Test Frame (Point/Shape)
    # Frame Zattrs do not have multiscales, so validators should skip or handle gracefully
    # .validateZattrs_multiscales checks for multiscales, it should fail for frames
    expect_match(SpatialData:::.validateZattrs_multiscales(as.list(za_frame), c()), "missing 'multiscales'")
    
    # Validate axes for frame
    msg <- c()
    expect_length(SpatialData:::.validateZattrs_axes(as.list(za_frame), msg), 0)
})
