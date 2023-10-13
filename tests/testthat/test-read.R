dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")

test_that("readZarr", {
    dir <- file.path(dir, "images", "blobs_image")
    zarr <- list.dirs(dir, recursive=FALSE)
    md <- Rarr::zarr_overview(zarr, as_data_frame=TRUE)
    # non-existent resolution
    expect_error(readArray(dir, resolution="9"))
    expect_error(readArray(file.path(dir, "9")))
    # w/ & w/o specifying resolution
    i <- readArray(dir, resolution="0")
    j <- readArray(file.path(dir, "0"))
    expect_identical(i, j)
    expect_s4_class(i, "ImageArray")
    expect_true(is.list(metadata(i)))
    expect_equal(dim(i), md$dim[[1]])
})

test_that("readShapes", {
    df <- readShapes(file.path(dir, "shapes", "blobs_polygons"))
    expect_s4_class(df, "DFrame")
    nms <- c("data", "index", "type")
    expect_equal(names(df), nms)
    expect_type(df$data, "list")
    expect_type(df$index, "integer")
    expect_type(df$type, "character")
})

test_that("readTable", {
    sce <- readTable(file.path(dir, "table", "table"))
    expect_s4_class(sce, "SingleCellExperiment")
})

test_that("readSpatialData", {
    sd <- readSpatialData(dir)
    expect_s4_class(table(sd), "SingleCellExperiment")
    expect_s4_class(image(sd), "ImageArray")
    expect_s4_class(label(sd), "LabelArray")
    expect_s4_class(shape(sd), "ShapeFrame")
    expect_s4_class(point(sd), "PointFrame")
    for (. in images(sd)) expect_s4_class(., "ImageArray")
    for (. in labels(sd)) expect_s4_class(., "LabelArray")
    for (. in shapes(sd)) expect_s4_class(., "ShapeFrame")
    for (. in points(sd)) expect_s4_class(., "PointFrame")
})
