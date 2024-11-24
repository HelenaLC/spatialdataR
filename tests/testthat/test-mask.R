library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, anndataR=FALSE)

test_that("mask(),ImageArray,LabelArray", {
    i <- "blobs_image"
    j <- "blobs_labels"
    x <- mask(x, i, j, fun=sum)
    expect_equivalent(
        assay(table(x, 1)),
        assay(table(x, 2)))
})

test_that("mask(),PointFrame,ShapeFrame", {
    i <- "blobs_points"
    j <- "blobs_circles"
    x <- mask(x, i, j)
    t <- getTable(x, j)
    md <- meta(point(x, i))
    md <- md$spatialdata_attrs
    fk <- md$feature_key
    nr <- length(unique(point(x, i)[[fk]]))
    nc <- nrow(shape(x, j))
    expect_equal(dim(t), c(nr, nc))
})