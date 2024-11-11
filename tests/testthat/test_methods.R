x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

fun <- c("image", "label", "shape", "point", "table")
nms <- c("blobs_image", "blobs_labels", "blobs_circles", "blobs_points", "table")
typ <- c("ImageArray", "LabelArray", "ShapeFrame", "PointFrame", "SingleCellExperiment")

test_that("get all", {
    for (f in paste0(fun, "s"))
        expect_is(get(f)(x), "list")
})

test_that("get one", {
    # i=numeric
    mapply(f=fun, t=typ, \(f, t) 
        expect_is(get(f)(x, i=1), t))
    # i=character
    mapply(f=fun, t=typ, n=nms, \(f, t, n) 
        expect_is(get(f)(x, n), t))
})
