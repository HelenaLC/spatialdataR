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
    # i=invalid
    for (f in fun) {
        expect_error(get(f)(x, 0))
        expect_error(get(f)(x, "."))
        expect_error(get(f)(x, c(1,1)))
    }
})

test_that("set one", {
    # value=NULL
    for (f in fun) {
        n <- length(get(paste0(f, "s"))(x))
        x <- get(paste0(f, "<-"))(x, i=1, value=NULL)
        m <- length(get(paste0(f, "s"))(x))
        expect_true(m == (n-1))
    }
})
