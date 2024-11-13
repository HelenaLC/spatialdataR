x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")

test_that("readElement()", {
    typ <- c(
        images="ImageArray", 
        labels="LabelArray", 
        points="PointFrame",
        shapes="ShapeFrame", 
        tables="SingleCellExperiment")
    for (l in names(typ)) {
        f <- paste0(toupper(substr(l, 1, 1)), substr(l, 2, nchar(l)-1))
        y <- list.files(file.path(x, l), full.names=TRUE)[1]
        expect_is(get(paste0("read", f))(y), typ[l])
    }
})

test_that("readSpatialData()", {
    expect_is(readSpatialData(x), "SpatialData")
    a <- list(x=x, images=NULL, labels=NULL, shapes=NULL, points=NULL, tables=NULL)
    # setting any layer to FALSE should skip it
    for (. in names(args)) {
        b <- a; b[[.]] <- FALSE
        obj <- do.call(readSpatialData, b)
        expect_length(get(.)(obj), 0)
    }
})