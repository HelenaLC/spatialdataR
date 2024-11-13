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
    a <- list(images=TRUE, labels=TRUE, shapes=TRUE, points=TRUE, tables=TRUE)
    for (. in names(a)) {
        # setting any layer to FALSE should skip it
        b <- c(list(x=x), a); b[[.]] <- FALSE
        obj <- do.call(readSpatialData, b)
        expect_length(get(.)(obj), 0)
        # setting layer to out of bounds index fails
        b <- c(list(x=x), a); b[[.]] <- 100
        expect_error(do.call(readSpatialData, b))
    }
})
