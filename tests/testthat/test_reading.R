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
    expect_is(y <- readSpatialData(x), "SpatialData")
    a <- list(images=TRUE, labels=TRUE, shapes=TRUE, points=TRUE, tables=TRUE)
    for (. in names(a)) {
        # setting any layer to FALSE skips it
        b <- c(list(x=x), a); b[[.]] <- FALSE
        obj <- do.call(readSpatialData, b)
        expect_length(get(.)(obj), 0)
        # specifying non-existent element fails
        b <- c(list(x=x), a); b[[.]] <- 100
        expect_error(do.call(readSpatialData, b))
        b <- c(list(x=x), a); b[[.]] <- "."
        expect_error(do.call(readSpatialData, b))
        # specifying element name works
        f <- paste0(substr(., 1, 1), substr(., 2, nchar(.)-1), "Names")
        b <- c(list(x=x), a); b[[.]] <- get(f)(y)[1]
        expect_silent(do.call(readSpatialData, b))
    }
})

test_that(".readTable_anndataR/basilisk()", {
    a <- readSpatialData(x, anndataR=TRUE)
    b <- readSpatialData(x, anndataR=FALSE)
    expect_equivalent(
        SpatialData::table(a), 
        SpatialData::table(b))
})
