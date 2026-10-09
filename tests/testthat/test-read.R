x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")

test_that("readElement()", {
    typ <- c(
        images="SpatialDataImage", 
        labels="SpatialDataLabel", 
        points="SpatialDataPoint",
        shapes="SpatialDataShape", 
        tables="SingleCellExperiment")
    for (l in names(typ)) {
        f <- paste0(toupper(substr(l, 1, 1)), substr(l, 2, nchar(l)-1))
        y <- list.files(file.path(x, l), full.names=TRUE)[1]
        expect_s4_class(get(paste0("read", f))(y), typ[l])
    }
})

test_that("readSpatialData()", {
    expect_s4_class(y <- readSpatialData(x), "SpatialData")
    a <- list(images=TRUE, labels=TRUE, shapes=TRUE, points=TRUE, tables=FALSE)
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

test_that("read from S3", {
    x <- readSpatialData(
        "https://s3.embl.de/csama2026/BC_xenium_sdata_AL_LR.zarr/", tables = FALSE
    )
    expect_s4_class(x, "SpatialData")
    expect_length(images(x), 2)
    expect_length(labels(x), 0)
    expect_length(points(x), 1)
    expect_length(shapes(x), 2)
    expect_length(tables(x), 0)

    # Reading element works and returns the same
    img <- readImage(
        "https://s3.embl.de/csama2026/BC_xenium_sdata_AL_LR.zarr/images/he_image"
    )
    expect_identical(img, images(x)[[1]])
})