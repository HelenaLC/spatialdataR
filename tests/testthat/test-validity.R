require(dplyr, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

test_that("SpatialData()", {
    # empty
    expect_silent(x <- SpatialData())
    expect_all_true(lengths(colnames(x)) == 0)
    for (l in .LAYERS) expect_is(get(l)(x), "SimpleList")
    # single layer
    e <- list(
        images=SpatialDataImage(),
        labels=SpatialDataLabel(),
        points=SpatialDataPoint(),
        shapes=SpatialDataShape(),
        tables=SingleCellExperiment())
    for (l in .LAYERS) {
        arg <- list(list(e[[l]])); names(arg) <- l
        expect_silent(x <- do.call("SpatialData", arg))
        expect_named(x[[l]])
        expect_length(x[[l]], 1)
        expect_is(x[[l]], "SimpleList")
        expect_identical(names(x[[l]]), gsub("s$", 1, l))
    }
})

test_that("validity,SpatialDataImage", {
    expect_error(SpatialDataImage(list(v <- character(1))))
    x <- image(sd,1); x@data[[1]][1,1,1] <- v; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]][1,1,1] <- v; expect_error(validObject(x))
    expect_error(SpatialDataImage(list(a <- array(numeric(1), c(1,1)))))
    x <- image(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- image(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

test_that("validity,SpatialDataLabel", {
    for (v in list(logical(1), character(1), numeric(1))) {
        expect_error(SpatialDataLabel(list(v)))
        x <- label(sd,1); x@data[[1]][1,1] <- v; expect_error(validObject(x))
        x <- label(sd,2); x@data[[2]][1,1] <- v; expect_error(validObject(x))
    }
    expect_error(SpatialDataLabel(list(a <- array(integer(1), c(1,1,1)))))
    x <- label(sd,1); x@data[[1]] <- a; expect_error(validObject(x))
    x <- label(sd,2); x@data[[2]] <- a; expect_error(validObject(x))
})

test_that("validity,sdPoint", {
    # valid
    x <- point(sd, 1)
    expect_true(validObject(x))
    # invalid
    df <- duckspatial::ddbs_drop_geometry(data(x))
    expect_error(SpatialDataPoint(df, meta(x)))
})

test_that("validity,sdShape", {
    # valid
    x <- shape(sd,1)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- select(data(x), -radius)
    expect_silent(validObject(x))
    x <- shape(sd,1)
    data(x) <- filter(data(x), radius == -1)
    expect_silent(validObject(x))
    # invalid: missing geometry
    x <- shape(sd,1)
    df <- duckspatial::ddbs_drop_geometry(data(x))
    expect_error(SpatialDataShape(df, meta(x)))
})

test_that("validity,sdTable", {
    # valid
    fn <- .validateTables
    expect_length(fn(sd), 0)
    # invalid: not a SCE
    x <- sd
    expect_error(tables(x)[[1]] <- matrix(1,2,3))
    
    # helper to update table's 'spatialdata_attrs'
    f <- \(x, i, j) {
        t <- x$tables[[1]]
        md <- int_metadata(t)
        md$spatialdata_attrs[[i]] <- j
        int_metadata(t) <- md
        `table<-`(x, value=t)
    }
    
    # missing/non-existent region
    expect_error(validObject(f(sd, "region", NULL)))
    expect_error(validObject(f(sd, "region", "x")))
    
    # invalid/multiple keys
    for (key in c("region_key", "instance_key")) {
        expect_error(validObject(f(sd, key, 1)), "character")
        expect_error(validObject(f(sd, key, "x")), "missing")
        expect_error(validObject(f(sd, key, c("a", "b"))), "length")
    }
})

test_that("validity,SpatialDataAttrs", {
    za <- meta(label(sd, 1))
    ms <- as.list(za)$multiscales[[1]]
    # multiscales
    fn <- .validateAttrs_multiscales
    expect_null(fn(list(), c()))
    expect_length(fn(as.list(za), c()), 0)
    # axes
    fn <- .validateAttrs_axes
    expect_length(fn(ms, c()), 0)
    bad_ax <- ms; bad_ax$axes <- NULL
    expect_match(fn(bad_ax, c()), "missing")
    # coordinate transformations
    fn <- .validateAttrs_coordTrans
    expect_length(fn(ms, c()), 0)
    bad_ct <- ms; bad_ct$coordinateTransformations <- NULL
    expect_match(fn(bad_ct, c()), "missing")
})
