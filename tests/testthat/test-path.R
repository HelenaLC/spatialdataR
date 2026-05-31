zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

test_that("path,image", {
    x <- path(image(sd))
    expect_length(x, 1)
    expect_is(x, "character")
    expect_true(file.info(x)$isdir)
    
    x <- path(SpatialDataImage())
    expect_length(x, 1)
    expect_true(is.na(x))
    expect_is(x, "character")
})

test_that("path,label", {
    x <- path(label(sd))
    expect_length(x, 1)
    expect_is(x, "character")
    expect_true(file.info(x)$isdir)
    
    x <- path(SpatialDataLabel())
    expect_length(x, 1)
    expect_true(is.na(x))
    expect_is(x, "character")
})

test_that("path,shape", {
    x <- path(shape(sd))
    expect_length(x, 1)
    expect_is(x, "character")
    expect_true(file.info(x)$isdir)
    expect_true(endsWith(x, ".parquet"))
    
    x <- path(SpatialDataShape())
    expect_length(x, 1)
    expect_true(is.na(x))
    expect_is(x, "character")
})

test_that("path,point", {
    x <- path(point(sd))
    expect_length(x, 1)
    expect_is(x, "character")
    expect_true(file.info(x)$isdir)
    expect_true(endsWith(x, ".parquet"))
    
    x <- path(SpatialDataPoint())
    expect_length(x, 1)
    expect_true(is.na(x))
    expect_is(x, "character")
})

test_that("path,table", {
    x <- path(table(sd))
    expect_length(x, 1)
    expect_is(x, "character")
    expect_true(file.info(x)$isdir)
    
    x <- path(SpatialDataPoint())
    expect_length(x, 1)
    expect_true(is.na(x))
    expect_is(x, "character")
})

test_that("path,sdata", {
    ls <- rownames(sd)
    es <- unlist(colnames(sd))
    ne <- length(es)
    
    x <- path(sd, simplify=TRUE)
    expect_is(x, "data.frame")
    expect_equal(ncol(x), 3)
    expect_equal(nrow(x), ne)
    for (. in seq_along(x))
        expect_is(x[[.]], "character")
    expect_all_true(x[[1]] %in% ls)
    expect_all_true(x[[2]] %in% es)
    expect_all_true(file.exists(x[[3]]))
    
    y <- sd
    label(y, 2) <- SpatialDataLabel()
    y <- path(y, simplify=TRUE)
    i <- y[[1]] == "labels" & y[[2]] == labelNames(sd)[2]
    expect_identical(i, is.na(y[[3]]))
    y[[3]][i] <- x[[3]][i]
    expect_identical(x, y)
    
    x <- path(sd, simplify=FALSE)
    expect_is(x, "list")
    expect_length(unlist(x), ne)
    expect_equal(names(x), ls)
    for (l in names(x)) {
        expect_length(x[[l]], length(sd[[l]]))
        expect_equal(names(x[[l]]), names(sd[[l]]))
        for (e in names(x[[l]])) {
            y <- x[[l]][[e]]
            expect_length(y, 1)
            expect_is(y, "character")
            expect_true(file.exists(y))
            expect_equal(y, path(sd[[l]][[e]]))
        }
    }
})
