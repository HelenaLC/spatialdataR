z <- list(v1="blobs.zarr", v3="blobs_v3.zarr")

for (v in names(z)) {

    x <- file.path("extdata", z[[v]])
    x <- system.file(x, package="spatialdataR")
    x <- readSpatialData(x)

    test_that(paste0(v, "-multiscales"), {
        y <- meta(image(x))
        z <- multiscales(y)
        expect_is(z, "list")
        expect_length(z, 1)
    })

    test_that(paste0(v, "-axes"), {
        # image
        y <- axes(image(x))
        expect_is(y, "list")
        expect_length(y, 3)
        # label
        y <- axes(label(x))
        expect_is(y, "list")
        expect_length(y, 2)
        # shape
        y <- axes(shape(x))
        expect_is(y, "list")
        expect_length(y, 2)
        expect_equal(unlist(y), c("x", "y"))
        # point
        y <- axes(point(x))
        expect_is(y, "list")
        expect_length(y, 2)
        expect_equal(unlist(y), c("x", "y"))
        # missing
        y <- image(x)
        switch(v,
            "v3"=y@meta$ome$multiscales[[1]]$axes <- NULL,
            y@meta$multiscales[[1]]$axes <- NULL)
        expect_error(axes(y))
    })

    test_that(paste0(v, "-channels"), {
        expect_error(channels(label(x)))
        expect_silent(z <- channels(y <- image(x)))
        expect_length(z, dim(y)[1])
    })
}

test_that(".val_ome_ver()", {
    # invalid
    expect_error(.val_ome_ver(1))
    expect_error(.val_ome_ver(TRUE))
    expect_error(.val_ome_ver("0.0"))
    expect_error(.val_ome_ver("0.30"))
    expect_error(.val_ome_ver(c("0.3", "0.4")))
    # valid
    expect_silent(.val_ome_ver(v <- "0.3-x"))
    expect_silent(x <- .val_ome_ver(v <- "0.3"))
    expect_is(x, "character")
    expect_length(x, 1)
    expect_identical(x, v)
})
test_that("SpatialDataAttrs()", {
    # invalid
    expect_error(SpatialDataAttrs(nch=0))
    expect_error(SpatialDataAttrs(dim=7))
    expect_error(SpatialDataAttrs(ver="0.0"))
    expect_error(SpatialDataAttrs(type="bad"))
    # 2-4D image
    nms <- c("c", "t", "z", "y", "x")
    for (d in seq(2, 4)) {
        x <- SpatialDataAttrs(type="image", dim=d, nch=7)
        ok <- if (d == 2) nms[-c(2,3)] else if (d == 3) nms[-2] else nms
        # axes name
        y <- axes(x, "name")
        expect_length(y, 1+d)
        expect_is(y, "character")
        expect_identical(y, ok)
        # axes type
        y <- axes(x, "type")
        expect_equal(sum(y == "time"), ifelse(d == 4, 1, 0))
        expect_equal(sum(y == "space"), ifelse(d == 2, 2, 3))
        expect_equal(sum(y == "channel"), 1)
        # channels
        y <- channels(x)
        expect_length(y, 7)
        expect_is(y, "character")
        expect_all_true(!duplicated(y))
    }
    # 2-4D label
    for (d in seq(2, 4)) {
        x <- SpatialDataAttrs(type="label", dim=d)
        y <- axes(x, "type")
        expect_length(y, d)
        expect_equal(sum(y == "time"), ifelse(d == 4, 1, 0))
        expect_equal(sum(y == "space"), ifelse(d == 2, 2, 3))
    }
    # 3-4D shape/point
    for (d in seq(2, 4)) {
        x <- SpatialDataAttrs(type="frame", dim=d)
        y <- axes(x, "type")
        expect_length(y, d)
        expect_null(channels(x))
        expect_equal(sum(y == "time"), ifelse(d == 4, 1, 0))
        expect_equal(sum(y == "space"), ifelse(d == 2, 2, 3))
    }
})
