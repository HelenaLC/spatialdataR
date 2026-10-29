# testing for both SpatialData container versions 0.1 and 0.2
z <- list(v01="blobs.zarr", v02="blobs_v3.zarr")
zv <- list(
    v01 = list(
        image="0.1", label="0.1", point="0.1", shape="0.2", table="0.1"
    ),
    v02 = list(
        image="0.3", label="0.3", point="0.2", shape="0.3", table="0.2"
    )
)

for (v in names(z)) {

    x <- file.path("extdata", z[[v]])
    x <- system.file(x, package="spatialdataR")
    x <- readSpatialData(x)

    test_that(paste0(v, "-multiscales"), {
        y <- meta(image(x))
        z <- multiscales(y)
        expect_type(z, "list")
        expect_length(z, 1)
    })

    test_that(paste0(v, "-axes"), {
        # image
        y <- axes(image(x))
        expect_type(y, "list")
        expect_length(y, 3)
        # label
        y <- axes(label(x))
        expect_type(y, "list")
        expect_length(y, 2)
        # shape
        y <- axes(shape(x))
        expect_type(y, "list")
        expect_length(y, 2)
        expect_equal(unlist(y), c("x", "y"))
        # point
        y <- axes(point(x))
        expect_type(y, "list")
        expect_length(y, 2)
        expect_equal(unlist(y), c("x", "y"))
        # missing
        y <- image(x)
        switch(v,
            "v02"=y@meta$ome$multiscales[[1]]$axes <- NULL,
            y@meta$multiscales[[1]]$axes <- NULL)
        expect_error(axes(y))
    })

    test_that(paste0(v, "-channels"), {
        expect_error(channels(label(x)))
        expect_silent(z <- channels(y <- image(x)))
        expect_length(z, dim(y)[1])
    })
    
    test_that("version()", {
        vx <- version(x)
        expect_s3_class(vx, c("tbl_df", "tbl", "data.frame"))
        expect_equal(colnames(vx), 
                     c("layer", "element", "version"))
        expect_all_true(vx$layer %in% paste0(names(zv[[v]]), "s"))
        for (el in names(zv[[v]])[1:4]) {
            f <- match.fun(el)
            expect_equal(version(f(x)), zv[[v]][[el]])
        }
        # match.fun matches with base::table
        expect_equal(version(table(x)), zv[[v]][["table"]])
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
    expect_type(x, "character")
    expect_length(x, 1)
    expect_identical(x, v)
})
test_that(".val_sd_ver()", {
    # invalid
    expect_error(.val_sd_ver(0.3), 'argument "type" is missing')
    expect_error(.val_sd_ver(1, "image"))
    expect_error(.val_sd_ver(TRUE, "image"))
    expect_error(.val_sd_ver("0.0", "image"))
    expect_error(.val_sd_ver("0.30", "image"))
    expect_error(.val_sd_ver(c("0.3", "0.4"), "image"))
    expect_error(.val_sd_ver("0.3", "point"))
    expect_error(.val_sd_ver("0.3", "table"))
    # valid
    expect_silent(x <- .val_sd_ver(v <- "0.3", "image"))
    expect_silent(x <- .val_sd_ver(v <- "0.3", "label"))
    expect_silent(x <- .val_sd_ver(v <- "0.3", "shape"))
    expect_silent(x <- .val_sd_ver(v <- "0.2", "point"))
    expect_silent(x <- .val_sd_ver(v <- "0.2", "table"))
    expect_type(x, "character")
    expect_length(x, 1)
    expect_identical(x, v)
})

test_that("SpatialDataAttrs()", {
    # invalid
    expect_error(SpatialDataAttrs(nch=0))
    expect_error(SpatialDataAttrs(dim=7))
    expect_error(SpatialDataAttrs(ver="0.0"))
    expect_error(SpatialDataAttrs(type="bad"))
    expect_error(SpatialDataAttrs(type = "point", dim=4))
    expect_error(SpatialDataAttrs(type = "shape", dim=4))
    # 2-4D image
    nms <- c("c", "t", "z", "y", "x")
    for (d in seq(2, 4)) {
        x <- SpatialDataAttrs(type="image", dim=d, nch=7)
        ok <- if (d == 2) nms[-c(2,3)] else if (d == 3) nms[-2] else nms
        # axes name
        y <- axes(x, "name")
        expect_length(y, 1+d)
        expect_type(y, "character")
        expect_identical(y, ok)
        # axes type
        y <- axes(x, "type")
        expect_equal(sum(y == "time"), ifelse(d == 4, 1, 0))
        expect_equal(sum(y == "space"), ifelse(d == 2, 2, 3))
        expect_equal(sum(y == "channel"), 1)
        # channels
        y <- channels(x)
        expect_length(y, 7)
        expect_type(y, "character")
        expect_all_true(!duplicated(y))
        # version
        expect_equal(x$spatialdata_attrs$version, "0.3")
    }
    # 2-4D label
    for (d in seq(2, 4)) {
        x <- SpatialDataAttrs(type="label", dim=d)
        y <- axes(x, "type")
        expect_length(y, d)
        expect_equal(sum(y == "time"), ifelse(d == 4, 1, 0))
        expect_equal(sum(y == "space"), ifelse(d == 2, 2, 3))
        # version
        expect_equal(x$spatialdata_attrs$version, "0.3")
    }
    # 3-4D shape/point
    nms <- c("x", "y", "z")
    for (d in seq(2, 3)) {
      for(typ in c("shape", "point")){
        x <- SpatialDataAttrs(type=typ, dim=d)
        ok <- if (d == 2) nms[-3] else nms
        y <- axes(x)
        expect_length(y, d)
        expect_equal(unlist(y), ok)
        expect_null(channels(x))
        # axes name
        y <- axes(x, "name")
        expect_length(y, d)
        expect_type(y, "character")
        expect_identical(y, ok)
        expect_error(axes(x, "type")) 
        # version
        expect_equal(x$spatialdata_attrs$version, 
                     if(typ == "point") "0.2" else "0.3")
      }
    }
})
