z <- list(v1="blobs.zarr", v3="blobs_v3.zarr")

for (v in names(z)) {
    
    x <- file.path("extdata", z[[v]])
    x <- system.file(x, package="SpatialData")
    x <- readSpatialData(x, anndataR=TRUE)
        
    test_that(paste0(v, "-multiscales"), {
        y <- meta(image(x))
        z <- multiscales(y)
        expect_is(z, "list")
        expect_length(z, 1)
        y$spatialdata_attrs <- NULL
        expect_error(multiscales(y))
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