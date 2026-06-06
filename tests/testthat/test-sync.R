require(SingleCellExperiment, quietly=TRUE)
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

test_that(".sync_tables_on_crop,label", {
    i <- region(table(sd))
    e <- element(sd, i)
    
    # crop around one label
    id <- sample(is <- instances(e), 1)
    mx <- as(data(e), "dgCMatrix")
    ij <- (nz <- summary(mx))[nz$x == id, ]
    dx <- range(ij[, 2]); dy <- range(ij[, 1])
    bb <- list(xmin=dx[1], xmax=dx[2], ymin=dy[1], ymax=dy[2])
    x <- crop(sd, bb)
    
    # verify table integrity
    se <- getTable(x, i)
    mx <- data(element(x, i))
    ni <- length(is <- unique(mx[mx != 0]))

    expect_equal(ncol(se), ni)      # right instances
    expect_identical(region(se), i) # regions untouched
})

test_that(".sync_tables_on_crop,shape", {
    i <- shapeNames(sd)[1]
    e <- element(sd, i)
    
    # mock shape annotation
    mx <- matrix(nc=length(e))
    se <- SingleCellExperiment(list(mx))
    sd <- setTable(sd, i, se)
    
    # crop around one shape
    id <- sample(length(e), 1)
    xy <- centroids(e)[id, ]
    dx <- xy[[1]]+c(-(. <- 1e-3), .); dy <- xy[[2]]+c(-., .)
    bb <- list(xmin=dx[1], xmax=dx[2], ymin=dy[1], ymax=dy[2])
    x <- crop(sd, bb)
    
    # verify table integrity
    se <- getTable(x, i)
    expect_equal(ncol(se), 1)       # right instances
    expect_identical(region(se), i) # regions untouched
})
