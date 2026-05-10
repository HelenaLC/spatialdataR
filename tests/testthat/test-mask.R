require(sf, quietly=TRUE)
require(SingleCellExperiment, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

test_that("mask,unsupported", {
    nm <- list(
        c(imageNames(x)[1], imageNames(x)[2]), # image,image
        c(labelNames(x)[1], labelNames(x)[2]), # label,label
        c(labelNames(x)[1], imageNames(x)[1]), # label,image
        c(shapeNames(x)[1], pointNames(x)[1])) # shape,point
    for (ij in nm) expect_error(mask(x, ij[1], ij[2]))
})

test_that("mask,sdImage,sdLabel", {
    i <- "blobs_image"
    j <- "blobs_labels"
    
    # reproduce example data
    y <- mask(x, i, j, how="sum")
    expect_equivalent(
        assay(tables(y)[[2]]), 
        assay(tables(x)[[1]]))
    
    # default to 'mean' with a message
    expect_message(y <- mask(x, i, j))
    expect_silent(z <- mask(x, i, j, how="mean"))
    expect_identical(y, z)
})

test_that("mask,sdPoint,sdShape", {
    i <- "blobs_points"
    j <- "blobs_circles"
    k <- "blobs_polygons"
    
    # test basic masking
    y <- mask(x, i, j)
    t <- getTable(y, j, drop=FALSE)
    
    # check dimensions: features x (1 + #shapes)
    fk <- feature_key(p <- point(x, i))
    np <- length(unique(as.data.frame(p)[[fk]]))
    nc <- nrow(shape(x, j))
    expect_equal(dim(t), c(np, nc + 1))
    expect_true("0" %in% colnames(t))
    
    # check counts: 
    # points in "0" column are those with NO intersection;
    # assay sum = (#points) + duplicates (points in multiple shapes)
    np <- nrow(as.data.frame(p))
    n0 <- t$n_instances["0"]
    
    # manually find points with NO intersections
    ij <- SpatialData:::.mask_map(p, shape(x, j))
    is <- dplyr::collect(ij)$id_y
    nq <- length(unique(is))
    expect_equal(as.numeric(n0), np - nq)
    
    # check that custom naming works
    y <- mask(x, i, j, name="x")
    expect_true("x" %in% tableNames(y))
    
    # mask again using a different mask
    y <- mask(x, i, j, name="t1")
    z <- mask(y, i, k, name="t2")
    
    expect_true("t1" %in% tableNames(z))
    expect_true("t2" %in% tableNames(z))
})

test_that("mask,sdShape,sdShape", {
    i <- "blobs_polygons"
    s <- shape(x, i)
    n <- length(s)
    
    # mock all-inclusive shape
    ex <- extent(s)
    bb <- st_bbox(c(
        xmin=ex$x[1],
        ymin=ex$y[1],
        xmax=ex$x[2],
        ymax=ex$y[2]))
    nn <- st_as_sfc(bb)
    bb <- st_sf(geometry=nn)
    y <- SpatialDataShape(bb)
    
    # missing table
    shape(x, j <- "box") <- y
    expect_error(mask(x, i, j))
    
    # w/ mock table
    mx <- matrix(runif(7*n),7,n)
    se <- SingleCellExperiment(mx)
    y <- setTable(x, i, se)
    
    for (how in c("sum", "mean", "detected", "prop.detected")) {
        fun <- switch(how, 
            sum=rowSums, mean=rowMeans,
            detected=\(.) rowSums(. > 0),
            prop.detected=\(.) rowMeans(. > 0))
        z <- mask(y, i, j, how=how)
        expect_length(tables(z), 1+length(tables(y)))
        sf <- tail(tables(z), 1)[[1]]
        expect_equal(dim(sf), c(7,2))
        expect_identical(assay(sf)[,"1"], fun(mx))
    }
    
    # non-null value
    
})
