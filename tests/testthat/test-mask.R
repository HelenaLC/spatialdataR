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

test_that("mask,unaligned", {
    i <- "blobs_image"
    j <- "blobs_labels"
    
    # non-existent
    expect_error(
        mask(x, i, j, "x"), 
        "should be “global”")
    
    # not shared
    za <- meta(image(x, i))
    ct <- "coordinateTransformations"
    za$multiscales[[1]][[ct]][[1]]$output$name <- "x"
    y <- x; meta(image(y, i)) <- za
    expect_error(
        mask(y, i, j, "x"), 
        "found no common")
})

test_that("mask,sdImage,sdLabel", {
    i <- "blobs_image"
    j <- "blobs_labels"
    
    # default to 'mean' with a message
    expect_message(y <- mask(x, i, j))
    expect_silent(z <- mask(x, i, j, how="mean"))
    expect_identical(y, z)
    
    # check against original
    expect_equivalent(
        assay(tables(y)[[2]]), 
        assay(tables(x)[[1]]))
})

test_that("mask w/ transform", {
    i <- "blobs_image"
    j <- "blobs_labels"
    a <- element(x, i)
    b <- element(x, j)
    
    # misaligned
    l <- list(1,.1,.1); t <- "scale"
    a <- addCT(a, name=t, type=t, data=l)
    y <- x; y[[layer(y, i)]][[i]] <- a
    expect_error(mask(y, i, j, t))
    
    # aligned
    l <- c(list(1), CTdata(b, t <- "scale"))
    a <- addCT(a, name=t, type=t, data=l)
    y <- x; y[[layer(y, i)]][[i]] <- a
    expect_silent(z <- mask(y, i, j, t, how=how <- "sum"))
    
    # in/valid CT index (not name)
    expect_error(mask(y, i, j, 0))
    expect_error(mask(y, i, j, 9))
    t <- which(CTname(a) == t)
    expect_identical(z, mask(y, i, j, t, how="sum"))
    
    # check structure
    se <- tail(tables(z),1)[[1]]
    expect_identical(assayNames(se), how)
    expect_equal(dim(se), c(dim(a)[1], length(instances(b))))
    expect_identical(rownames(se), as.character(channels(a)))
    expect_setequal(colnames(se), as.character(instances(b)))
    
    # check aggregation
    replicate(3, {
        . <- sample(instances(b), 1)
        mx <- as.matrix(data(a)[1,,])
        my <- as.matrix(data(b) == .)
        expect_identical(sum(mx*my), assay(se)[1,as.character(.)])
    })
})

test_that("mask,sdPoint,sdShape", {
    i <- "blobs_points"
    j <- "blobs_circles"
    k <- "blobs_polygons"
    
    # can only count points
    expect_message(mask(x, i, j, how="mean"))
    
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
    bb <- st_as_sfc(bb)
    bb <- st_sf(geometry=bb)
    y <- SpatialDataShape(bb)
    
    # missing table
    shape(x, j <- "box") <- y
    expect_error(mask(x, i, j))
    
    # w/ mock table
    mx <- matrix(runif(7*n),7,n)
    se <- SingleCellExperiment(mx)
    y <- setTable(x, i, se)
    
    # out-of-bounds masking
    shape(y, "out") <- translation(s, c(1e3,1e3))
    expect_error(mask(y, i, "out", how="sum"))
    
    # note: data at "0" are from non-intersecting instances;
    # here, all data should be aggregated to column "1"
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
})
