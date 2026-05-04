library(SingleCellExperiment)
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

test_that("mask,ImageArray,LabelArray", {
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

test_that("mask,PointFrame,ShapeFrame", {
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

# TODO: omit SpatialData.data

test_that("mask,ShapeFrame,ShapeFrame", {
    testthat::skip()
    
    i <- "cells"
    j <- "anatomical"
    
    # error without 'table'
    y <- x; tables(y) <- list()
    expect_error(mask(y, i, j))
    
    # test basic masking with "0" column
    y <- mask(x, i, j, how="sum")
    old <- getTable(x, i)
    new <- getTable(y, j, drop=FALSE)
    
    # dimensions: features x (1 + #shapes)
    expect_equal(dim(new), c(nrow(old), nrow(shape(x, j)) + 1))
    expect_true("0" %in% colnames(new))
    
    # sum of aggregated should match original total (for "sum")
    expect_equal(sum(assay(new)), sum(assay(old)))
    expect_equal(sum(new$n_instances), ncol(old))
    
    # test with partial values (subset of genes)
    v <- sample(rownames(old), 10)
    y_sub <- mask(x, i, j, value=v)
    new_sub <- getTable(y_sub, j, drop=FALSE)
    expect_equal(nrow(new_sub), length(v))
    expect_equal(sum(assay(new_sub)), sum(assay(old[v, ])))
})
