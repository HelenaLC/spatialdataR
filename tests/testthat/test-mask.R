library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, anndataR=TRUE)

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
    y <- mask(x, i, j)
    t <- getTable(y, j)
    fk <- feature_key(p <- point(x, i))
    np <- length(unique(p[[fk]]))
    nc <- nrow(shape(x, j))
    expect_equal(dim(t), c(np, nc))
    # ignore 'how' with a warning
    expect_warning(mask(x, i, j, how="sum"))
})

require(SpatialData.data, quietly=TRUE)
x <- get_demo_SDdata("merfish")
x <- readSpatialData(x)

test_that("mask,ShapeFrame,ShapeFrame", {
    i <- "cells"
    j <- "anatomical"
    # error without 'table'
    y <- x; tables(y) <- list()
    expect_error(mask(y, i, j))
    # default to 'sum' with a message
    expect_silent(y <- mask(x, i, j))
    expect_message(z <- mask(x, i, j, how="sum"))
    expect_identical(y, z)
    old <- getTable(y, i)
    new <- getTable(y, j)
    expect_equal(dim(new), c(nrow(old), nrow(shape(x, j))+1))
    expect_equal(sum(assay(new)), sum(assay(old)))
    expect_identical(rownames(new), rownames(old))
    expect_identical(meta(new)$region, j)
    # 'value' should be a character 
    # vector of rownames in 'table(x, i)'
    v <- sample(rownames(old), 5)
    new <- getTable(mask(x, i, j, value=v), j)
    expect_equal(sum(assay(new[v, ])), sum(assay(old[v, ])))
    expect_error(mask(x, i, j, value=`[<-`(v, i=1, "x")))
})
