library(testthat)
library(SpatialData)
library(SingleCellExperiment)

context("setTable (new)")

test_that("setTable() correctly associates a SingleCellExperiment with an element", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path, anndataR=TRUE)
    
    # 1. Basic association with a label element
    i <- "blobs_labels"
    e <- element(sd, i)
    # Create a minimal SCE with matching number of columns
    sce <- SingleCellExperiment(matrix(0, 1, length(instances(e))))
    
    sd_new <- setTable(sd, i, sce)
    
    expect_true(paste0(i, "_table") %in% tableNames(sd_new))
    t <- getTable(sd_new, i)
    expect_equal(region(t), i)
    expect_equal(instance_key(t), "instance_id") # default ik
    expect_equal(region_key(t), "region") # default rk
})

test_that("setTable() handles custom name and keys", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path, anndataR=TRUE)
    
    i <- "blobs_circles"
    e <- element(sd, i)
    sce <- SingleCellExperiment(matrix(0, 1, nrow(e)))
    
    sd_new <- setTable(sd, i, sce, name="my_custom_table", rk="my_rk", ik="my_ik")
    
    expect_true("my_custom_table" %in% tableNames(sd_new))
    t <- SpatialData::table(sd_new, "my_custom_table")
    expect_equal(region_key(t), "my_rk")
    expect_equal(instance_key(t), "my_ik")
})

test_that("setTable() fails with invalid inputs", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path, anndataR=TRUE)
    
    i <- "blobs_labels"
    e <- element(sd, i)
    
    # Not an SCE
    expect_error(setTable(sd, i, data.frame(a=1)))
    
    # Mismatched dimensions (if instances are not set)
    # The current implementation checks ncol(y) vs nrow(e) if instances(y) is NULL
    sce_wrong <- SingleCellExperiment(matrix(0, 1, length(instances(e)) + 1))
    expect_error(setTable(sd, i, sce_wrong), "ncol\\(y\\)' must match 'nrow\\(element\\(x, i\\)\\)'")
    
    # Non-existent element
    expect_error(setTable(sd, "non_existent", SingleCellExperiment()), "is not an element of 'x'")
})
