library(SpatialData)
library(testthat)
library(sf)

test_that(".box2rev works with real image and injected scale", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a scale transformation into global space
    # Axes are c, y, x. Scale by 1, 2, 3.
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "scale"
    m$multiscales[[1]]$coordinateTransformations[[1]]$scale <- c(1, 2, 3)
    # Use @meta since meta<- might not be exported
    img@meta <- m
    
    y <- list(xmin=30, xmax=60, ymin=20, ymax=40)
    z <- SpatialData:::.box2rev(img, y, j=1)
    
    # Expected: x/3, y/2
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 20)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 20)
})

test_that(".box2rev handles j as character", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a scale transformation into global space
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "scale"
    m$multiscales[[1]]$coordinateTransformations[[1]]$scale <- c(1, 2, 3)
    img@meta <- m
    
    y <- list(xmin=30, xmax=60, ymin=20, ymax=40)
    z <- SpatialData:::.box2rev(img, y, j="global")
    
    expect_equal(unname(z$xmin), 10)
})

test_that(".box2rev works with identity (default)", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    y <- list(xmin=10, xmax=50, ymin=10, ymax=50)
    z <- SpatialData:::.box2rev(img, y, j=1)
    
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 50)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 50)
})

test_that(".box2rev handles sequence transformation", {
    path <- system.file("extdata", "blobs.zarr", package="SpatialData")
    sd <- readSpatialData(path)
    img <- image(sd)
    
    # Inject a sequence: scale then translation
    # Scale: c=1, y=2, x=3
    # Translation: c=0, y=10, x=5
    m <- meta(img)
    m$multiscales[[1]]$coordinateTransformations[[1]]$type <- "sequence"
    m$multiscales[[1]]$coordinateTransformations[[1]]$transformations <- list(
        list(type="scale", scale=c(1, 2, 3)),
        list(type="translation", translation=c(0, 10, 5))
    )
    img@meta <- m
    
    # Query in global space
    # (x_array * 3) + 5 = x_global  => x_array = (x_global - 5) / 3
    # (y_array * 2) + 10 = y_global => y_array = (y_global - 10) / 2
    
    y <- list(xmin=35, xmax=65, ymin=30, ymax=50)
    z <- SpatialData:::.box2rev(img, y, j=1)
    
    expect_equal(unname(z$xmin), 10)
    expect_equal(unname(z$xmax), 20)
    expect_equal(unname(z$ymin), 10)
    expect_equal(unname(z$ymax), 20)
})
