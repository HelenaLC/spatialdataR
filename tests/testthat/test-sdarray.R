x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("data_type()", {
    # image
    za <- data(image(x))
    dt <- data_type(za)
    expect_length(dt, 1)
    expect_is(dt, "character")
    expect_identical(dt, "float64")
    expect_identical(dt, data_type(za[1,,]))
    expect_identical(dt, data_type(image(x)))
    # label
    za <- data(label(x))
    dt <- data_type(za)
    expect_length(dt, 1)
    expect_is(dt, "character")
    expect_identical(dt, "int16")
    expect_identical(dt, data_type(head(za)))
    expect_identical(dt, data_type(label(x)))
})

test_that("SpatialDataImage()", {
    rgb <- \(n) sample(seq_len(255), n, replace=TRUE)
    mat <- array(rgb(3*20*20), dim=c(3,20,20))
    SpatialDataImage(mat)
    # invalid
    expect_error(SpatialDataImage(mat, 1))
    expect_error(SpatialDataImage(mat, list()))
    # single scale
    expect_silent(SpatialDataImage(list()))
    expect_silent(SpatialDataImage(list(mat)))
    expect_silent(SpatialDataImage(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(rgb(prod(.)), dim=.))
    expect_silent(SpatialDataImage(lys))
})

test_that("data(),SpatialDataImage", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- SpatialDataImage(lys)
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})


test_that("create, SpatialDataImage", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- SpatialDataImage(img)
  expect_identical(data(imgarray), img)
  expect_identical(dim(imgarray),dim(img))
  
  # coordinate systems
  expect_identical(CTname(imgarray), "global")
  expect_identical(CTtype(imgarray), "identity")
  imgarray_new <- addCT(imgarray, "test", "scale", c(1,2,2))
  expect_identical(CTname(imgarray_new), c("global", "test"))
  expect_identical(CTtype(imgarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(images = list(test_image = imgarray))
  expect_identical(data(image(sd)), data(imgarray))
  expect_identical(image(sd), imgarray)
  expect_identical(image(sd, 1), imgarray)
})

test_that("create multiscale, SpatialDataImage", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- SpatialDataImage(img, scale_factors = c(2,2,2))
  expect_identical(data(imgarray), img)
  expect_identical(dim(imgarray),dim(img))
  
  # coordinate systems
  expect_identical(CTname(imgarray), "global")
  expect_identical(CTtype(imgarray), "identity")
  imgarray_new <- addCT(imgarray, "test", "scale", c(1,2,2))
  expect_identical(CTname(imgarray_new), c("global", "test"))
  expect_identical(CTtype(imgarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(images = list(test_image = imgarray))
  expect_identical(data(image(sd)), data(imgarray))
  expect_identical(data(image(sd),2), data(imgarray,2))
  expect_identical(data(image(sd),3), data(imgarray,3))
  expect_identical(image(sd), imgarray)
  expect_identical(image(sd, 1), imgarray)
})

z <- list(0.1, 0.2)

for (v in names(z)) {
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write, SpatialDataImage", {
    
    # create image
    set.seed(1)
    img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
                 dim = c(3,100,100))
    
    # make image array
    imgarray <- SpatialDataImage(img, version = image(sdFormat(v)))
    sd <- SpatialData(images = list(test_image = imgarray))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    imgarray2 <- image(sd2)
    expect_identical(realize(data(imgarray)), 
                     realize(data(imgarray2)))
    expect_equal(meta(imgarray),
                 meta(imgarray2))
  })
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write multiscale, SpatialDataImage", {
    
    # create image
    set.seed(1)
    img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
                 dim = c(3,100,100))
    
    # make image array
    imgarray <- SpatialDataImage(img, scale_factors = c(2,2,2), 
                                 version = image(sdFormat(v)))
    sd <- SpatialData(images = list(test_image = imgarray))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    imgarray2 <- image(sd2)
    expect_identical(realize(data(imgarray, 1)), 
                     realize(data(imgarray2, 1)))
    expect_identical(realize(data(imgarray, 2)), 
                     realize(data(imgarray2, 2)))
    expect_identical(realize(data(imgarray, 3)), 
                     realize(data(imgarray2, 3)))
    expect_equal(meta(imgarray),meta(imgarray2))
  })
}

test_that("SpatialDataLabel()", {
    val <- sample(seq_len(12), 20*20, replace=TRUE)
    mat <- array(val, dim=c(20, 20))
    SpatialDataLabel(mat)
    # invalid
    expect_error(SpatialDataLabel(mat, 1))
    expect_error(SpatialDataLabel(mat, list()))
    # single scale
    expect_silent(SpatialDataLabel(list()))
    expect_silent(SpatialDataLabel(list(mat)))
    expect_silent(SpatialDataLabel(list(mat), SpatialDataAttrs()))
    # multiscale
    dim <- lapply(c(20, 10, 5), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(sample(seq_len(12), prod(.), replace=TRUE), dim=.))
    expect_silent(SpatialDataLabel(lys))
})

test_that("data(),SpatialDataLabel", {
    dim <- lapply(c(8, 4, 2), \(.) rep(., 2))
    lys <- lapply(dim, \(.) array(0L, dim=.))
    lab <- SpatialDataLabel(lys)
    for (. in seq_along(lys))
        expect_identical(data(lab, .), lys[[.]])
    expect_identical(data(lab, Inf), lys[[3]])
    expect_error(data(lab, 0))
    expect_error(data(lab, -1))
    expect_error(data(lab, 99))
    expect_error(data(lab, ""))
    expect_error(data(lab, c(1,2)))
})

test_that("create,SpatialDataLabel", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- SpatialDataLabel(lbl)
  expect_identical(data(lblarray), lbl)
  expect_identical(dim(lblarray),dim(lbl))
  
  # coordinate systems
  expect_identical(CTname(lblarray), "global")
  expect_identical(CTtype(lblarray), "identity")
  lblarray_new <- addCT(lblarray, "test", "scale", c(2,2))
  expect_identical(CTname(lblarray_new), c("global", "test"))
  expect_identical(CTtype(lblarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(labels = list(test_label = lblarray))
  expect_identical(data(label(sd)), data(lblarray))
  expect_identical(label(sd), lblarray)
  expect_identical(label(sd, 1), lblarray)
})

test_that("create multiscale,SpatialDataLabel", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- SpatialDataLabel(lbl, scale_factors = c(2,2,2))
  expect_identical(data(lblarray), lbl)
  expect_identical(dim(lblarray),dim(lbl))
  
  # coordinate systems
  expect_identical(CTname(lblarray), "global")
  expect_identical(CTtype(lblarray), "identity")
  lblarray_new <- addCT(lblarray, "test", "scale", c(2,2))
  expect_identical(CTname(lblarray_new), c("global", "test"))
  expect_identical(CTtype(lblarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(labels = list(test_label = lblarray))
  expect_identical(data(label(sd)), data(lblarray))
  expect_identical(data(label(sd),2), data(lblarray,2))
  expect_identical(data(label(sd),3), data(lblarray,3))
  expect_identical(label(sd), lblarray)
  expect_identical(label(sd, 1), lblarray)
})

z <- list(0.1, 0.2)

for (v in names(z)) {
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write,SpatialDataLabel", {
    
    # create label
    set.seed(1)
    lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
                 dim = c(100,100))
    
    # make label array
    lblarray <- SpatialDataLabel(lbl, version = label(sdFormat(v)))
    sd <- SpatialData(labels = list(test_label = lblarray))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    lblarray2 <- label(sd2)
    expect_identical(realize(data(lblarray)), 
                     realize(data(lblarray2)))
    expect_equal(meta(lblarray),meta(lblarray2))
  })
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write multiscale,SpatialDataLabel", {
    
    # create label
    set.seed(1)
    lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
                 dim = c(100,100))
    
    # make label array
    lblarray <- SpatialDataLabel(lbl, scale_factors = c(2,2,2), 
                                 version = label(sdFormat(v)))
    sd <- SpatialData(labels = list(test_label = lblarray))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    lblarray2 <- label(sd2)
    expect_identical(realize(data(lblarray)), 
                     realize(data(lblarray2)))
    expect_identical(realize(data(lblarray, 2)), 
                     realize(data(lblarray2, 2)))
    expect_identical(realize(data(lblarray, 3)), 
                     realize(data(lblarray2, 3)))
    expect_equal(meta(lblarray),meta(lblarray2))
  })
  
}