x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("bounding box", {
    d <- dim(i <- image(x))
    # missing bounding box coordinates
    expect_error(query(i, xmin=0, xmax=1, ymin=0))
    # invalid coordinate space
    expect_error(query(i, ".", xmin=0, xmax=1, ymin=0, ymax=1))
    expect_error(query(i, 100, xmin=0, xmax=1, ymin=0, ymax=1))
    # neither crop nor shift
    expect_identical(query(i, xmin=0, xmax=d[3], ymin=0, ymax=d[2]), i)
    # order is irrelevant
    expect_identical(query(i, ymax=d[2], xmax=d[3], xmin=0, ymin=0), i)
    # crop but don't shift
    j <- query(i, xmin=0, xmax=w <- d[3]/2, ymin=0, ymax=h <- d[2]/4)
    expect_equal(dim(j), c(3, h, w)) 
    expect_identical(coordTransData(i), coordTransData(j))
    # crop and shift
    j <- query(i, xmin=1, xmax=w <- d[3]/2, ymin=2, ymax=h <- d[2]/4)
    expect_equal(dim(j), c(3, 1+h-2, 1+w-1))
    expect_equal(coordTransType(j), t <- "translation")
    expect_equivalent(coordTransData(j)[[t]][[1]], c(0, 2, 1))
})