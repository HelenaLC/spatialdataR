path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)
i <- image(x)
l <- label(x)
s <- shape(x)

test_that("zattrs,ImageArray", {
    l <- zattrs(i)
    expect_s4_class(l, "Zattrs")
})
test_that("zattrs,LabelArray", {
    l <- zattrs(l)
    expect_s4_class(l, "Zattrs")
})
test_that("zattrs,ShapeFrame", {
    l <- zattrs(s)
    expect_s4_class(l, "Zattrs")
})
test_that("addCoordTrans,identity", {
    j <- addCoordTrans(i, name <- "foo", type <- "identity", NULL)
    df <- coords(i)
    fd <- coord(j, name)
    expect_s3_class(fd, "data.frame")
    expect_true(is.null(fd$data))
    expect_identical(fd$type, type)
    expect_identical(fd$output$name, name)
    expect_identical(coords(j)[seq(nrow(df)), , drop=FALSE], df)
})
test_that("addCoordTrans,scale", {
    j <- addCoordTrans(i, name <- "foo", type <- "scale", data <- rep(1, 3))
    df <- coords(i)
    fd <- coords(j)
    expect_s3_class(fd, "data.frame")
    expect_equal(nrow(fd), nrow(df)+1)
})
test_that("rmvCoordTrans", {
    c <- coords(i)$output$name
    j <- rmvCoordTrans(i, c)
    expect_true(!c %in% coords(j)$output$name)
    expect_equal(nrow(coords(j)), nrow(coords(i))-1)
})

test_that(".zratts,read/write", {
    za <- file.path(path, "images", "blobs_image", ".zattrs")
    old <- jsonlite::fromJSON(za)
    new <- unclass(Zattrs(old))
    dir <- tempdir()
    tmp <- file.path(dir, ".zattrs")
    write(jsonlite::toJSON(new), tmp)
    new <- jsonlite::fromJSON(tmp)
    # should be identical, but there's
    # some double/integer conversion 
    # happening due to 'write'...
    expect_equal(new, old)
})
