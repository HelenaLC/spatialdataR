dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")
x <- readSpatialData(dir)
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

# test_that("addCoord,identity", {
#     j <- addCoord(i, name <- "foo", type <- "identity", NULL)
#     df <- coords(i)
#     fd <- coord(j, name)
#     expect_s3_class(fd, "data.frame")
#     expect_true(is.null(fd$data))
#     expect_identical(fd$type, type)
#     expect_identical(fd$output$name, name)
#     expect_identical(coords(j)[seq(nrow(df)), , drop=FALSE], df)
# })
# test_that("addCoord,scale", {
#     j <- addCoord(i, name <- "foo", type <- "scale", data <- rep(1, 3))
#     df <- coords(i)
#     fd <- coords(j)
#     expect_s3_class(fd, "data.frame")
#     expect_equal(nrow(fd), nrow(df)+1)
# })
# test_that("rmvCoord", {
#     c <- coords(i)$output$name
#     j <- rmvCoord(i, c)
#     expect_true(!c %in% coords(j)$output$name)
#     expect_equal(nrow(coords(j)), nrow(coords(i))-1)
# })

test_that(".zattrs,read/write", {
    za <- file.path(dir, "images", "blobs_image", ".zattrs")
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
