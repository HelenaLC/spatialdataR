path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)
i <- image(x)
l <- label(x)
s <- shape(x)

test_that("zattrs,ImageArray", {
    l <- zattrs(i)
    expect_type(l, "list")
})
test_that("zattrs,LabelArray", {
    l <- zattrs(l)
    expect_type(l, "list")
})
test_that("zattrs,ShapeFrame", {
    l <- zattrs(s)
    expect_type(l, "list")
})
test_that(".add_coord", {
    name <- "poke"
    type <- "fire"
    j <- .add_coord(i, name, type, 123)
    df <- coords(i)
    fd <- coord(j, name)
    expect_s3_class(fd, "data.frame")
    expect_identical(fd$type, type)
    expect_identical(fd$output$name, name)
    expect_identical(coords(j)[seq(nrow(df)), , drop=FALSE], df)
})
test_that(".rmv_cood", {
    c <- coords(i)$output$name
    j <- .rmv_coord(i, c)
    expect_true(!c %in% coords(j)$output$name)
    expect_equal(nrow(coords(j)), nrow(coords(i))-1)
})
