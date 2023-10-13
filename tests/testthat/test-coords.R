dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")
x <- readSpatialData(dir)

test_that("coordNames,SpatialDataEelement", {
    for (e in c("image", "label", "shape", "point"))
        expect_identical(coordNames(get(e)(x)), "global")
})

test_that("coordNames,list", {
    y <- Reduce(c, elements(x))
    expect_identical(coordNames(x), coordNames(y))
    expect_identical(coordNames(x), coordNames(y[sample(length(y), 2)]))
})

test_that("coords", {
    z <- coords(y <- image(x))
    expect_s3_class(z, "data.frame")
    expect_length(nrow(z), length(coordNames(y)))
    expect_identical(z$output$name, coordNames(y))
})
