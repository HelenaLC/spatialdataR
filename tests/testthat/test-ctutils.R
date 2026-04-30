x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

.CTtype <- c(
    "identity", "scale", "rotate", 
    "translation", "affine", "sequence")

test_that("CTlist", {
    y <- CTlist(label(x))
    expect_is(y, "list")
    expect_length(y, 5)
    z <- Reduce(intersect, lapply(y, names))
    expect_setequal(z, c("input", "output", "type"))
    z <- vapply(y, \(.) .$type, character(1))
    expect_true(all(z %in% .CTtype))
})
test_that("CTdata", {
    # invalid
    expect_error(CTdata(label(x), ""))
    expect_error(CTdata(label(x), 99))
    expect_error(CTdata(label(x), Inf))
    expect_error(CTdata(label(x), TRUE))
    # identity
    y <- CTdata(label(x), "global")
    expect_null(y)
    # scale
    y <- CTdata(label(x), "scale")
    expect_is(y, "list")
    expect_length(y, 2)
    expect_is(unlist(y), "numeric")
    expect_true(all(unlist(y) > 0))
    # translation
    y <- CTdata(label(x), "translation")
    expect_is(y, "list")
    expect_length(y, 2)
    expect_is(unlist(y), "numeric")
    # affine
    y <- CTdata(label(x), "affine")
    expect_is(y, "list")
    expect_length(y, 2)
    expect_is(unlist(y), "numeric")
    expect_true(all(unlist(y) > 0))
    z <- vapply(y, length, integer(1))
    expect_true(all(z == 3))
    # sequence
    y <- CTdata(label(x), "sequence")
    expect_is(y, "list")
    expect_length(y, 2)
    expect_true(all(names(y) %in% .CTtype))
    z <- vapply(y, length, integer(1))
    expect_true(all(z == 2))
})
test_that("CTtype", {
    y <- CTtype(label(x))
    expect_is(y, "character")
    expect_length(y, 5)
    expect_true(all(y %in% .CTtype))
})
test_that("CTname,element", {
    y <- CTname(label(x))
    expect_is(y, "character")
    expect_length(y, 5)
    expect_true(all(nchar(y) > 0))
    expect_true(!any(duplicated(y)))
})
test_that("CTname,object", {
    y <- CTname(x)
    expect_is(y, "character")
    expect_true(!any(duplicated(y)))
    y <- CTname(image(x))
    z <- CTname(meta(image(x)))
    expect_is(y, "character")
    expect_length(y, 1)
    expect_identical(y, z)
})

test_that("rmvCT", {
    y <- label(x)
    # invalid index/name
    expect_error(rmvCT(y, 100))
    expect_error(rmvCT(y, "."))
    expect_error(rmvCT(y, c(".", CTname(y)[1])))
    # identity is kept with a warning
    expect_warning(z <- rmvCT(y, "global"))
    expect_identical(CTname(z), CTname(y))
    # by name
    i <- sample(setdiff(CTname(y), "global"), 2) 
    expect_identical(CTname(rmvCT(y, i)), setdiff(CTname(y), i))
    # by index
    i <- sample(which(CTtype(y) != "identity"), 2) 
    expect_identical(CTname(rmvCT(y, i)), CTname(y)[-i])
})

test_that("addCT", {
    # get 1st element from each layer
    ls <- setdiff(SpatialData:::.LAYERS, "tables")
    es <- lapply(ls, \(.) x[.,1][[.]][[1]])
    .check_data <- \(z, x) {
        expect_true("." %in% CTname(z))
        ct <- CTlist(z)[[which(CTname(z) == ".")]]
        expect_identical(ct[[t]][[1]], x)
    }
    for (y in es) {
        t <- "identity"
        expect_error(addCT(y, ".", t, 12345))
        expect_silent(z <- addCT(y, ".", t, v <- NULL))
        .check_data(z, v)
        t <- "rotate"
        expect_error(addCT(y, ".", t, -12345)) # negative
        expect_error(addCT(y, ".", t, c(1,1))) # too many
        expect_error(addCT(y, ".", t, ".")) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- 1)) 
        .check_data(z, v)
        t <- "scale"
        d <- ifelse(is(y, "ImageArray"), 3, 2)
        expect_error(addCT(y, ".", t, numeric(d))) # zeroes
        expect_error(addCT(y, ".", t, 1+numeric(d+1))) # too many
        expect_error(addCT(y, ".", t, character(d))) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- 1+numeric(d)))
        .check_data(z, v)
        t <- "translation"
        expect_error(addCT(y, ".", t, numeric(d+1))) # too many
        expect_error(addCT(y, ".", t, character(d))) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- numeric(d)))
        .check_data(z, v)
    }
})
