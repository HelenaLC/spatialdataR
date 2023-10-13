s4 <- list(
    "images"="ImageArray",
    "labels"="LabelArray",
    "shapes"="ShapeFrame",
    "points"="PointFrame")

.no_s <- \(.) gsub("s$", "", .)

# new ----

test_that("SpatialData,nan", {
  x <- SpatialData()
  expect_s4_class(x, "SpatialData")
  expect_length(elements(x), 0)
})

test_that("SpatialData,one", {
    for (e in names(s4)) {
        f <- s4[e]
        f[[e]] <- get(s4[[e]])()
        # new obj
        x <- do.call(SpatialData, f)
        expect_s4_class(x, "SpatialData")
        # get one
        y <- get(.no_s(e))(x)
        expect_s4_class(y, s4[[e]])
        # get all
        y <- get(e)(x)
        expect_type(y, "list")
        expect_s4_class(y[[1]], s4[[e]])
        # get any
        y <- elements(x)
        expect_type(y, "list")
        expect_identical(names(y), e)
    }
})

dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")
x <- readSpatialData(dir)

# nms ----

test_that("image/label/shape/pointNames()", {
    for (e in names(s4)) {
        y <- get(paste0(.no_s(e), "Names"))(x)
        n <- length(l <- attr(x, e))
        expect_length(y, n)
        if (n > 0) {
            expect_type(y, "character")
            expect_identical(y, names(l))
        }
    }
})

test_that("elementNames()", {
    y <- elementNames(x)
    expect_type(y, "list")
    for (. in y) expect_type(., "character")
    layers <- attributes(x)
    layers <- layers[setdiff(names(layers), c("metadata", "class"))]
    .na <- \(.) length(.) == 0 || is(., "name")
    expect_length(y, sum(!vapply(layers, .na, logical(1))))
})

# get one ----

test_that("image/label/shape/point()", {
    for (e in names(s4)) {
        f <- get(.no_s(e))
        g <- get(paste0(.no_s(e), "Names"))
        # nope
        expect_error(f(x, 00))
        expect_error(f(x, .1))
        expect_error(f(x, -1))
        expect_error(f(x, 99))
        expect_error(f(x, ""))
        # yeah
        expect_silent(f(x))
        expect_s4_class(f(x), s4[[e]])
        expect_identical(f(x, 1), f(x))
        expect_identical(f(x, g(x)[1]), f(x))
    }
})

# test_that("table()", {
#   expect_s4_class(table(x), "SingleCellExperiment")
#   expect_error(table(x) <- "")
#   expect_error(table(x) <- NA)
#   expect_silent(table(x) <- NULL)
# })

test_that("element()", {
    expect_error(element(x, e="foo"))
    expect_error(element(x, i="foo"))
    expect_error(element(x, i=12345))
    for (e in names(s4)) expect_s4_class(element(x, e), s4[[e]])
})

# get all ----

test_that("images/labels/shapes/points()", {
    for (e in names(s4)) expect_identical(get(e)(x), attr(x, e))
})

# set one ----

test_that("image/label/shape/point<-()", {
    for (e in names(s4)) {
        y <- get(s4[[e]])()
        f <- get(paste0(.no_s(e), "<-"))
        # null
        expect_silent(z <- f(x, 1, value=NULL))
        expect_identical(get(e)(z), get(e)(x)[-1])
        # nope
        expect_error(f(x, value="foo"))
        expect_error(f(x, 99, value=y))
        # yeah
        expect_silent(z <- f(x, i <- "foo", value=y))
        expect_identical(get(e)(z), c(get(e)(x), list(foo=y)))
    }
})

# set all ----

test_that("images/labels/shapes/points<-()", {
    for (e in names(s4)) {
        f <- get(paste0(.no_s(e), "s<-"))
        # null
        expect_silent(y <- f(x, list()))
        expect_type(get(e)(y), "list")
        expect_length(get(e)(y), 0)
        # yeah
        v <- list(get(s4[[e]])())
        expect_silent(y <- f(x, v))
        expect_identical(get(e)(y), v)
    }
})
