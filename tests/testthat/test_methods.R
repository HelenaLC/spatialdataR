library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

fun <- c("image", "label", "shape", "point", "table")
nms <- c("blobs_image", "blobs_labels", "blobs_circles", "blobs_points", "table")
typ <- c("ImageArray", "LabelArray", "ShapeFrame", "PointFrame", "SingleCellExperiment")

# get ----

test_that("get all", {
    for (f in paste0(fun, "s"))
        expect_is(get(f)(x), "list")
})

test_that("get one", {
    # i=numeric
    mapply(f=fun, t=typ, \(f, t) 
        expect_is(get(f)(x, i=1), t))
    # i=character
    mapply(f=fun, t=typ, n=nms, \(f, t, n) 
        expect_is(get(f)(x, n), t))
    # i=invalid
    for (f in fun) {
        expect_error(get(f)(x, 0))
        expect_error(get(f)(x, "."))
        expect_error(get(f)(x, c(1,1)))
    }
})

# set ----

test_that("set all", {
    obj <- list(
        ImageArray(), LabelArray(), 
        ShapeFrame(), PointFrame(), 
        SingleCellExperiment())
    names(obj) <- .LAYERS
    for (. in .LAYERS) {
        y <- x; y[[.]] <- list()
        expect_length(y[[.]], 0)
        # character
        y[[.]] <- list(obj[[.]])
        expect_length(y[[.]], 1)
        expect_identical(y[[.]][[1]], obj[[.]])
        # index
        y[[.]][[2]] <- obj[[.]]
        expect_length(y[[.]], 2)
        expect_identical(y[[.]][[2]], obj[[.]])
    }
})

test_that("set one", {
    # value=NULL
    for (f in fun) {
        n <- length(get(paste0(f, "s"))(x))
        x <- get(paste0(f, "<-"))(x, i=1, value=NULL)
        m <- length(get(paste0(f, "s"))(x))
        expect_true(m == (n-1))
    }
    # value=in/valid
    obj <- list(
        ImageArray(), LabelArray(), 
        ShapeFrame(), PointFrame(), 
        SingleCellExperiment())
    mapply(f=fun, o=obj, t=typ, \(f, o, t) {
        set <- get(paste0(f, "<-"))
        nms <- get(paste0(f, "Names"))
        # character
        x <- set(x, i=".", value=o)
        expect_true("." %in% nms(x))
        expect_is(get(f)(x, "."), t)
        # numeric
        x <- set(x, i=1, value=o)
        expect_is(get(f)(x, 1), t)
        # missing
        n <- \(.) length(get(paste0(f, "s"))(.))
        expect_silent(set(x, value=o))
        y <- set(x, value=NULL)
        expect_equal(n(y), n(x)-1)
        # invalid
        expect_error(set(x, i=1, value=1))
    })
})

test_that("get nms", {
    for (f in fun) {
        lys <- get(paste0(f, "s"))
        nms <- get(paste0(f, "Names"))
        expect_is(nms(x), "character")
        expect_identical(nms(x), names(lys(x)))
    }
})

# $ ----

test_that("$", {
    mapply(i=paste0(fun, "s"), n=nms, t=typ, \(i, n, t) {
        # object-wide
        f <- parse(text=sprintf("x$%s", i))
        expect_is(y <- eval(f), "list")
        # element-wise
        expect_is(names(y), "character")
        expect_length(names(y), length(y))
        f <- parse(text=sprintf("y$%s", n))
        expect_is(eval(f), t)
    })
})

# sub ----

test_that("[,Shape/PointFrame", {
    for (y in list(shape(x), point(x))) {
        # one index subsets in vector-like fashion
        expect_equal(dim(y[1]), c(1, ncol(y)))
        # two indices subset in array-like fashion
        expect_equal(nrow(y[1,]), 1) # no j
        expect_equal(ncol(y[,1]), 1) # no i
        expect_equal(dim(y[1,1]), c(1,1)) # both
        expect_identical(dim(y[,]), dim(y)) # none
        expect_equal(nrow(y[-1,]), nrow(y)-1) # neg
    }
})

test_that("[,LabelArray", {
    y <- label(x)
    # logical
    expect_identical(y[TRUE,TRUE], y) 
    expect_equal(dim(y[FALSE,FALSE]), c(0,0)) 
    expect_equal(dim(y[FALSE,TRUE]), c(0,ncol(y))) 
    expect_equal(dim(y[TRUE,FALSE]), c(nrow(y),0)) 
    i <- logical(nrow(y)); j <- logical(ncol(y))
    n <- replicate(2, sample(seq(2, 10), 1))
    i[sample(nrow(y), n[1])] <- TRUE
    j[sample(ncol(y), n[2])] <- TRUE
    expect_equal(nrow(y[i,]), n[1])
    expect_equal(ncol(y[,j]), n[2])
    # numeric
    expect_identical(y[,], y) # none
    expect_equal(nrow(y[1,]), 1) # no j
    expect_equal(ncol(y[,1]), 1) # no i
    expect_equal(dim(y[1,1]), c(1,1)) # both
    # TODO: multiscales
})

# test_that("[,ImageArray", {
#     d <- \(.) dim(data(., 1))
#     i <- image(x, "blobs_image")
#     expect_identical(d(i[TRUE,,])[1], d(i)[1])
#     expect_identical(d(i[,TRUE,])[2], d(i)[2])
#     expect_identical(d(i[,,TRUE])[3], d(i)[3])
# })

test_that("[,SpatialData", {
    # count number of elements in each layer
    .n <- \(.) vapply(attributes(.)[.LAYERS], length, numeric(1)) 
    # logical
    expect_true(all(.n(x[TRUE]) == .n(x)))
    expect_true(all(.n(x[FALSE]) == 0))
    # i=missing
    expect_true(all(.n(x[,1]) == 1))
    idx <- seq_along(nms <- .LAYERS)
    mapply(i=idx, n=nms, \(i, n) {
        # i=positive
        y <- x[i, ]
        expect_length(get(n)(y), length(get(n)(y)))
        lapply(setdiff(.LAYERS, n), \(.) expect_length(get(.)(y), 0))
        # i=negative
        y <- x[-i, ]
        expect_length(get(n)(y), 0)
        # i=character
        y <- x[setdiff(nms, n), ]
        expect_length(get(n)(y), 0)
    })
    # i=invalid
    expect_error(x[100,])
    expect_error(x[100,])
    expect_error(x[".",])
    # multiple
    y <- x[i <- c(1,3), ]
    n <- .n(x); n[-i] <- 0
    expect_identical(.n(y), n)
    y <- x[c(1, 2), c(1, 2)]
    . <- imageNames(x)
    expect_identical(imageNames(y), .[1])
    expect_identical(labelNames(y), labelNames(x)[2])
    expect_error(x[c(1, 2), list(1, 9)]) # any out of bounds
    expect_error(x[c(1, 2), c(1, 2, 3)]) # mismatching length
    # vector or list handling for simple indexing
    expect_identical(x[c(1, 2), c(1, 1)], x[c(1, 2), list(1, 1)])
    expect_equivalent(.n(x[c(1, 2), list(1, c(1, 2))]), c(1, 2, 0, 0, 0))
    # j=negative
    expect_identical(imageNames(x[1,-100]), .)
    expect_identical(imageNames(x[1,-1]), .[-1])
    # j=missing
    expect_identical(imageNames(x[1,]), .)
    # j=character
    expect_identical(imageNames(`[`(x, 1, .)), .)
    expect_identical(imageNames(`[`(x, 1, .[2])), .[2])
    expect_error(x[1,"."]); expect_error(x[1,c(".", .)])
    # length 'j' > 'i'
    y <- x[1, c(1,2)]
    expect_length(labels(y), 0)
    expect_length(images(y), length(.))
    expect_identical(imageNames(y), .)
    # length 'i' > 'j'
    y <- x[c(1,2), 1]
    expect_length(shapes(y), 0)
    expect_identical(images(y), images(x)[1])
    expect_identical(labels(y), labels(x)[1])
})
