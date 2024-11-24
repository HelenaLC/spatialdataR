library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

fun <- c("image", "label", "shape", "point", "table")
nms <- c("blobs_image", "blobs_labels", "blobs_circles", "blobs_points", "table")
typ <- c("ImageArray", "LabelArray", "ShapeFrame", "PointFrame", "SingleCellExperiment")

# get ----

test_that("layer()", {
    # invalid
    expect_error(layer(x, 0))
    expect_error(layer(x, 9))
    expect_error(layer(x, "."))
    expect_error(layer(x, TRUE))
    expect_error(layer(x, .LAYERS))
    expect_silent(layer(x)) # missing
    # valid
    i <- sample(.LAYERS, 1)
    n <- length(attr(x, i))
    y <- layer(x, i)
    expect_is(y, "list")
    expect_length(y, n)
})

test_that("element()", {
    # invalid
    expect_error(element(x, 1, 0))
    expect_error(element(x, 1, 9))
    expect_error(element(x, 1, "."))
    expect_error(element(x, 1, TRUE))
    expect_error(element(x, 1, colnames(x)[[2]]))
    expect_silent(element(x, 1)) # missing
    # valid
    i <- sample(.LAYERS, 1)
    j <- sample(names(attr(x, i)), 1)
    expect_silent(element(x, i, j))
})

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
        expect_silent(y <- get(f)(x, Inf))
        set <- get(paste0(f, "s<-"))
        y <- set(x, list())
        expect_error(get(f)(y, 1))
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

test_that("[,ImageArray", {
    d <- \(x) {
        y <- data(x, NULL)
        vapply(y, dim, numeric(3))
    }
    i <- image(x, "blobs_image")
    # missing
    expect_identical(i[,,,], i)
    # invalid
    expect_error(i["",,])
    expect_error(i[,"",])
    expect_error(i[,,""])
    expect_error(i[4,,])
    expect_error(i[,c(1, 3),])
    expect_error(i[,,c(1, 3)])
    # one TRUE, two FALSE
    ijk <- matrix(FALSE, 3, 3)
    diag(ijk) <- TRUE
    lapply(seq_len(3), \(.) {
        ijk <- as.list(ijk[., ])
        j <- do.call(`[`, c(list(i), ijk))
        expect_identical(d(j)[.], d(i)[.])
    })
    # one FALSE, two TRUE
    ijk <- matrix(TRUE, 3, 3)
    diag(ijk) <- FALSE
    lapply(seq_len(3), \(.) {
        ijk <- as.list(ijk[., ])
        j <- do.call(`[`, c(list(i), ijk))
        expect_true(d(j)[.] == 0)
    })
    # multiscale
    i <- image(x, "blobs_multiscale_image")
    j <- seq_len(d(i)[2]/2)
    k <- seq_len(d(i)[2]/4)
    expect_equal(d(i[,j,k]), d(i)/c(1,2,4))
})

test_that("[,SpatialData", {
    # valid
    .n <- \(.) vapply(colnames(.), length, numeric(1))
    n <- .n(y <- x[i <- 4, j <- c(1, 3)])
    expect_true(n[i] == 2)
    expect_true(all(n[-i] == 0))
    expect_identical(
        colnames(y)[[i]], 
        colnames(x)[[i]][j])
    n <- .n(y <- x[c(1, 2), list(1, j <- c(1, 2))])
    expect_true(all(n[j] == c(1, 2)))
    expect_true(all(n[-j] == 0))
    # invalid
    expect_error(x[9,1])
    expect_error(x[1,9])
    # missing both
    expect_identical(x[,], x) 
    # missing 'i'
    expect_true(all(.n(x[,1]) == 1))
    # negative 'i'
    n <- .n(y <- x[-1,])
    expect_true(n[1] == 0)
    expect_true(all(n[-1] > 0))
    # missing 'j'
    n <- .n(y <- x[1,])
    expect_length(layer(y, 1), n[1])
    expect_true(all(n[-1] == 0))
    # negative 'j'
    n <- .n(y <- x[,-1])
    expect_equal(n, .n(x)-1)
    # infinite 'j'
    expect_silent(y <- x[1, Inf])
    expect_identical(
        element(y, 1, 1), 
        element(x, 1, .n(x)[1]))
})
