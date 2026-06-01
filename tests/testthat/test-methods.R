library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x)

fun <- c("image", "label", "shape", "point", "table")
nms <- c("blobs_image", "blobs_labels", "blobs_circles", "blobs_points", "table")
typ <- c("SpatialDataImage", "SpatialDataLabel", "SpatialDataShape", "SpatialDataPoint", "SingleCellExperiment")

# get ----

test_that("get all", {
    for (. in .LAYERS) {
        y <- slot(x, .)
        expect_identical(x[[.]], y)
        expect_identical(x[[.LAYERS[.]]], y)
    }
    for (f in paste0(fun, "s"))
        expect_is(get(f)(x), "SimpleList")
    expect_error(x[[0]])
    expect_error(x[[7]])
    expect_error(x[["x"]])
})

test_that("get one", {
    env <- asNamespace("spatialdataR")
    # i=numeric
    mapply(f=fun, t=typ, \(f, t)
        expect_is(get(f, envir=env)(x, i=1), t))
    # i=character
    mapply(f=fun, t=typ, n=nms, \(f, t, n)
        expect_is(get(f, envir=env)(x, i=n), t))
    # i=invalid
    for (f in fun) {
        expect_error(get(f, envir=env)(x, 0))
        expect_error(get(f, envir=env)(x, "."))
        expect_error(get(f, envir=env)(x, c(1,1)))
        y <- get(paste0(f, "s<-"))(x, list())
        expect_null(get(f, envir=env)(y, 1))
    }
})

test_that("layer()", {
    ok <- unlist(colnames(x))
    # invalid
    expect_error(layer(x, 0))
    expect_error(layer(x, 9))
    expect_error(layer(x, "."))
    expect_error(layer(x, TRUE))
    expect_error(layer(x, sample(ok, 2)))
    # valid
    replicate(5, {
        i <- sample(ok, 1)
        y <- layer(x, i)
        expect_length(y, 1)
        expect_is(y, "character")
        expect_in(y, rownames(x))
    })
})

test_that("element()", {
    # invalid
    expect_error(element(x, 99))
    expect_error(element(x, "."))
    expect_error(element(x, TRUE))
    # valid
    expect_silent(a <- element(x))
    expect_silent(b <- element(x, 1))
    expect_identical(a, b)
    replicate(5, {
        i <- sample(.LAYERS, 1)
        j <- sample(names(slot(x, i)), 1)
        expect_identical(x[[i]][[j]], element(x, j))
    })
})

test_that("element<-()", {
    i <- vapply(colnames(x), \(.) .[1], character(1))
    for (. in i) {
        # clear
        y <- x; element(y, .) <- NULL
        expect_error(element(y, .))
        # valid
        y <- x; element(y, .) <- element(x, .)
        expect_identical(element(y, .), element(x, .))
        # invalid
        es <- colnames(x)
        ex <- unlist(es[layer(x, .)])
        es <- setdiff(unlist(es), ex)
        el <- element(x, sample(es, 1))
        expect_error(element(x, .) <- el)
    }
})

# set ----

obj <- list(
    images=SpatialDataImage(), 
    labels=SpatialDataLabel(),
    shapes=SpatialDataShape(), 
    points=SpatialDataPoint(),
    tables=SingleCellExperiment())

test_that("set all", {
    for (. in .LAYERS) {
        # invalid
        y <- x; expect_error(y[[.]] <- "ao")
        y <- x; expect_error(y[[.]] <- 7777)
        y <- x; expect_error(y[[.]] <- TRUE)
        y <- x; expect_error(y[[987]] <- list())
        y <- x; expect_error(y[["x"]] <- list())
        # clear
        y <- x; y[[.]] <- NULL
        expect_length(y[[.]], 0)
        y <- x; y[[.]] <- list()
        expect_length(y[[.]], 0)
        # character
        y[[.]] <- list(obj[[.]])
        expect_length(y[[.]], 1)
        expect_identical(y[[.]][[1]], obj[[.]])
        # index
        z <- x; z[[match(., .LAYERS)]] <- y[[.]]
        expect_identical(z[[.]], y[[.]])
        # element
        y[[.]][[2]] <- obj[[.]]
        expect_length(y[[.]], 2)
        expect_identical(y[[.]][[2]], obj[[.]])
        # auto-naming to layer+index
        y <- x; y[[.]] <- list(obj[[.]]) # all unnamed
        expect_named(y[[.]])
        expect_length(y[[.]], 1)
        expect_is(y[[.]], "SimpleList")
        expect_identical(names(y[[.]]), gsub("s$", "1", .))
        y <- x; y[[.]] <- list(a=obj[[.]], obj[[.]], b=obj[[.]]) # one unnamed
        expect_named(y[[.]])
        expect_length(y[[.]], 3)
        expect_is(y[[.]], "SimpleList")
        expect_identical(names(y[[.]]), c("a", gsub("s$", "2", .), "b"))
    }
})

test_that("set one", {
    # value=NULL
    for (f in fun) {
        y <- x
        n <- length(get(paste0(f, "s"))(y))
        y <- get(paste0(f, "<-"))(y, i=1, value=NULL)
        m <- length(get(paste0(f, "s"))(y))
        expect_true(m == (n-1))
    }
    # value=in/valid
    mapply(f=fun, o=obj, t=typ, \(f, o, t) {
        all <- get(paste0(f, "s"))
        set <- get(paste0(f, "<-"))
        nms <- get(paste0(f, "Names"))
        n <- \(.) length(all(.))
        # character
        y <- set(x, i=".", value=o)
        expect_true("." %in% nms(y))
        expect_is(get(f)(y, "."), t)
        # numeric
        y <- set(x, i=1, value=o)
        expect_is(get(f)(y, 1), t)
        # when index > number of elements,
        # element name becomes layer+index
        y <- set(x, i=n(x)+1, value=o)
        i <- paste0(f, n(x)+1)
        expect_true(i %in% nms(y))
        expect_length(all(y), n(x)+1)
        expect_identical(element(y, i), o)
        z <- set(x, i=n(x)+2, value=o)
        expect_identical(nms(y), nms(z))
        # missing
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

test_that("set nms", {
    expect_error(imageNames(x)[1] <- "")
    expect_error(imageNames(x) <- rep("x", length(images(x))))
    y <- x; val <- letters[seq_along(images(x))]
    expect_silent(imageNames(y) <- val)
    expect_identical(imageNames(y), val)
    y <- x
    r <- region(table(x))
    labels(y) <- labels(y)[r]
    labelNames(y) <- "x"
    r <- region(table(y))
    expect_identical(r, "x")
})

# $ ----

test_that("$", {
    mapply(i=paste0(fun, "s"), n=nms, t=typ, \(i, n, t) {
        # object-wide
        f <- parse(text=sprintf("x$%s", i))
        expect_is(y <- eval(f), "SimpleList")
        # element-wise
        expect_is(names(y), "character")
        expect_length(names(y), length(y))
        f <- parse(text=sprintf("y$%s", n))
        expect_is(eval(f), t)
    })
})

# sub ----

test_that("[,sdShape/Point", {
    y <- shape(x)
    expect_error(y[-1,])
    # one index subsets in vector-like fashion
    expect_equal(dim(y[1]), c(1, ncol(y)))
    # two indices subset in array-like fashion
    expect_equal(nrow(y[1,]), 1) # no j
    expect_equal(ncol(y[,1]), 1) # no i
    expect_equal(dim(y[1,1]), c(1,1)) # both
    expect_identical(dim(y[,]), dim(y)) # none

    y <- point(x)
    expect_error(y[-1,])
    # one index subsets in vector-like fashion
    expect_equal(dim(y[1]), c(1, ncol(y)))
    # two indices subset in array-like fashion
    expect_equal(nrow(y[1,]), 1) # no j
    expect_equal(ncol(y[,1]), 2) # no i (preserve geometry)
    expect_equal(dim(y[1,1]), c(1,2)) # both
    expect_identical(dim(y[,]), dim(y)) # none
})

test_that("[,sdLabel", {
    y <- label(x)
    # logical
    expect_identical(y[TRUE,TRUE], y)
    expect_equal(dim(y[FALSE,FALSE]), c(0,0))
    expect_equal(dim(y[FALSE,TRUE]), c(0,ncol(y)))
    expect_equal(dim(y[TRUE,FALSE]), c(nrow(y),0))
    # numeric
    expect_identical(y[,], y) # none
    expect_equal(nrow(y[1,]), 1) # no j
    expect_equal(ncol(y[,1]), 1) # no i
    expect_equal(dim(y[1,1]), c(1,1)) # both
    # TODO: multiscales
})

test_that("[,sdImage", {
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
    for (l in rownames(x)) {
        e <- names(x[[l]])
        expect_no_error(y <- x[l, e[1]])
    }
    # invalid
    expect_error(x[9,1])
    expect_error(x[1,9])
    expect_error(x[1,"x"])
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
    expect_length(y[[1]], n[1])
    expect_true(all(n[-1] == 0))
    # negative 'j'
    n <- .n(y <- x[,-1])
    expect_equal(n, .n(x)-1)
    # infinite 'j'
    expect_no_error(y <- x[1, Inf])
})
