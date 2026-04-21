require(SingleCellExperiment, quietly=TRUE)
oo <- options()$arrow.pull_as_vector
options(arrow.pull_as_vector=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, anndataR=TRUE)

se <- SpatialData::table(x)
md <- int_metadata(se)
md <- md$spatialdata_attrs
i <- md[[rk <- md$region_key]]

test_that("hasTable()", {
    # TRUE
    i <- region(table(x))
    expect_true(hasTable(x, i))
    # FALSE
    j <- setdiff(unlist(colnames(x)), c(i, tableNames(x)))
    expect_true(all(!vapply(j, hasTable, x=x, logical(1))))
    # 'name' argument
    expect_error(hasTable(x, i, 123))
    expect_error(hasTable(x, i, "."))
    expect_error(hasTable(x, i, c(TRUE, FALSE)))
    expect_identical(hasTable(x, i, name=TRUE), tableNames(x))
    # invalid
    expect_error(hasTable(x, 123))
    expect_error(hasTable(x, "."))
    expect_error(hasTable(x, character(2)))
    expect_error(hasTable(x, sample(j, 1), name=TRUE)) # none
    expect_error(hasTable(setTable(x, i), i, name=TRUE)) # many
})

test_that("getTable()", {
    # invalid
    expect_error(getTable(x, 123))
    expect_error(getTable(x, "."))
    expect_error(getTable(x, character(2)))
    # valid
    expect_silent(t <- getTable(x, i))
    expect_identical(t, SpatialData::table(x))
    # 'drop' argument
    expect_error(getTable(x, i, 123))
    expect_error(getTable(x, i, "."))
    expect_error(getTable(x, i, c(TRUE, FALSE)))
    # alter 'region' of a couple random observations
    s <- t; y <- x
    int_colData(s)[[rk]] <- paste(int_colData(s)[[rk]])
    int_colData(s)[[rk]][. <- sample(ncol(s), 2)] <- "."
    SpatialData::table(y) <- s
    # these should be gone when 'drop=TRUE'
    t1 <- getTable(y, i, drop=FALSE)
    t2 <- getTable(y, i, drop=TRUE)
    expect_identical(t1, s)
    expect_identical(t2, s[, -.])
})

test_that("setTable(),labels", {
    # invalid 'i'
    expect_error(setTable(x, 123))
    expect_error(setTable(x, "."))
    expect_error(setTable(x, character(2)))
    # 'name' that already exists fails
    expect_error(setTable(x, i, name=tableNames(x)))
    # valid w/o dots
    y <- setTable(x, i)
    expect_length(tables(y), 2)
    expect_equal(nrow(SpatialData::table(y, 2)), 0)
    # invalid dots
    . <- list(1, \(n) runif(n))
    expect_error(setTable(x, i, .))
    expect_error(setTable(x, i, 1))
    # invalid 'data.frame'
    df <- data.frame(foo=runif(37))
    expect_error(setTable(x, i, df))
})

test_that("setTable(),points/shapes", {
    for (. in c("point", "shape")) {
        nms <- paste0(., "Names")
        i <- get(nms)(x)[1]
        y <- get(.)(x)
        # dots = valid 'data.frame'
        n <- switch(.,
            shape=length(y),
            point={
                ik <- instance_key(y)
                n <- length(unique(pull(data(y), ik)))
            })
        df <- data.frame(foo=runif(n))
        expect_silent(y <- setTable(x, i, df))
        expect_length(tables(y), 2)
        expect_true(hasTable(y, i))
        t <- getTable(y, i)
        expect_identical(t$foo, df$foo)
        #expect_identical(region(t), i)
        # dots = list of functions
        f <- list(
            numbers=\(n) runif(n),
            letters=\(n) sample(letters, n, TRUE))
        expect_silent(y <- setTable(x, i, f))
        expect_length(tables(y), 2)
        expect_true(hasTable(y, i))
        t <- getTable(y, i)
        expect_true(all(names(f) %in% names(colData(t))))
        #expect_identical(region(t), i)
    }
})

test_that("valTable()", {
    n <- ncol(t <- getTable(x, i))
    # invalid
    expect_error(valTable(x, i, "."))
    expect_error(valTable(x, i, 123))
    expect_error(valTable(x, i, sample(rownames(t), 2)))
    expect_error(valTable(x, i, sample(names(colData(t)), 2)))
    # 'colData'
    cd <- DataFrame(a=sample(letters, n), b=runif(n))
    s <- t; colData(s) <- cd
    y <- x; SpatialData::table(y) <- s
    expect_identical(valTable(y, i, j <- "a"), s[[j]])
    expect_identical(valTable(y, i, j <- "b"), s[[j]])
    expect_error(valTable(y, i, "c"))
    # 'assay' data
    j <- sample(rownames(t), 1)
    v <- valTable(x, i, j)
    expect_identical(v, assay(t)[j, ])
    # 'assay' argument
    assay(t, ".") <- 1+assay(t); SpatialData::table(x) <- t
    v <- valTable(x, i, j, assay=".")
    expect_identical(v, assay(t, ".")[j, ])
    expect_error(valTable(x, i, rownames(t)[1], assay=".."))
})

options(arrow.pull_as_vector=oo) # reset
