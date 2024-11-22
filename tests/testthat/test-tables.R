require(SingleCellExperiment, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, table=1, anndataR=TRUE)
table(x)$region <- as.character(table(x)$region)
i <- (md <- int_metadata(table(x))$spatialdata_attrs)$region

test_that("hasTable()", {
    # TRUE
    i <- md$region
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
    expect_identical(t, table(x))
    # 'drop' argument
    expect_error(getTable(x, i, 123))
    expect_error(getTable(x, i, "."))
    expect_error(getTable(x, i, c(TRUE, FALSE)))
    # later 'region' of a couple random observations
    s <- t; s$region[. <- sample(ncol(s), 2)] <- "."; table(x) <- s
    t1 <- getTable(x, i, drop=FALSE)
    t2 <- getTable(x, i, drop=TRUE)
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
    expect_equal(nrow(table(y, 2)), 0)
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
                md <- meta(y)$spatialdata_attrs
                ik <- md$instance_key
                n <- length(unique(pull(data(y), ik)))
            })
        df <- data.frame(foo=runif(n))
        expect_silent(y <- setTable(x, i, df))
        expect_length(tables(y), 2)
        expect_true(hasTable(y, i))
        t <- getTable(y, i)
        expect_identical(t$foo, df$foo)
        md <- int_metadata(t)$spatialdata_attrs
        expect_identical(md$region, i)
        # dots = list of functions
        f <- list(
            numbers=\(n) runif(n),
            letters=\(n) sample(letters, n, TRUE))
        expect_silent(y <- setTable(x, i, f))
        expect_length(tables(y), 2)
        expect_true(hasTable(y, i))
        t <- getTable(y, i)
        expect_true(all(names(f) %in% names(colData(t))))
        md <- int_metadata(t)$spatialdata_attrs
        expect_identical(md$region, i)
    }
})

test_that("valTable()", {
    t <- getTable(x, i)
    # invalid
    expect_error(valTable(x, i, "."))
    expect_error(valTable(x, i, 123))
    expect_error(valTable(x, i, sample(rownames(t), 2)))
    expect_error(valTable(x, i, sample(names(colData(t)), 2)))
    # 'colData'
    j <- sample(names(colData(t)), 1)
    v <- valTable(x, i, j)
    expect_identical(v, t[[j]])
    # 'assay' data
    j <- sample(rownames(t), 1)
    v <- valTable(x, i, j)
    expect_identical(v, assay(t)[j, ])
    # 'assay' argument
    assay(t, ".") <- 1+assay(t); table(x) <- t
    v <- valTable(x, i, j, assay=".")
    expect_identical(v, assay(t, ".")[j, ])
    expect_error(valTable(x, i, rownames(t)[1], assay=".."))
})