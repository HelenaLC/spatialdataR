require(sf, quietly=TRUE)
require(dplyr, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="spatialdataR")
x <- readSpatialData(x, tables=FALSE)

test_that("new,sdPoint", {
    # empty construction
    expect_silent(p0 <- SpatialDataPoint())
    expect_s4_class(p0, "SpatialDataPoint")
    expect_equal(nrow(p0), 0)
    
    # construction from 'data.frame' with xy
    df <- data.frame(x=1:5, y=1:5, genes=letters[1:5], cell_id=1:5)
    expect_silent(p1 <- SpatialDataPoint(df))
    expect_equal(nrow(p1), 5)
    expect_true("geometry" %in% names(p1))
    
    # supplying 'ik' and 'fk' (instance/feature key)
    expect_silent(p2 <- SpatialDataPoint(df, ik="cell_id", fk="genes"))
    expect_equal(instance_key(p2), "cell_id")
    expect_equal(feature_key(p2), "genes")
    
    # geometry validation (must be POINT);
    # use sf object to force non-POINT geometry
    mx <- matrix(c(0,1,1,0,0,0,0,1,1,0), ncol=2)
    df <- st_sf(geometry=st_sfc(st_polygon(list(mx))))
    expect_error(SpatialDataPoint(df), "only 'POINT' geometries supported")
})

test_that("new,sdFrame", {
    # empty construction
    expect_silent(s0 <- SpatialDataShape())
    expect_s4_class(s0, "SpatialDataShape")
    expect_equal(nrow(s0), 0)
    
    # construction from 'data.frame' with xy (points)
    df <- data.frame(x=1:5, y=1:5)
    expect_silent(s1 <- SpatialDataShape(df))
    expect_equal(nrow(s1), 5)
    expect_equal(geom_type(s1), "POINT")
    
    # polygon creation from xyi
    df <- data.frame(
        x=c(0,1,1,0,0, 2,3,3,2,2),
        y=c(0,0,1,1,0, 2,2,3,3,2),
        i=c(1,1,1,1,1, 2,2,2,2,2))
    expect_silent(s2 <- SpatialDataShape(df))
    expect_equal(nrow(s2), 2)
    expect_equal(geom_type(s2), "POLYGON")
    expect_setequal(rownames(as.data.frame(s2)), c("1", "2"))
})

test_that("names", {
    y <- names(p <- point(x))
    expect_type(y, "character")
    expect_identical(y, colnames(data(p)))
})

test_that("$,[[", {
    for (. in list(point(x), shape(x))) {
        # names
        nms <- .DollarNames(.)
        expect_type(nms, "character")
        expect_length(nms, ncol(.))
        expect_identical(nms, colnames(data(.)))
        # valid
        lapply(seq_len(ncol(.)), \(i) {
            j <- names(.)[i]
            y <- do.call(`$`, list(., j))
            z <- pull(data(.), j)
            expect_identical(y, z)
            expect_identical(y, z <- do.call(`[[`, list(., i)))
            expect_identical(z, do.call(`[[`, list(., j)))
        })
        # invalid
        expect_error(.[[0]])
        expect_error(.[[ncol(.) + 1]])
    }
})

test_that("[", {
    for (a in list(point(x), shape(x))) {
        # rows
        n <- length(a)
        m <- sample(n, 1)
        l <- logical(n)
        l[sample(n, m)] <- TRUE
        for (i in list(2, c(1, 3), l)) {
            n <- if (is.numeric(i)) length(i) else sum(i)
            expect_length(b <- a[i], n)
            expect_identical(
                collect(data(b)),
                collect(data(a))[i, ])
        }
        # cols
        b <- a[1, j <- sample(names(a), 2)]
        j <- unique(c(j, "geometry"))
        expect_identical(names(b), j)
        # invalid
        expect_error(a[-1, ])
        expect_error(a[, -1])
        expect_error(collect(a[".", ]))
        expect_error(collect(a[, "."]))
    }
})

test_that("filter", {
    n <- length(p <- point(x))
    expect_length(filter(p), n)
    expect_length(filter(p, genes == "x"), 0)
    f <- \() filter(p, missing == 1)
    expect_error(show(f()))
})

test_that("select", {
    p <- point(x)
    replicate(3, {
        n <- sample(ncol(p), 1)
        i <- sample(names(p), n)
        y <- select(p, all_of(i))
        z <- data(p) |> select(all_of(i))
        expect_equal(collect(data(y)), collect(z))
    })
})

test_that("mutate", {
    p <- point(x)
    q <- mutate(p, 
        foo=row_number(), 
        bar=toupper(genes))
    expect_equal(q$foo, seq_along(p))
    expect_equal(q$bar, toupper(p$genes))
})

test_that("as.data.frame", {
    y <- as.data.frame(p <- point(x))
    expect_s3_class(y, "data.frame")
    expect_equal(dim(y), dim(p))
    expect_equal(names(y), names(p))
    expect_identical(y, as.data.frame(collect(data(p))))
})
