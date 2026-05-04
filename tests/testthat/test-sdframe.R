require(sf, quietly=TRUE)
require(dplyr, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("new,PointFrame", {
    # 1. Empty construction
    expect_silent(p0 <- PointFrame())
    expect_s4_class(p0, "PointFrame")
    expect_equal(nrow(p0), 0)
    
    # 2. Construction from data.frame with x, y
    df <- data.frame(x=1:5, y=1:5, genes=letters[1:5], cell_id=1:5)
    expect_silent(p1 <- PointFrame(df))
    expect_equal(nrow(p1), 5)
    expect_true("geometry" %in% names(p1))
    
    # 3. Supplying ik and fk
    expect_silent(p2 <- PointFrame(df, ik="cell_id", fk="genes"))
    expect_equal(instance_key(p2), "cell_id")
    expect_equal(feature_key(p2), "genes")
    
    # 4. Geometry validation (must be POINT)
    # Use sf object to force non-POINT geometry
    poly <- st_sfc(st_polygon(list(matrix(c(0,1,1,0,0,0,0,1,1,0), ncol=2))))
    df_poly <- st_sf(data.frame(a=1), geometry=poly)
    expect_error(PointFrame(df_poly), "only 'POINT' geometries supported")
})

test_that("new,ShapeFrame", {
    # 1. Empty construction
    expect_silent(s0 <- ShapeFrame())
    expect_s4_class(s0, "ShapeFrame")
    expect_equal(nrow(s0), 0)
    
    # 2. Construction from data.frame with x, y (points)
    df_pts <- data.frame(x=1:5, y=1:5)
    expect_silent(s1 <- ShapeFrame(df_pts))
    expect_equal(nrow(s1), 5)
    expect_equal(geom_type(s1), "POINT")
    
    # 3. Polygon creation from x, y, i
    df_poly <- data.frame(
        x = c(0, 1, 1, 0, 0,  2, 3, 3, 2, 2),
        y = c(0, 0, 1, 1, 0,  2, 2, 3, 3, 2),
        i = c(1, 1, 1, 1, 1,  2, 2, 2, 2, 2)
    )
    expect_silent(s2 <- ShapeFrame(df_poly))
    expect_equal(nrow(s2), 2)
    expect_equal(geom_type(s2), "POLYGON")
    expect_setequal(rownames(as.data.frame(s2)), c("1", "2"))
})

test_that("names", {
    y <- names(p <- point(x))
    expect_is(y, "character")
    expect_identical(y, colnames(data(p)))
})

test_that("$,[[", {
    # names
    nms <- .DollarNames(p <- point(x))
    expect_is(nms, "character")
    expect_length(nms, ncol(p))
    expect_identical(nms, colnames(data(p)))
    # valid
    lapply(seq_len(ncol(p)), \(i) {
        j <- names(p)[i]
        y <- do.call(`$`, list(p, j))
        z <- pull(data(p), j)
        expect_identical(y, z)
        expect_identical(y, z <- do.call(`[[`, list(p, i)))
        expect_identical(z, do.call(`[[`, list(p, j)))
    })
    # invalid
    expect_error(p[[0]])
    expect_error(p[[ncol(p) + 1]])
})

test_that("filter", {
    n <- length(p <- point(x))
    expect_length(filter(p), n)
    expect_length(filter(p, genes == "x"), 0)
    f <- \() filter(p, z == 1)
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

test_that("as.data.frame", {
    y <- as.data.frame(p <- point(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), dim(p))
    expect_equal(names(y), names(p))
    expect_identical(y, as.data.frame(collect(data(p))))
})
