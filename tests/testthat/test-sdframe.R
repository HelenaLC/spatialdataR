require(sf, quietly=TRUE)
require(dplyr, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("new,sdPoint", {
    # 1. Empty construction
    expect_silent(p0 <- SpatialDataPoint())
    expect_s4_class(p0, "SpatialDataPoint")
    expect_equal(nrow(p0), 0)
    
    # 2. Construction from data.frame with x, y
    df <- data.frame(x=1:5, y=1:5, genes=letters[1:5], cell_id=1:5)
    expect_silent(p1 <- SpatialDataPoint(df))
    expect_equal(nrow(p1), 5)
    expect_true("geometry" %in% names(p1))
    
    # 3. Supplying ik and fk
    expect_silent(p2 <- SpatialDataPoint(df, ik="cell_id", fk="genes"))
    expect_equal(instance_key(p2), "cell_id")
    expect_equal(feature_key(p2), "genes")
    
    # 4. Geometry validation (must be POINT)
    # Use sf object to force non-POINT geometry
    poly <- st_sfc(st_polygon(list(matrix(c(0,1,1,0,0,0,0,1,1,0), ncol=2))))
    df_poly <- st_sf(data.frame(a=1), geometry=poly)
    expect_error(SpatialDataPoint(df_poly), "only 'POINT' geometries supported")
})

test_that("new,sdFrame", {
    # 1. Empty construction
    expect_silent(s0 <- SpatialDataShape())
    expect_s4_class(s0, "SpatialDataShape")
    expect_equal(nrow(s0), 0)
    
    # 2. Construction from data.frame with x, y (points)
    df_pts <- data.frame(x=1:5, y=1:5)
    expect_silent(s1 <- SpatialDataShape(df_pts))
    expect_equal(nrow(s1), 5)
    expect_equal(geom_type(s1), "POINT")
    
    # 3. Polygon creation from x, y, i
    df_poly <- data.frame(
        x = c(0, 1, 1, 0, 0,  2, 3, 3, 2, 2),
        y = c(0, 0, 1, 1, 0,  2, 2, 3, 3, 2),
        i = c(1, 1, 1, 1, 1,  2, 2, 2, 2, 2)
    )
    expect_silent(s2 <- SpatialDataShape(df_poly))
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

test_that("create, SpatialDataPoint", {
  
  # make point frame
  df <- example_points()
  pf <- SpatialDataPoint(df)
  expect_identical(st_coordinates(st_as_sf(data(pf))), 
                   {
                     dfm <- as.matrix(df)
                     colnames(dfm) <- c("X", "Y")
                     dfm
                   })
  expect_equal(dim(pf), c(100,1)) # geometry column of POINT
  expect_identical(names(pf), "geometry")

  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(points = list(test_points = pf))
  expect_identical(data(point(sd)), data(pf))
  expect_identical(point(sd), pf)
  expect_identical(point(sd, 1), pf)
})

test_that("create polygon, SpatialDataShape", {
  
  # make point frame
  df <- example_polygons()
  pf <- SpatialDataShape(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(ddbs_collect(df)))
  expect_identical(names(pf), colnames(df))
  expect_identical(ddbs_collect(data(pf[1:2,1])),
                   ddbs_collect(df)[1:2,1])
  
  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(shapes = list(test_shapes = pf))
  expect_identical(data(shape(sd)), data(pf))
  expect_identical(shape(sd), pf)
  expect_identical(shape(sd, 1), pf)
})

test_that("create circle, SpatialDataShape", {
  
  # make point frame
  df <- example_circles()
  pf <- SpatialDataShape(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(ddbs_collect(df)))
  expect_identical(names(pf), colnames(df))
  expect_identical(ddbs_collect(data(pf[1:2,1])),
                   ddbs_collect(df)[1:2,1])
  
  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(shapes = list(test_shapes = pf))
  expect_identical(data(shape(sd)), data(pf))
  expect_identical(shape(sd), pf)
  expect_identical(shape(sd, 1), pf)
})

z <- list(0.1, 0.2)

for (v in z) {
 
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write, SpatialDataPoint", {
    
    # make sd data
    df <- example_points()
    pf <- SpatialDataPoint(df, version = point(sdFormat(v)))
    sd <- SpatialData(points = list(test_points = pf))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    pf2 <- point(sd2)
    # attr(data(pf), "source_table") is not identical, obviously
    expect_equal(
      ddbs_collect(data(pf)),
      ddbs_collect(data(pf2))
    )
    expect_identical(st_coordinates(st_as_sf(data(pf))), 
                     st_coordinates(st_as_sf(data(pf2))))
    expect_identical(meta(pf),meta(pf2))
    expect_identical(names(pf), names(pf2))
  })
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write polygon, SpatialDataShape", {
    
    # make sd data
    df <- example_polygons()
    pf <- SpatialDataShape(df, version = shape(sdFormat(v)))
    sd <- SpatialData(shapes = list(test_shapes = pf))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    pf2 <- shape(sd2)
    # TODO: they are not identical, why ? 
    expect_equal(data(pf) |> collect(), 
                 data(pf2) |> collect())
    expect_identical(meta(pf),meta(pf2))
    expect_identical(names(pf), names(pf2))
    expect_identical(data(pf[1:2, 1]) |> collect(), 
                     data(pf2[1:2,1]) |> collect())
  })
  
  td <- tempdir()
  zarr.store <- "test.zarr"
  zarr.path <- file.path(td, zarr.store)
  unlink(zarr.path, recursive = TRUE)
  
  test_that("write circle, SpatialDataShape", {
    
    # make sd data
    df <- example_circles()
    pf <- SpatialDataShape(df, version = shape(sdFormat(v)))
    sd <- SpatialData(shapes = list(test_shapes = pf))
    
    # write to location
    zarr.path <- tempfile(fileext = ".zarr")
    writeSpatialData(sd, path = zarr.path, version = v)
    expect_true(dir.exists(zarr.path))
    
    # read back and compare
    sd2 <- readSpatialData(zarr.path)
    pf2 <- shape(sd2)
    # TODO: they are not identical, why ? 
    expect_equal(data(pf) |> collect(), 
                 data(pf2) |> collect())
    expect_identical(meta(pf),meta(pf2))
    expect_identical(names(pf), names(pf2))
    expect_identical(data(pf[1:2, 1]) |> collect(), 
                     data(pf2[1:2,1]) |> collect())
  })
  
}