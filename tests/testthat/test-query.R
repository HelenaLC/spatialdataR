library(sf)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("query,...", {
    # extract 1st element from every layer
    lys <- lapply(seq_len(4), \(.) x[.,1][[.]][[1]])
    for (y in lys) {
        # missing bounding box coordinates
        expect_error(query(y, xmin=0, xmax=1, ymin=0))
        # # invalid coordinate space
        # expect_error(query(y, ".", xmin=0, xmax=1, ymin=0, ymax=1))
        # expect_error(query(y, 100, xmin=0, xmax=1, ymin=0, ymax=1))
    }
})

test_that("query,ImageArray", {
    d <- dim(i <- image(x))
    # neither crop nor shift
    expect_identical(query(i, xmin=0, xmax=d[3], ymin=0, ymax=d[2]), i)
    # order is irrelevant
    expect_identical(query(i, ymax=d[2], xmax=d[3], xmin=0, ymin=0), i)
    # crop but don't shift
    j <- query(i, xmin=0, xmax=w <- d[3]/2, ymin=0, ymax=h <- d[2]/4)
    expect_equal(dim(j), c(3, h, w)) 
    expect_identical(CTdata(i), CTdata(j))
    # crop and shift
    j <- query(i, xmin=1, xmax=w <- d[3]/2, ymin=2, ymax=h <- d[2]/4)
    expect_equal(dim(j), c(3, 1+h-2, 1+w-1))
    expect_equal(CTtype(j), t <- "translation")
    expect_equivalent(CTdata(j)[[t]][[1]], c(0, 2, 1))
})

test_that("query,PointFrame", {
    n <- length(p <- point(x))
    # this shouldn't do anything
    q <- query(p, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    expect_is(data(q), "arrow_dplyr_query")
    expect_identical(collect(data(p)), collect(data(q)))
    # this should drop everything
    q <- query(p, xmin=Inf, xmax=-Inf, ymin=Inf, ymax=-Inf)
    expect_equal(nrow(collect(data(q))), 0)
    # proper query
    bb <- lapply(c("x", "y"), \(.) {
        v <- collect(data(p))[[.]]
        d <- c((d <- diff(range(v)))/4, d/2)
        names(d) <- paste0(., c("min", "max"))
        as.list(d) }) |> Reduce(f=c)
    q <- do.call(query, c(list(x=p), bb))
    df <- collect(data(p))
    fd <- collect(data(q))
    i <- df$x >= bb$xmin & df$x <= bb$xmax & 
        df$y >= bb$ymin & df$y <= bb$ymax
    expect_identical(df[i, ], fd)
})

test_that("query,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock query without any effect
    t <- query(s, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    expect_equal(collect(data(s)), collect(data(t)))
    # this should drop everything
    t <- query(s, xmin=Inf, xmax=-Inf, ymin=Inf, ymax=-Inf)
    expect_equal(nrow(collect(data(t))), 0)
    # proper query
    xy <- st_coordinates(st_as_sf(data(s)))
    xy <- data.frame(xy); names(xy) <- c("x", "y")
    xy <- xy[i <- sample(nrow(xy), 1), ]
    bb <- lapply(xy, \(.) c(.-1e-9, .+1e-9))
    bb <- data.frame(t(unlist(bb)))
    names(bb) <- c("xmin", "xmax", "ymin", "ymax")
    t <- do.call(query, c(list(x=s), bb))
    expect_equal(s[i], t)
})
