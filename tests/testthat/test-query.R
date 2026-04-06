require(sf, quietly=TRUE)
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
    expect_identical(CTlist(i), CTlist(j))
    # crop and shift
    j <- query(i, xmin=1, xmax=w <- d[3]/2, ymin=2, ymax=h <- d[2]/4)
    expect_equal(dim(j), c(3, 1+h-2, 1+w-1))
})

test_that("query-box,PointFrame", {
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

test_that("query-pol,PointFrame", {
    n <- length(p <- point(x))
    # sample random points &
    # query tiny polygon around them
    replicate(5, {
        i <- sample(n, 1)
        xy <- c(p[i]$x, p[i]$y)
        xy <- rbind(
            xy+c(0, d <- 1e-6),
            xy+c(-d,-d), xy+c(+d,-d))
        q <- query(p, xy)
        expect_length(q, 1)
        expect_equal(q, p[i])
    })
})

test_that("query-box,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock query without any effect
    t <- query(s, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    expect_equal(nrow(data(t)), nrow(data(s)))
    # this should drop everything
    t <- query(s, xmin=0, xmax=1e-3, ymin=0, ymax=1e-3)
    expect_equal(nrow(t), 0)
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

test_that("query-pol,ShapeFrame", {
    n <- length(s <- shape(x))
    xy <- st_coordinates(st_as_sf(data(s)))
    # sample random shapes &
    # query tiny polygon around them
    replicate(5, {
        i <- sample(n, 1)
        xy <- xy[i, ]
        xy <- rbind(
            xy+c(0, d <- 1e-6),
            xy+c(-d,-d), xy+c(+d,-d))
        t <- query(s, xy)
        expect_length(t, 1)
        expect_equal(t, s[i])
    })
})
