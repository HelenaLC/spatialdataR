require(sf, quietly=TRUE)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, anndataR=TRUE)

test_that("query,.check_box", {
    # valid
    q <- list(
        list(xmin=0, xmax=1, ymin=0, ymax=1),
        list(xmin=-1, xmax=0, ymin=-1, ymax=0),
        list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
    for (. in q) expect_silent(.check_box(.))
    # invalid
    q <- list(
        list(xmin=0, xmax=1, ymin=0), 
        list(xmin=1, xmax=0, ymin=1, ymax=0),
        list(xmin=0, xmax=-1, ymin=0, ymax=-1),
        list(xmin=0, xmax=1, ymin=10, ymax=NA),
        list(xmin=Inf, xmax=-Inf, ymin=Inf, ymax=-Inf))
    for (. in q) expect_error(.check_box(.))
})

test_that("query,.check_pol", {
    # valid
    q <- list(
        m <- matrix(seq_len(8), 4, 2), 
        rbind(c(1,1), c(2,2), c(3,3)), # open
        rbind(c(1,1), c(2,2), c(3,3), c(1,1))) 
    for (. in q) expect_silent(.check_pol(.))
    # invalid
    q <- list(
        matrix(seq_len(6), 2, 3), # wrong dim.
        matrix(numeric(6), 3, 2), # duplicates
        `[<-`(m, i=1, j=1, value=Inf), # not finite
        `[<-`(m, i=1, j=1, value=NA))  # missing value
    for (. in q) expect_error(.check_pol(.))
})

test_that("query,ImageArray", {
    d <- dim(i <- image(x))
    # query equals dimensions
    y <- list(xmin=0, xmax=d[3], ymin=0, ymax=d[2])
    expect_identical(query(i, y), i)
    # order is irrelevant
    y <- list(ymax=d[2], xmax=d[3], xmin=0, ymin=0)
    expect_identical(query(i, y), i)
    # crop but don't shift
    y <- list(xmin=0, xmax=w <- d[3]/2, ymin=0, ymax=h <- d[2]/4)
    expect_equal(dim(j <- query(i, y)), c(3, h, w)) 
    expect_identical(CTlist(i), CTlist(j))
    # crop and shift
    y <- list(
        xmin=dx <- 3, xmax=w <- d[3]/2, 
        ymin=dy <- 5, ymax=h <- d[2]/4)
    expect_equal(dim(query(i, y)), c(3, 1+h-dy, 1+w-dx))
    # non-finite boundaries
    y <- list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    expect_silent(query(i, y))
})

test_that("query,LabelArray", {
    d <- dim(l <- label(x))
    # query equals dimensions
    y <- list(xmin=0, xmax=d[2], ymin=0, ymax=d[1])
    expect_identical(query(l, y), l)
    # order is irrelevant
    y <- list(ymax=d[1], xmax=d[2], xmin=0, ymin=0)
    expect_identical(query(l, y), l)
    # crop but don't shift
    y <- list(xmin=0, xmax=w <- d[2]/2, ymin=0, ymax=h <- d[1]/4)
    expect_equal(dim(m <- query(l, y)), c(h, w)) 
    expect_identical(CTlist(l), CTlist(m))
    # crop and shift
    y <- list(
        xmin=dx <- 3, xmax=w <- d[2]/2, 
        ymin=dy <- 5, ymax=h <- d[1]/4)
    expect_equal(dim(query(l, y)), c(1+h-dy, 1+w-dx))
    # non-finite boundaries
    y <- list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    expect_silent(query(l, y))
})

test_that("query-box,PointFrame", {
    n <- length(p <- point(x))
    # this shouldn't do anything
    q <- query(p, list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
    expect_is(data(q), "arrow_dplyr_query")
    expect_identical(collect(data(p)), collect(data(q)))
    # this should drop everything
    q <- query(p, list(xmin=0, xmax=1e-3, ymin=0, ymax=1e-3))
    expect_equal(nrow(collect(data(q))), 0)
    # proper query
    bb <- lapply(c("x", "y"), \(.) {
        v <- collect(data(p))[[.]]
        d <- c((d <- diff(range(v)))/4, d/2)
        names(d) <- paste0(., c("min", "max"))
        as.list(d) }) |> Reduce(f=c)
    q <- do.call(query, c(list(x=p), list(bb)))
    df <- collect(data(p))
    fd <- collect(data(q))
    i <- 
        df$x >= bb$xmin & df$x <= bb$xmax & 
        df$y >= bb$ymin & df$y <= bb$ymax
    expect_identical(df[i, ], fd)
})

test_that("query-pol,PointFrame", {
    n <- length(p <- point(x))
    f <- \(.) collect(data(.))
    # mock all-inclusive query
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_identical(f(query(p, xy)), f(p))
    # sample random points &
    # query tiny polygon around them
    replicate(5, {
        i <- sample(n, 1)
        xy <- c(p[i]$x, p[i]$y)
        i <- p$x == xy[1] & p$y == xy[2]
        xy <- rbind(
            xy+c(0, d <- 1e-6),
            xy+c(-d,-d), xy+c(+d,-d))
        q <- query(p, xy)
        expect_length(q, sum(i))
        expect_identical(f(q), f(p[which(i)]))
    })
})

test_that("query-box,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock query without any effect
    t <- query(s, list(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
    expect_equal(nrow(data(t)), nrow(data(s)))
    # this should drop everything
    t <- query(s, list(xmin=0, xmax=1e-3, ymin=0, ymax=1e-3))
    expect_equal(nrow(t), 0)
    # proper query
    xy <- st_coordinates(st_as_sf(data(s)))
    xy <- data.frame(xy); names(xy) <- c("x", "y")
    xy <- xy[i <- sample(nrow(xy), 1), ]
    bb <- lapply(xy, \(.) c(.-1e-9, .+1e-9))
    bb <- data.frame(t(unlist(bb)))
    names(bb) <- c("xmin", "xmax", "ymin", "ymax")
    t <- do.call(query, c(list(x=s), list(bb)))
    expect_equal(s[i], t)
})

test_that("query-pol,ShapeFrame", {
    n <- length(s <- shape(x))
    # mock all-inclusive query
    xy <- rbind(c(0,0), c(0,1e6), c(1e6,0))
    expect_equal(query(s, xy), s)
    # sample random shapes &
    # query tiny polygon around them
    xy <- st_coordinates(st_as_sf(data(s)))
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
