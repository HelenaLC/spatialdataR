require(SingleCellExperiment, quietly=TRUE)
zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

test_that(".sync_tables_on_crop,label", {
    i <- region(table(sd))
    e <- element(sd, i)
    
    # crop around one label
    id <- sample(is <- instances(e), 1)
    mx <- as(data(e), "dgCMatrix")
    ij <- (nz <- summary(mx))[nz$x == id, ]
    dx <- range(ij[, 2]); dy <- range(ij[, 1])
    bb <- list(xmin=dx[1], xmax=dx[2], ymin=dy[1], ymax=dy[2])
    x <- crop(sd, bb)
    
    # verify table integrity
    se <- getTable(x, i)
    mx <- data(element(x, i))
    ni <- length(is <- unique(mx[mx != 0]))

    expect_equal(ncol(se), ni)      # right instances
    expect_identical(region(se), i) # regions untouched
})

test_that(".sync_tables_on_crop,shape", {
    i <- shapeNames(sd)[1]
    e <- element(sd, i)
    
    # mock shape annotation
    mx <- matrix(nc=length(e))
    se <- SingleCellExperiment(list(mx))
    sd <- setTable(sd, i, se)
    
    # crop around one shape
    id <- sample(length(e), 1)
    xy <- centroids(e)[id, ]
    dx <- xy[[1]]+c(-(. <- 1e-3), .); dy <- xy[[2]]+c(-., .)
    bb <- list(xmin=dx[1], xmax=dx[2], ymin=dy[1], ymax=dy[2])
    x <- crop(sd, bb)
    
    # verify table integrity
    se <- getTable(x, i)
    expect_equal(ncol(se), 1)       # right instances
    expect_identical(region(se), i) # regions untouched
})

test_that(".sync_shapes_on_drop", {
    i <- shapeNames(sd)[1]

    # give shapes ids from 0, in a column
    # named after the table's instance key
    e <- mutate(element(sd, i), instance_id=row_number()-1)
    n <- length(id <- e$instance_id)
    shape(sd, i) <- e

    # mock shape annotation; unlike 'setTable()',
    # 'table<-' doesn't set the shapes' key
    se <- table(sd)[, seq_len(n)]
    regions(se) <- i
    instances(se) <- id

    # all shapes are kept
    x <- sd; table(x, "se") <- se
    expect_identical(element(x, i)$instance_id, id)

    # dropping an observation drops its shape
    table(x, "se") <- se[, -1]
    expect_identical(element(x, i)$instance_id, id[-1])

    # a table numbered by row position keeps
    # all shapes, although they have an 'instance_id'
    instances(se) <- seq_len(n)
    x <- sd; table(x, "se") <- se
    expect_identical(element(x, i)$instance_id, id)

    # shapes with their own instance key are
    # matched on it, not on 'instance_id'
    e <- mutate(e, a=instance_id+100)
    instance_key(meta(e)) <- "a"
    shape(sd, i) <- e
    instances(se) <- e$a
    x <- sd; table(x, "se") <- se[, 1:2]
    expect_identical(element(x, i)$a, e$a[1:2])
})

test_that("shapes get their instance key from the index of the .parquet", {
    i <- shapeNames(sd)[1]
    pa <- file.path(zs, "shapes", i)

    # a range index isn't stored as a column
    expect_null(instance_key(readShape(pa)))

    # mock an element written from a GeoDataFrame with
    # index 'id', which isn't the table's instance key
    dir.create(td <- tempfile())
    file.copy(pa, td, recursive=TRUE)
    pq <- file.path(c(pa, file.path(td, i)), "shapes.parquet")
    DBI::dbExecute(.conn(), sprintf(paste(
        "COPY (SELECT *, 10*row_number() OVER () AS id",
        "FROM read_parquet('%s')) TO '%s' (FORMAT parquet,",
        "KV_METADATA {pandas: '{\"index_columns\": [\"id\"]}'})"),
        pq[1], pq[2]))
    e <- readShape(file.path(td, i))
    id <- 10*seq_along(e)
    expect_identical(instance_key(e), "id")
    expect_equal(instances(e), id)

    # mock shape annotation
    se <- table(sd)[, seq_along(id)]
    regions(se) <- i
    instances(se) <- id
    x <- sd; shape(x, i) <- e

    # all shapes are kept
    table(x, "se") <- se
    expect_equal(instances(element(x, i)), id)
    expect_equal(instances(getTable(x, i)), id)

    # dropping observations drops their shapes
    k <- c(2, 4)
    y <- x; table(y, "se") <- se[, k]
    expect_equal(instances(element(y, i)), id[k])
    y <- query(x, i="se", instance_id %in% id[k])
    expect_equal(instances(element(y, i)), id[k])

    # cropping around one shape keeps its observation
    k <- sample(length(id), 1)
    xy <- centroids(e)[k, ]
    dx <- xy[[1]]+c(-(. <- 1e-3), .); dy <- xy[[2]]+c(-., .)
    bb <- list(xmin=dx[1], xmax=dx[2], ymin=dy[1], ymax=dy[2])
    y <- crop(x, bb)
    expect_equal(instances(element(y, i)), id[k])
    expect_equal(instances(getTable(y, i)), id[k])
})
