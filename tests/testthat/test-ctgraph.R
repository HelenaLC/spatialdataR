x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x)

test_that("CTgraph", {
    # invalid
    expect_error(CTgraph(list()))
    expect_error(CTgraph(SpatialData::table(x)))
    # object-wide
    g <- CTgraph(x)
    expect_is(g, "graph")
    # graph should contain node for
    # every element & transformation
    ns <- lapply(setdiff(SpatialData:::.LAYERS, "tables"), 
        \(l) lapply(names(x[[l]]), 
            \(e) c(paste0("_", e), CTname(x[[l]][[e]]))))
    ns <- sort(unique(unlist(ns)))
    expect_true(all(ns %in% sort(graph::nodes(g))))
    # element-wise
    for (l in setdiff(SpatialData:::.LAYERS, "tables")) 
        for (e in names(x[[l]])) {
            y <- x[[l]][[e]]
            g <- CTgraph(y)
            expect_is(g, "graph")
            expect_true("_self" %in% graph::nodes(g))
        }
})

test_that("CTpath", {
    i <- "blobs_image"
    y <- element(x, i)
    z <- CTpath(y, j <- CTname(y))
    expect_identical(CTpath(x, i, j), z)
    expect_is(z, "list")
    expect_length(z <- z[[1]], 2)
    expect_setequal(names(z), c("type", "data"))
    expect_is(z$type, "character")
    expect_length(z$type, 1)
})

test_that("CTplot", {
    f <- function(.) {
        tf <- tempfile(fileext=".pdf")
        on.exit(unlink(tf))
        pdf(tf); .; dev.off()
        file.size(tf)
    }
    g <- CTgraph(x)
    p <- f(CTplot(g))
    expect_is(p, "numeric")
    expect_true(p > f(plot(1)))
    p <- f(CTplot(g, 0.1))
    q <- f(CTplot(g, 0.9))
    expect_true(p < q)
})
