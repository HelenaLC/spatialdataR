zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

fn <- \(x, y) {
    z <- paste(capture.output(show(x)), collapse="\n")
    for (. in y) expect_match(z, .)
}

test_that("show(SpatialData)", {
    # element counts
    ni <- length(imageNames(sd))
    nl <- length(labelNames(sd))
    np <- length(pointNames(sd))
    ns <- length(shapeNames(sd))
    nt <- length(tableNames(sd))
    
    # coordinate systems
    cg <- CTgraph(sd)
    ts <- graph::nodeData(cg, graph::nodes(cg), "type")
    cs <- graph::nodes(cg)[ts == "space"]
    nc <- length(cs)
    
    # expected patterns 
    ok <- c(
        "class: SpatialData",
        sprintf("- images\\(%d\\):", ni),
        sprintf("- labels\\(%d\\):", nl),
        sprintf("- points\\(%d\\):", np),
        sprintf("- shapes\\(%d\\):", ns),
        sprintf("- tables\\(%d\\):", nt),
        sprintf("coordinate systems\\(%d\\):", nc))
    
    # include element names
    el <- unname(unlist(colnames(sd[-5])))
    ok <- c(ok, el, tableNames(sd))
    
    # add coordinate systems with element counts
    for (c in cs) {
        # check connectivity b/w spatial elements & coordinate systems
        pa <- suppressWarnings(RBGL::sp.between(cg, paste0("_", el), c))
        n <- sum(vapply(pa, \(.) !is.na(.$length), logical(1)))
        ok <- c(ok, sprintf("- %s\\(%d\\):", c, n))
    }
    fn(sd, ok)
})

test_that("show(SpatialDataElement)", {
    # image
    x <- image(sd, 1)
    y <- axes(x, "name")
    ok <- c(
        "class: SpatialDataImage",
        sprintf("\\(%dd: %s\\)", length(y), paste(y, collapse="")),
        sprintf("levels\\(%d\\):", length(data(x, NULL))),
        sprintf("(%s)", paste(dim(x), collapse=",")))
    fn(x, ok)
    
    # label
    x <- label(sd, 1)
    y <- axes(x, "name")
    ok <- c(
        "class: SpatialDataLabel",
        sprintf("\\(%dd: %s\\)", length(y), paste(y, collapse="")),
        sprintf("levels\\(%d\\):", length(data(x, NULL))),
        sprintf("(%s)", paste(dim(x), collapse=",")))
    fn(x, ok)
    
    # point
    x <- point(sd, 1)
    ok <- c(
        "class: SpatialDataPoint",
        sprintf("count: %d", length(x)),
        sprintf("data\\(%d\\):", length(names(x))),
        paste(names(x), collapse=" "))
    fn(x, ok)
    
    # shape
    x <- shape(sd, 1)
    ok <- c(
        "class: SpatialDataShape",
        sprintf("count: %d", length(x)),
        sprintf("data\\(%d\\):", length(names(x))),
        paste(names(x), collapse=" "))
    fn(x, ok)
})

test_that("show(SpatialDataAttrs)", {
    l <- list(image(sd), label(sd))
    for (x in l) {
        x <- meta(x)
        ok <- c(
            "class: SpatialDataAttrs",
            sprintf("axes\\(%d\\):", length(axes(x))),
            sprintf("coordTrans\\(%d\\):", length(CTlist(x))))
        if (!is.null(chs <- channels(x))) 
            ok <- c(ok, sprintf("channels\\(%d\\):", length(chs)))
        if (!is.null(ms <- multiscales(x)[[1]]))
            ok <- c(ok, sprintf("datasets\\(%d\\):", length(ms$datasets)))
        fn(x, ok)
    }
})
