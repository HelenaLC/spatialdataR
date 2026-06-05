zs <- file.path("extdata", "blobs.zarr")
zs <- system.file(zs, package="spatialdataR")
sd <- readSpatialData(zs)

test_that("show(SpatialData)", {
    # element counts
    ni <- length(imageNames(sd))
    nl <- length(labelNames(sd))
    np <- length(pointNames(sd))
    ns <- length(shapeNames(sd))
    nt <- length(tableNames(sd))
    
    # coordinate systems
    g <- CTgraph(sd)
    typ <- graph::nodeData(g, graph::nodes(g), "type")
    xyz <- graph::nodeData(g, graph::nodes(g), "type") == "space"
    cs <- graph::nodes(g)[xyz]
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
        pa <- suppressWarnings(RBGL::sp.between(g, paste0("_", el), c))
        n <- sum(vapply(pa, \(.) !is.na(.$length), logical(1)))
        ok <- c(ok, sprintf("- %s\\(%d\\):", c, n))
    }
    
    # capture show & match against patterns
    out <- paste(capture.output(show(sd)), collapse="\n")
    for (. in ok) expect_match(out, .)
})
