#' given a graphNEL instance, nodes with nchar > max are split and hyphenated at
#' character position floor(nchar/fac)
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' g <- SpatialData:::.coord2graph(x)
#' plotCoordGraph(g, cex=.6)
#' @export
plotCoordGraph = function(g, cex=.6) {
 g2view = g  # leave g alone
 graph::nodes(g2view) <- nodefix(graph::nodes(g2view))
 graph::graph.par(list(nodes=list(shape="plaintext", cex=cex)))
 g2view = Rgraphviz::layoutGraph(g2view)
 Rgraphviz::renderGraph(g2view)
}

nodefix = function (x, fac = 2, max = 10) 
{
    nc = nchar(x)
    if (any(nc > max)) {
        x[nc > max] = fixup(x[nc > max], fac)
    }
    x
}

fixup = function (x, fac) 
{
    nc = nchar(x)
    hm = floor(nc/fac)
    xs = strsplit(x, "")
    xu = lapply(seq_len(length(xs)), function(i) c(xs[[i]][seq_len(hm[i])], 
        "-\n", xs[[i]][-seq_len(hm[i])]))
    xu = lapply(xu, function(z) paste(z, collapse = ""))
    unlist(xu)
}

