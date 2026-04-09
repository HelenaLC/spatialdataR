#' @name CTgraph
#' @title Coord. trans. graph
#' @aliases CTgraph CTpath CTplot
#' 
#' @param x \code{SpatialData}, an element, or \code{Zattrs}.
#' @param i character string; name of source node.
#' @param j character string; name of target coordinate space.
#' @param g base R graph; extracted with \code{CTgraph}.
#' @param cex scalar numeric; controls fontsize of node labels.
#' @param fac,max scalar numeric; node labels with \code{nchar>max}
#'   are split and hyphenated at position \code{floor(nchar/fac)}
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # object-wide
#' g <- CTgraph(x)
#' CTplot(g)   
#' 
#' # one element
#' y <- label(x)
#' g <- CTgraph(y)
#' CTplot(g) 
#' 
#' # retrieve transformation(s) 
#' # from element to target space
#' CTpath(x, "blobs_labels", "sequence")
NULL

#' @rdname CTgraph
#' @export
setMethod("CTgraph", "SpatialData", \(x) {
    names(ls) <- ls <- setdiff(.LAYERS, "tables")
    md <- lapply(ls, \(l) {
        names(es) <- es <- names(x[[l]])
        lapply(es, \(e) meta(x[[l]][[e]]))
    })
    .make_g(md)
})

#' @rdname CTgraph
#' @export
setMethod("CTgraph", "SpatialDataElement", \(x) 
    .make_g(list("mock"=list("self"=meta(x)))))

#' @rdname CTgraph
#' @export
setMethod("CTgraph", "ANY", \(x) stop("'x' should be a", 
    " 'SpatialData' object, or a non-'table' element"))

#' @importFrom graph graphAM nodeDataDefaults<- edgeDataDefaults<-
.init_g <- \() {
    g <- graphAM(edgemode="directed")
    edgeDataDefaults(g, "data") <- list()
    edgeDataDefaults(g, "type") <- character()
    nodeDataDefaults(g, "type") <- character()
    return(g)
}

#' @importFrom graph nodes addNode addEdge nodeData<- edgeData<- 
.make_g <- \(md) {
    g <- .init_g()
    for (l in names(md)) for (e in names(md[[l]])) {
        .md <- md[[l]][[e]]
        ms <- .md$multiscales
        if (!is.null(ms)) .md <- ms[[1]]
        ct <- .md$coordinateTransformations
        g <- addNode(e, g)
        nodeData(g, e, "type") <- "element"
        for (i in seq_along(ct)) {
            n <- ct[[i]]$output$name
            if (!n %in% nodes(g)) {
                g <- addNode(n, g)
                nodeData(g, n, "type") <- "space"
            }
            t <- ct[[i]]$type
            if (t == "sequence") {
                sq <- ct[[i]]$transformations
                . <- e
                for (j in seq_along(sq)) {
                    if (j == length(sq)) {
                        m <- n
                    } else {
                        m <- paste(e, n, j, sep="_")
                        g <- addNode(m, g)
                        nodeData(g, m, "type") <- "none"
                    }
                    t <- sq[[j]]$type
                    d <- sq[[j]][[t]]
                    g <- addEdge(., m, g)
                    edgeData(g, ., m, "type") <- t
                    edgeData(g, ., m, "data") <- list(d)
                    . <- m
                }
            } else {
                g <- addEdge(e, n, g)
                d <- ct[[i]][[ct[[i]]$type]]
                edgeData(g, e, n, "type") <- t
                edgeData(g, e, n, "data") <- list(d)
            }
        }
    }
    return(g)
}

# path ----

#' @importFrom graph edgeData
#' @importFrom RBGL sp.between
.path_ij <- \(g, i, j) {
    p <- sp.between(g, i, j)
    p <- p[[1]]$path_detail
    n <- length(p)-1
    lapply(seq_len(n), \(.) edgeData(g, p[.], p[.+1])[[1]])
}

#' @rdname CTgraph
#' @export
setMethod("CTpath", "SpatialData", \(x, i, j) {
    g <- CTgraph(x)
    .path_ij(g, i, j)
})

#' @rdname CTgraph
#' @export
setMethod("CTpath", "SpatialDataElement", \(x, j) {
    g <- CTgraph(x)
    .path_ij(g, "self", j)
})

#' @rdname CTgraph
#' @export
setMethod("CTpath", "ANY", \(x) stop("'x' should be a", 
    " 'SpatialData' object, or a non-'table' element"))

# plot ----

#' @importFrom graph nodes nodes<- graph.par
#' @rdname CTgraph
#' @export
CTplot <- \(g, cex=0.5, fac=2, max=10) {
    if (!requireNamespace("Rgraphviz", quietly=TRUE))
        stop("Install 'Rgraphviz' to use this function")
    g2view <- g # leave 'g' alone
    nodes(g2view) <- .nodefix(nodes(g2view), fac=fac, max=max)
    graph.par(list(nodes=list(shape="plaintext", cex=cex)))
    g2view <- Rgraphviz::layoutGraph(g2view)
    Rgraphviz::renderGraph(g2view)
}

.nodefix <- \(x, fac=2, max=10) {
    fix <- nchar(x) > max
    if (!any(fix)) return(x)
    x[fix] <- .fixup(x[fix], fac)
    x
}

.fixup <- \(x, fac) {
    xs <- strsplit(x, "")
    nc <- floor(nchar(x)/fac)
    vapply(seq_along(xs), \(i) {
        j <- seq_len(nc[i])
        y <- c(xs[[i]][j], "-\n", xs[[i]][-j])
        paste(y, collapse="")
    }, character(1))
}
