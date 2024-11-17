# TODO: currently applying transformations only on 'data.frame's for plotting,
# not the actual data (e.g., image)... but this might be necessary for queries?

# TODO: for all layers, implement all transformations 
# (translate, scale, rotate, affine, and sequential)

#' @name .coord2graph
#' @rdname coord2graph
#' @title CS graph representation
#' 
#' @param x \code{SpatialData} object
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' g <- SpatialData:::.coord2graph(x)
#' # visualize element-coordinate system relations as graph
#' graph::plot(g)
#' # retrieve transformation from element to target space
#' graph::edgeData(g, "blobs_labels", "translation", "data")
#' 
#' @importFrom graph graphAM nodes
#'   addNode nodeData<- nodeDataDefaults<-
#'   addEdge edgeData<- edgeDataDefaults<-
.coord2graph <- \(x) {
    g <- graphAM(edgemode="directed")
    edgeDataDefaults(g, "data") <- list()
    edgeDataDefaults(g, "type") <- character()
    nodeDataDefaults(g, "type") <- character()
    names(ls) <- ls <- setdiff(.LAYERS, "tables")
    for (l in ls) for (e in names(x[[l]])) {
        md <- meta(x[[l]][[e]])
        ms <- md$multiscales
        if (!is.null(ms)) md <- ms
        ct <- md$coordinateTransformations
        ct <- if (length(ct) == 1) ct[[1]] else ct
        g <- addNode(e, g)
        nodeData(g, e, "type") <- "element"
        for (i in seq(nrow(ct))) {
            n <- ct$output$name[i]
            if (!n %in% nodes(g)) {
                g <- addNode(n, g)
                nodeData(g, n, "type") <- "space"
            }
            t <- ct$type[i]
            if (t == "sequence") {
                sq <- ct$transformations[i][[1]]
                . <- e
                for (j in seq(nrow(sq))) {
                    if (j == nrow(sq)) {
                        m <- n
                    } else {
                        m <- paste(e, n, j, sep="_")
                        g <- addNode(m, g)
                        nodeData(g, m, "type") <- "none"
                    }
                    t <- sq$type[j]
                    d <- sq[[t]][j]
                    g <- addEdge(., m, g)
                    edgeData(g, ., m, "type") <- t
                    edgeData(g, ., m, "data") <- d
                    . <- m
                }
            } else {
                g <- addEdge(e, n, g)
                d <- ct[[ct$type[i]]][i]
                edgeData(g, e, n, "type") <- t
                edgeData(g, e, n, "data") <- d
            }
        }
    }
    return(g)
}

#' @name .get_path
#' @rdname get_path
#' @title get transformations path
#' 
#' @param g \code{\link[graph]{graphAM}}
#' @param i,j source and target node label
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' g <- SpatialData:::.coord2graph(x)
#' SpatialData:::.get_path(g, "blobs_labels", "sequence")
#' 
#' @importFrom graph edgeData
#' @importFrom RBGL sp.between
.get_path <- \(g, i, j) {
    p <- sp.between(g, i, j)
    p <- p[[1]]$path_detail
    n <- length(p)-1
    lapply(seq_len(n), \(.)
        edgeData(g, p[.], p[.+1])[[1]])
}

setGeneric("getCS", \(x, ...) standardGeneric("getCS"))
setGeneric("getTS", \(x, ...) standardGeneric("getTS"))
setGeneric("setCS", \(x, ...) standardGeneric("setCS"))
setGeneric("setTS", \(x, ...) standardGeneric("setTS"))

# coordinate systems
setMethod("getCS", "SpatialDataElement", \(x) {
    ms <- (md <- meta(x))$multiscales
    if (!is.null(ms)) md <- md$multiscales
    cs <- md$coordinateTransformations
    if (length(cs) == 1) cs[[1]] else cs
})

# TODO: check the validity of this function
setMethod("setCS", "SpatialDataElement", \(x, c) {
    ms <- (md <- meta(x))$multiscales
    if (!is.null(ms)){
        cs <- md$coordinateTransformations
        if (length(cs) == 1) {
            md$multiscales$coordinateTransformations <- c
        } else{
            md$multiscales$coordinateTransformations[[1]] <- c
        }
    }
    md
})

# transformations
setMethod("getTS", "SpatialDataElement", \(x, i=1) {
    y <- getCS(x)
    if (is.character(i)) 
        i <- which(y$output$name == i)
    y[i, ]$transformations[[1]]
})

# transformations
setMethod("setTS", "SpatialDataElement", \(x, i=1, t) {
    y <- getCS(x)
    if (is.character(i)) 
        i <- which(y$output$name == i)
    y[i, ]$transformations[[1]] <- t
    x@meta <- Zattrs(setCS(x,y))
    x
})

#' #' @importFrom EBImage resize
#' setMethod("scale", "ImageArray", \(x, t, ...) {
#'     a <- as.array(data(x)) 
#'     # TODO: this should be done w/o realizing 
#'     # into memory, but EBImage needs an array?
#'     d <- length(dim(a))
#'     if (missing(t)) 
#'         t <- rep(1, d)
#'     b <- resize(aperm(a),
#'         w=dim(a)[d]*t[d],
#'         h=dim(a)[d-1]*t[d-1])
#'     x@data <- aperm(b)
#'     x
#' })
#' 
#' #' @importFrom EBImage resize
#' setMethod("translation", "ImageArray", \(x, t, ...) {})
#' setMethod("transform", "ImageArray", \(x, t) get(t$type)(x, unlist(t[[t$type]])))
