# TODO: better handling of .zattrs...

setGeneric("getCS", \(x, ...) standardGeneric("getCS"))
setGeneric("getTS", \(x, ...) standardGeneric("getTS"))

# TODO: implement all transformations (translate, scale, rotate, affine, and sequential) for all layers

# coordinate systems
setMethod("getCS", "SpatialDataElement", \(x) {
    ms <- (md <- meta(x))$multiscales
    if (!is.null(ms)) md <- md$multiscales
    cs <- md$coordinateTransformations
    if (length(cs) == 1) cs[[1]] else cs
})

# transformations
setMethod("getTS", "SpatialDataElement", \(x, i=1) {
    y <- getCS(x)
    if (is.character(i)) 
        i <- which(y$output$name == i)
    y[i, ]$transformations[[1]]
})

#' @importFrom EBImage resize
setMethod("scale", "ImageArray", \(x, t, ...) {
    a <- as.array(data(x))
    d <- length(dim(a))
    if (missing(t)) 
        t <- rep(1, d)
    b <- resize(aperm(a),
        w=dim(a)[d]*t[d],
        h=dim(a)[d-1]*t[d-1])
    x@data <- aperm(b)
    x
})

#' @importFrom EBImage resize
setMethod("translation", "ImageArray", \(x, t, ...) {})
setMethod("transform", "ImageArray", \(x, t) get(t$type)(x, unlist(t[[t$type]])))