# TODO: currently applying transformations only on 'data.frame's for plotting,
# not the actual data (e.g., image)... but this might be necessary for queries?

# TODO: for all layers, implement all transformations 
# (translate, scale, rotate, affine, and sequential) 

# TODO: better handling of .zattrs... e.g., accessors for 
# axes names/types, CS shared between elements etc.

setGeneric("getCS", \(x, ...) standardGeneric("getCS"))
setGeneric("getTS", \(x, ...) standardGeneric("getTS"))

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
    # TODO: this should be done w/o realizing 
    # into memory, but EBImage needs an array?
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