#' @name utils
#' @rdname utils
#' @title Utilities
#' @aliases centroids
#' 
#' @param x a \code{SpatialData} element (any but image)
#' @param as determines how results will be returned
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' centroids(label(x))
#' centroids(shape(x))
#' centroids(shape(x, 3), "list")
#' 
#' head(centroids(point(x)))
#' xy <- centroids(point(x), "list")
#' plot(xy$gene_a, col=a <- "red")
#' points(xy$gene_b, col=b <- "blue")
#' legend("topright", legend=names(xy), col=c(a, b), pch=21)
NULL

#' @export
#' @rdname utils
setMethod("centroids", "ANY", \(x, ...) stop("'centroids' ",
    "only supported for label, shape, and point elements"))

#' @export
#' @rdname utils
#' @importFrom Matrix summary
setMethod("centroids", "LabelArray", \(x, 
    as=c("data.frame", "matrix")) {
    as <- match.arg(as)
    y <- data(x)
    y <- as(y, "dgCMatrix")
    i <- summary(y)
    xy <- tapply(i[, -3], i[[3]], colMeans)
    xy <- do.call(rbind, xy)
    xy <- cbind(xy, as.integer(rownames(xy)))
    colnames(xy) <- c("x", "y", "i")
    if (as == "matrix") return(xy)
    as.data.frame(xy)
})

#' @export
#' @rdname utils
#' @importFrom sf st_as_sf st_geometry_type st_coordinates
setMethod("centroids", "ShapeFrame", \(x, 
    as=c("data.frame", "matrix", "list")) {
    as <- match.arg(as)
    y <- st_as_sf(data(x))
    xy <- st_coordinates(y)
    colnames(xy)[c(1, 2)] <- c("x", "y")
    if (as == "matrix") return(xy)
    xy <- as.data.frame(xy)
    if (as == "data.frame") return(xy)
    split(xy, xy[seq(3, ncol(xy))])
})

#' @export
#' @rdname utils
setMethod("centroids", "PointFrame", \(x, 
    as=c("data.frame", "matrix", "list")) {
    as <- match.arg(as)
    i <- feature_key(x)
    xy <- data(x)[, c("x", "y", i)]
    xy <- as.data.frame(xy)
    if (as == "data.frame") return(xy)
    if (as == "matrix") return(as.matrix(xy))
    lapply(split(xy, xy[[i]]), `[`, -3)
})