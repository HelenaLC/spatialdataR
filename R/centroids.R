#' @name centroids
#' @title Spatial element centroids
#'
#' @param x a \code{SpatialData} element (any but image).
#' @param as character string; how results should be returned.
#' @param ... ignored.
#'
#' @returns
#' A table (\code{data.frame} or \code{matrix}) of spatial coordinates 
#' (if \code{as="list"}, split by instance (shapes) or features (points)).
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
#' @rdname centroids
setMethod("centroids", "ANY", \(x, ...) stop("'centroids' ",
    "only supported for label, shape, and point elements"))

#' @export
#' @rdname centroids
#' @importFrom Matrix summary
setMethod("centroids", "LabelArray", \(x,
    as=c("data.frame", "matrix")) {
    as <- match.arg(as)
    y <- data(x)
    y <- as(y, "dgCMatrix")
    i <- summary(y)
    # flip dimensions so that columns=x, rows=y
    # TODO: should these be offset by 0.5?
    i[, c(1, 2)] <- i[, c(2, 1)]-0.5
    xy <- tapply(i[, -3], i[[3]], colMeans)
    xy <- do.call(rbind, xy)
    xy <- cbind(xy, as.integer(rownames(xy)))
    dimnames(xy) <- list(NULL, c("x", "y", "i"))
    if (as == "matrix") return(xy)
    xy <- as.data.frame(xy)
    xy$i <- factor(xy$i); xy
})

#' @export
#' @rdname centroids
#' @importFrom sf st_as_sf st_geometry_type st_coordinates
setMethod("centroids", "ShapeFrame", \(x,
    as=c("data.frame", "matrix", "list")) {
    as <- match.arg(as)
    y <- st_as_sf(data(x))
    xy <- st_coordinates(y)
    colnames(xy)[c(1, 2)] <- c("x", "y")
    if (as == "matrix") return(xy)
    xy <- as.data.frame(xy)
    rownames(xy) <- NULL
    if (ncol(xy) > 2)
        for (. in seq(3, ncol(xy)))
            xy[[.]] <- factor(xy[[.]], unique(xy[[.]]))
    if (as == "data.frame") return(xy)
    split(xy, xy[seq(3, ncol(xy))])
})

#' @export
#' @rdname centroids
#' @importFrom dplyr pull
#' @importFrom sf st_as_sf st_coordinates
setMethod("centroids", "PointFrame", \(x,
    as=c("data.frame", "list")) {
    as <- match.arg(as)
    xy <- data(x) |>
        st_as_sf() |> 
        st_coordinates()
    xy <- data.frame(xy)
    names(xy) <- axes(x)
    fk <- feature_key(x)
    xy[[fk]] <- pull(x, fk)
    if (as == "data.frame") return(xy)
    lapply(split(xy, xy[[fk]]), `[`, -3)
})
