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
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x, tables=FALSE)
#'
#' centroids(label(x))
#' centroids(shape(x))
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
setMethod("centroids", "SpatialDataLabel", \(x,
    as=c("data.frame", "matrix")) {
    y <- data(x)
    as <- match.arg(as)
    ax <- .get_xy_axes(x)
    # max-projection
    if (length(dim(y)) > 2) 
        y <- apply(y, c(ax$y, ax$x), max)
    y <- as(y, "dgCMatrix")
    i <- summary(y)
    # flip dimensions so that columns=x, rows=y
    i[, c(1, 2)] <- i[, c(2, 1)]-0.5
    xy <- tapply(i[, -3], i[[3]], colMeans)
    xy <- do.call(rbind, xy)
    xy <- cbind(xy, as.integer(rownames(xy)))
    dimnames(xy) <- list(NULL, c("x", "y", "i"))
    # multi-scale adjustment
    sf <- .get_ms_scale(x)
    xy[,1] <- xy[,1]*sf[ax$x]
    xy[,2] <- xy[,2]*sf[ax$y]
    # offset
    wh <- metadata(x)$wh
    if (!is.null(wh)) {
        xy[,1] <- xy[,1]+wh[[1]][1]
        xy[,2] <- xy[,2]+wh[[2]][1]
    }
    # output
    if (as == "matrix") return(xy)
    xy <- as.data.frame(xy)
    xy$i <- factor(xy$i)
    return(xy)
})

#' @export
#' @rdname centroids
#' @importFrom sf st_as_sf st_geometry_type st_centroid st_coordinates
setMethod("centroids", "SpatialDataShape", \(x,
    as=c("data.frame", "matrix", "list")) {
    as <- match.arg(as)
    xy <- data(x) |>
        st_as_sf() |>
        st_centroid() |>
        st_coordinates()
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
setMethod("centroids", "SpatialDataPoint", \(x,
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
