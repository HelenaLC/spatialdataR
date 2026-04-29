#' @name utils
#' @rdname utils
#' @title Utilities
#' @aliases centroids extent
#'
#' @param x a \code{SpatialData} element (any but image).
#' @param i scalar integer or string; target coordinate space.
#' @param as character string; how results should be returned.
#' @param ... optional arguments passed to and from other methods.
#'
#' @returns
#' For \code{centroids}, a table (\code{data.frame} or \code{matrix})
#' of spatial coordinates (if \code{as="list"}, split by instance);
#' for extend, a length-2 numeric list of x- and y-ranges.
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
#'
#' # object-wide
#' extent(x)
#'
#' # element-wise
#' extent(label(x))
#' extent(point(x))
#' extent(shape(x))
NULL

# centroids ----

# Internal helper for null-coalescing
`%||%` <- \(a, b) if (is.null(a)) b else a

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
    rownames(xy) <- NULL
    if (ncol(xy) > 2)
        for (. in seq(3, ncol(xy)))
            xy[[.]] <- factor(xy[[.]], unique(xy[[.]]))
    if (as == "data.frame") return(xy)
    split(xy, xy[seq(3, ncol(xy))])
})

#' @export
#' @rdname utils
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

# extent ----

#' @export
#' @rdname utils
setMethod("extent", "SpatialData", \(x, i=1) {
    ls <- setdiff(.LAYERS, "tables")
    ex <- Reduce(c, lapply(ls, \(.) lapply(x[[.]], extent, i=i)))
    xy <- do.call(rbind, lapply(ex, do.call, what=cbind))
    list(x=range(xy[, 1]), y=range(xy[, 2]))
})

#' @export
#' @rdname utils
setMethod("extent", "sdArray", \(x, i=1) {
    x <- transform(x, i)
    wh <- metadata(x)$wh
    if (!is.null(wh)) return(wh)
    n <- length(d <- dim(x))
    if (n == 3) d <- d[-1]
    d <- rev(d)
    names(d) <- c("x", "y")
    lapply(d, \(.) c(0, .))
})

#' @export
#' @rdname utils
#' @importFrom duckspatial ddbs_bbox
setMethod("extent", "sdFrame", \(x, i=1) {
    x <- transform(x, i)
    v <- ddbs_bbox(data(x))
    l <- list(
        x=c(v$xmin, v$xmax), 
        y=c(v$ymin, v$ymax))
    lapply(l, unname)
})
