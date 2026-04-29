#' @name extent
#' @title Spatial element extent
#'
#' @param x a \code{SpatialData} element (any but table).
#' @param i scalar integer or string; target coordinate space.
#'
#' @returns Length-2 list with numeric x and y ranges.
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#'
#' # object-wide
#' extent(x)
#'
#' # element-wise
#' extent(image(x))
#' extent(point(x))
#' extent(shape(x))
#' 
#' # with transformation(s)
#' extent(label(x), "scale")
#' extent(label(x), "translation")
NULL

#' @export
#' @rdname extent
setMethod("extent", "SpatialData", \(x, i=1) {
    ex <- .lapplyLayer(x, extent, i=i)
    ex <- unlist(ex, recursive=FALSE)
    xy <- do.call(rbind, lapply(ex, do.call, what=cbind))
    list(x=range(xy[, 1]), y=range(xy[, 2]))
})

#' @export
#' @rdname extent
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
#' @rdname extent
#' @importFrom duckspatial ddbs_bbox
setMethod("extent", "sdFrame", \(x, i=1) {
    x <- transform(x, i)
    v <- ddbs_bbox(data(x))
    l <- list(
        x=c(v$xmin, v$xmax), 
        y=c(v$ymin, v$ymax))
    lapply(l, unname)
})
