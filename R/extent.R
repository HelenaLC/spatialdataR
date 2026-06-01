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
#' x <- system.file(x, package="spatialdataR")
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
    xy <- dplyr::bind_rows(ex)
    list(x=range(xy$x), y=range(xy$y))
})

#' @export
#' @rdname extent
setMethod("extent", "SpatialDataArray", \(x, i=1) {
    x <- transform(x, i)
    wh <- metadata(x)$wh %||% {
        n <- length(d <- dim(x))
        if (n == 3) d <- d[-1]
        d <- rev(d)
        lapply(d, \(.) c(0, .))
    }
    names(wh) <- c("x", "y")
    return(wh)
})

#' @export
#' @rdname extent
#' @importFrom duckspatial ddbs_bbox
setMethod("extent", "SpatialDataFrame", \(x, i=1) {
    x <- transform(x, i)
    v <- ddbs_bbox(data(x))
    l <- list(
        x=c(v$xmin, v$xmax), 
        y=c(v$ymin, v$ymax))
    lapply(l, unname)
})
