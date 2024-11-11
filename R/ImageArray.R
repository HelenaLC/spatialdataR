#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param data \code{\link[Rarr]{ZarrArray}} for on-disk, 
#'   \code{array} for in-memory representation.
#' @param meta ...
#' @param metadata ....
#'
#' @return \code{ImageArray}
#'
#' @examples
#' dir.create(td <- tempfile())
#' pa <- unzip_merfish_demo(td)
#' pa <- file.path(pa, "images", "rasterized")
#' (ia <- readImage(pa))
#' 
#' a <- as.array(data(ia))
#' a <- aperm(a, c(2,3,1))
#' plot(EBImage::Image(a/255))
#'
#' @importFrom S4Vectors metadata<-
#' @export
ImageArray <- function(data=array(), meta=Zattrs(), metadata=list(), ...) {
    x <- .ImageArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname ImageArray
#' @export
setMethod("dim", "ImageArray", \(x) dim(data(x)))
