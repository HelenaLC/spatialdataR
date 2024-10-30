#' ImageArray
#'
#' @return ...
#'
#' @examples
#' # TODO
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
