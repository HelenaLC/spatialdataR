#' LabelArray
#'
#' @return ...
#'
#' @examples
#' # TODO
#'
#' @importFrom S4Vectors metadata<-
#' @export
LabelArray <- function(data=array(), meta=Zattrs(), metadata=list(), ...) {
    x <- .LabelArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname LabelArray
#' @export
setMethod("dim", "LabelArray", \(x) dim(data(x)))
