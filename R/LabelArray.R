#' @name LabelArray
#' @title The `LabelArray` class
#'
#' @param x \code{LabelArray}
#' @param data list of \code{\link[Rarr]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param ... option arguments passed to and from other methods.
#'
#' @return \code{LabelArray}
#'
#' @examples
#' # TODO
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @export
LabelArray <- function(data=array(), meta=Zattrs(), metadata=list(), ...) {
    x <- .LabelArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname LabelArray
#' @export
setMethod("dim", "LabelArray", \(x) dim(data(x)))

#' @rdname LabelArray
#' @exportMethod [
setMethod("[", "LabelArray", \(x, i, j, ..., drop=FALSE) {
    # TODO: subsetting for multiscales
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE
    x@data <- data(x)[i, j, drop=FALSE]
    return(x)
})
