#' @name LabelArray
#' @title The \code{LabelArray} class
#'
#' @description 
#' The \code{LabelArray} class stores \code{SpatialData} elements from its 
#' \code{"labels"} layers. These are represented as a \code{ZarrMatrix} 
#' (\code{data} slot) associated with .zattrs stored as \code{\link{Zattrs}} 
#' (\code{meta} slot); a list of \code{metadata} stores other arbitrary info.
#'
#' Currently defined methods (here, \code{x} is a \code{LabelArray}):
#' \itemize{
#' \item \code{data/meta(x)} to access underlying \code{ZarrMatrix/Zattrs}
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' }
#'
#' @param x \code{LabelArray}
#' @param data list of \code{\link[Rarr]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param i,j indices specifying elements to extract.
#' @param drop ignored.
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
#' @importFrom utils head tail
#' @exportMethod [
setMethod("[", "LabelArray", \(x, i, j, ..., drop=FALSE) {
  if (missing(i)) i <- TRUE else if (isFALSE(i)) i <- 0 else .check_jk(i, "i")
  if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
  n <- length(data(x, NULL))
  d <- dim(data(x, 1))
  x@data <- lapply(seq_len(n), \(.) {
    i <- if (isTRUE(i)) seq_len(d[1]) else i
    j <- if (isTRUE(j)) seq_len(d[2]) else j
    ij <- lapply(list(i, j), \(ij) {
      fac <- 2^(.-1)
      seq(floor(head(ij, 1)/fac), 
          ceiling(tail(ij, 1)/fac))
    })
    data(x, .)[ij[[1]], ij[[2]], drop=FALSE]
  })
  x
})