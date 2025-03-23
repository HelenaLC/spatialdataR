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
