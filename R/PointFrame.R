#' @rdname PointFrame
#' @title The `PointFrame` class
#' @aliases
#' PointFrame PointFrame-class
#' $,PointFrame-method
#' dim,PointFrame-method
#' length,PointFrame-method
#' coord,PointFrame-method
#' coords,PointFrame-method
#'
#' @description
#' ...
#'
#' @param x An object of class \code{PointFrame}.
#' @param data An object of class \code{\link{arrow}[Table]}.
#' @param metadata A list
#' @param i,j Indices for subsetting (see \code{?base::Extract}).
#' @param name A character string specifying the column extract..
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{PointFrame}
#'
#' @examples
#' path <- "extdata/blobs/points/blobs_points"
#' path <- system.file(path, package = "SpatialData")
#' (pf <- readPoints(path))
#'
#' @author Helena L. Crowell
#'
#' @importFrom S4Vectors metadata<-
#' @export
PointFrame <- function(data=NULL, metadata=list(), ...) {
    if (is.null(data)) data <- data.frame()
    pf <- .PointFrame(data=data, ...)
    metadata(pf) <- metadata
    return(pf)
}
