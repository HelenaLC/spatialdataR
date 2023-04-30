#' @rdname ShapeFrame
#' @title The `ShapeFrame` class
#' @aliases
#' ShapeFrame ShapeFrame-class
#' coords,ShapeFrame-method
#' scaleFrame
#'
#' @description
#' A \code{\link{DataFrame}} with fixed structure
#' and .zattrs stored as metadata.
#' Each row corresponds to a shape, and is defined by its
#' \itemize{
#' \item{index: unique identifier}
#' \item{data:
#'   list of arrays containing xy-coordinates,
#'   and (if shapes are circles) radii.}
#' \item{type:
#'   character string specifying the geometry
#'   (\code{"circle"} or \code{"polygon"})}
#' }
#'
#' @param x An object of class \code{ShapeFrame}.
#' @param data A \code{\link{DataFrame}} of appropriate format.
#' @param metadata A list of metadata corresponding to .zattrs.
#'
#' @return \code{ShapeFrame}
#'
#' @examples
#' ShapeFrame()
#'
#' @author Helena L. Crowell
#'
#' @importFrom S4Vectors DataFrame isEmpty metadata<-
#' @export
ShapeFrame <- function(data=DataFrame(), metadata=list()) {
    if (isEmpty(data))
        data <- data.frame(
            data=numeric(),
            index=integer(),
            type=character())
    data <- DataFrame(data)
    df <- .ShapeFrame(data)
    metadata(df) <- metadata
    return(df)
}

#' @rdname ShapeFrame
#' @export
setMethod("as.array", "ShapeFrame", function(x) do.call(rbind, x$data))
