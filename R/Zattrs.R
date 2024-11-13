#' @name Zattrs
#' @title The `Zattrs` class
#'
#' @return \code{Zattrs}
#'
#' @examples
#' Zattrs()
#'
#' @export
Zattrs <- \(x=list()) {
    .Zattrs(x)
}

# TODO: ideally some valid empty constructor for each type of element,
# e.g., .zattrs are different for point/label/shape/image elements;
# simplest would be xyz (time, channel), identity transformation etc. 

#' @importFrom utils .DollarNames
#' @export
.DollarNames.Zattrs <- \(x, pattern="") names(x)

#' @rdname SpatialData
#' @exportMethod $
setMethod("$", "Zattrs", \(x, name) x[[name]])
