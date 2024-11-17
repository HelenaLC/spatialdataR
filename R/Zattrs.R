#' @name Zattrs
#' @title The `Zattrs` class
#'
#' @param x list extracted from a OME-NGFF compliant .zattrs file.
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

#' @rdname Zattrs
#' @exportMethod $
setMethod("$", "Zattrs", \(x, name) x[[name]])

setGeneric("axes", \(x, ...) standardGeneric("axes"))
setGeneric("coordTransData", \(x, ...) standardGeneric("coordTransData"))
setGeneric("coordTransName", \(x, ...) standardGeneric("coordTransName"))
setGeneric("coordTransType", \(x, ...) standardGeneric("coordTransType"))

#' @rdname Zattrs
#' @export
setMethod("axes", "Zattrs", \(x, ...) {
    if (!is.null(ms <- x$multiscales)) x <- ms
    if (is.null(x <- x$axes)) stop("couln't find 'axes'") 
    if (is.character(x)) x else x[[1]]
})

#' @rdname Zattrs
#' @export
setMethod("coordTransData", "Zattrs", \(x, ...) {
    ms <- x$multiscales
    if (!is.null(ms)) x <- ms
    x <- x$coordinateTransformations
    if (is.null(dim(x))) x[[1]] else x
})

#' @rdname Zattrs
#' @export
setMethod("coordTransName", "Zattrs", \(x, ...) coordTransData(x)$output$name)

#' @rdname Zattrs
#' @export
setMethod("coordTransType", "Zattrs", \(x, ...) coordTransData(x)$type)

#' @rdname Zattrs
#' @export
setMethod("axes", "SpatialDataElement", \(x, ...) axes(meta(x)))

#' @rdname Zattrs
#' @export
setMethod("coordTransData", "SpatialDataElement", \(x, ...) coordTransData(meta(x)))

#' @rdname Zattrs
#' @export
setMethod("coordTransName", "SpatialDataElement", \(x, ...) coordTransName(meta(x)))

#' @rdname Zattrs
#' @export
setMethod("coordTransType", "SpatialDataElement", \(x, ...) coordTransType(meta(x)))
