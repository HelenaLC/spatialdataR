#' @name Zattrs
#' @title The `Zattrs` class
#'
#' @param x list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' 
#' @return \code{Zattrs}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' z <- meta(label(x))
#' axes(z) 
#' CTdata(z)
#' CTname(z)
#' CTtype(z)
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
