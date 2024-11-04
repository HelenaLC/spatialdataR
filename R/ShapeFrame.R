#' @name ShapeFrame
#' @title The `ShapeFrame` class
#'
#' @param data ...
#' @param meta ...
#' @param metadata ....
#'
#' @return \code{ShapeFrame}
#'
#' @examples
#' tf = tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' y <- file.path(base, "shapes", "cells")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
#' 
#' y <- file.path(base, "shapes", "anatomical")
#' (s <- readShape(y))
#' plot(sf::st_as_sf(data(s)), cex=0.2)
#'
#' @importFrom S4Vectors metadata<-
#' @export
ShapeFrame <- function(data=data.frame(), meta=Zattrs(), metadata=list(), ...) {
    x <- .ShapeFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

# TODO: it's really annoying that this doesn't just inherit
# data.frame() operations, cuz data are in an extra slot... 
# but else not sure how to assure validity, stash .zattrs etc.

#' @rdname ShapeFrame
#' @export
setMethod("data", "ShapeFrame", \(x) x@data)

#' @rdname ShapeFrame
#' @export
setMethod("dim", "ShapeFrame", \(x) dim(data(x)))

#' @rdname ShapeFrame
#' @export
setMethod("length", "ShapeFrame", \(x) nrow(data(x)))

#' @rdname ShapeFrame
#' @export
setMethod("names", "ShapeFrame", \(x) {
    setdiff(names(data(x)), "__null_dask_index__") })

#' @importFrom utils .DollarNames
#' @export
.DollarNames.ShapeFrame <- \(x, pattern="") {
    grep(pattern, names(x), value=TRUE)
}

#' @rdname ShapeFrame
#' @exportMethod $
setMethod("$", "ShapeFrame", \(x, name) data(x)[[name]])
