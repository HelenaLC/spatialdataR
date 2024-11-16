#' @name ShapeFrame
#' @title The `ShapeFrame` class
#'
#' @param x \code{ShapeFrame}
#' @param data \code{arrow}-derived table for on-disk,
#'   \code{data.frame} for in-memory representation.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{ShapeFrame}
#'
#' @examples
#' tf <- tempfile()
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
#' @importFrom methods new
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
setMethod("dim", "ShapeFrame", \(x) dim(data(x)))

#' @rdname ShapeFrame
#' @export
setMethod("length", "ShapeFrame", \(x) nrow(data(x)))

#' @rdname ShapeFrame
#' @export
setMethod("names", "ShapeFrame", \(x) names(data(x)))

#' @importFrom utils .DollarNames
#' @export
.DollarNames.ShapeFrame <- \(x, pattern="") {
    grep(pattern, names(x), value=TRUE)
}

#' @rdname ShapeFrame
#' @exportMethod $
setMethod("$", "ShapeFrame", \(x, name) data(x)[[name]])

# sub ----

#' @rdname ShapeFrame
#' @export
setMethod("[", c("ShapeFrame", "missing", "ANY"), 
    \(x, i, j, ...) x[seq_len(nrow(x)), j])

#' @rdname ShapeFrame
#' @export
setMethod("[", c("ShapeFrame", "ANY", "missing"), 
    \(x, i, j, ...) x[i, seq_len(ncol(x))])

#' @rdname ShapeFrame
#' @export
setMethod("[", c("ShapeFrame", "missing", "missing"), 
    \(x, i, j, ...) x[seq_len(nrow(x)), seq_len(ncol(x))])

#' @rdname ShapeFrame
#' @export
setMethod("[", c("ShapeFrame", "numeric", "numeric"), \(x, i, j, ...) { 
    i <- seq_len(nrow(x))[i]
    j <- seq_len(nrow(x))[j]
    x@data <- x@data[i, j]
    return(x)
})
