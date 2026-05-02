#' @name Zattrs
#' @title The `Zattrs` class
#' 
#' @param x list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' @param type character string; either "array" (image/label) or "frame" (point/shape).
#' @param axes list of axes; if NULL, defaults to cyx (array) or xy (frame).
#' @param transformations list of transformations; if NULL, defaults to global identity.
#' @param ... additional attributes (e.g., version, feature_key).
#' 
#' @details 
#' When missing \code{x}, \code{Zattrs} will generate a valid object with
#' default axes (array: cyx, frame: xy) and transformations (identify) 
#' according to the specified type.
#' 
#' @return \code{Zattrs}
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' (z <- meta(label(x)))
#' 
#' CTname(z)
#' CTtype(z)
#' CTdata(z, "scale")
#' 
#' feature_key(point(x))
#' 
#' # constructor
#' Zattrs(type="frame")
#' Zattrs(type="array")
#' Zattrs(type="array", n=7)
#' Zattrs(type="array", label=TRUE)
#' 
#' @export
Zattrs <- function(x, type=c("array", "frame"), label=FALSE, trans=NULL, ver="0.4", n=3, ...) {
    if (!missing(x)) return(.Zattrs(x))
    type <- match.arg(type)
    # axes:
    # xy for points/shapes
    ax <-  list(
        list(name="x", type="space"), 
        list(name="y", type="space"))
    if (type == "array") {
        # yx for labels
        ax <- rev(ax)
        # cyx for images
        if (!label) ax <- c(list(list(name="c", type="channel")), ax)
    }
    # transformations:
    ct <- trans %||% .default_ct(ax)
    # .zattrs list:
    if (type == "array") {
        # default structure
        res <- list(
            omero=list(channels=list(label=letters[seq_len(n)])),
            multiscales=list(list(
                axes=ax,
                version="0.4",
                coordinateTransformations=ct,
                datasets=list(list(path="0", coordinateTransformations=list(list(type="scale", scale=list(1, 1))))))))
        if (ver == "0.3") res <- list(ome=res)
    } else {
        # points/shapes
        res <- list(axes=ax, coordinateTransformations=ct)
    }
    res$spatialdata_attrs <- list(version=ver)
    Zattrs(res)
}

# Internal helper to generate OME-NGFF axes
.default_ax <- \(type=c("array", "frame")) {
    switch(match.arg(type),
        # cyx for images/labels
        array=list(
            list(name="c", type="channel"),
            list(name="y", type="space"),
            list(name="x", type="space")),
        # xy for points/shapes
        list(
            list(name="x", type="space"),
            list(name="y", type="space")))
}

# Internal helper to generate coordinate transformations
.default_ct <- \(axes, name="global", type="identity", data=NULL) {
    ct <- list(input=axes, output=list(name=name), type=type)
    if (!is.null(data)) ct[[type]] <- data
    list(ct)
}

#' @export
#' @importFrom utils .DollarNames
.DollarNames.Zattrs <- \(x, pattern="") names(x)

#' @rdname Zattrs
#' @exportMethod $
setMethod("$", "Zattrs", \(x, name) x[[name]])

# internal use only!
#' @noRd 
.zv <- \(x) {
    v <- x$spatialdata_attrs$version
    if (!length(v)) stop("couldn't find 'version' in 'spatialdata_attrs'")
    ok <- length(v) == 1 && is.character(v) && v %in% sprintf("0.%d", seq_len(5))
    if (!ok) stop("invalid 'version' in 'spatialdata_attrs'; expected '0.x' where x is 1-5")
    return(v)
}

# internal use only!
#' @noRd 
.ms <- \(x) switch(.zv(x), "0.3"=x$ome$multiscales, x$multiscales)

# internal use only!
#' @noRd 
.ch <- \(x) {
    if (.zv(x) == "0.3") x <- x$ome
    unlist(x$omero$channels)
}

# internal use only!
#' @noRd 
setMethod("multiscales", "list", .ms)

#' @export
setMethod("channels", "Zattrs", \(x, ...) .ch(x))
