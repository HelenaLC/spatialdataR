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

#' @name SDattrs
#' @title \code{SpatialData} attributes
#' 
#' @aliases
#' region
#' regions
#' instances
#' region_key
#' feature_key
#' instance_key
#' 
#' @param x depends on which attributes are available; 
#'   specifically, \code{PointFrame} (\code{feature/instance_key}), or
#'   \code{SingleCellExperiment} (\code{region}, \code{region/instance_key}),
#' 
#' @return character string
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#' 
#' region(table(x))
#' region_key(table(x))
#' 
#' instance_key(point(x))
#' fk <- feature_key(point(x))
#' base::table(point(x)[[fk]])
NULL

# TODO: only points can have this?
#' @export
#' @rdname SDattrs
setMethod("feature_key", "list", \(x) x$spatialdata_attrs$feature_key)
#' @export
#' @rdname SDattrs
setMethod("feature_key", "PointFrame", \(x) feature_key(meta(x)))

# TODO: only tables can have this?
#' @export
#' @rdname SDattrs
setMethod("region_key", "SingleCellExperiment", \(x) meta(x)$region_key)

#' @export
#' @rdname SDattrs
setMethod("region", "SingleCellExperiment", \(x) meta(x)$region)

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData
setMethod("regions", "SingleCellExperiment", \(x) int_colData(x)[[region_key(x)]])

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "character"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$region <- value
    return(x)
})

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "character"), \(x, value) {
    int_colData(x)[[region_key(x)]] <- value
    return(x)
})

# TODO: only tables and points can have this?
#' @export
#' @rdname SDattrs
setMethod("instance_key", "list", \(x) x$instance_key)
#' @export
#' @rdname SDattrs
setMethod("instance_key", "LabelArray", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "PointFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "ShapeFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "SingleCellExperiment", \(x) instance_key(meta(x)))

#' @export
#' @rdname SDattrs
setMethod("instances", "LabelArray", \(x) {
    # unique values in first scale, excluding 0
    z <- data(x, 1)
    as.integer(setdiff(unique(as.vector(z)), 0))
})
#' @export
#' @rdname SDattrs
#' @importFrom dplyr pull
setMethod("instances", "PointFrame", \(x) pull(data(x), instance_key(x)))
#' @export
#' @rdname SDattrs
setMethod("instances", "ShapeFrame", \(x) {
    ik <- tryCatch(instance_key(x), error=\(e) NULL)
    if (is.null(ik)) return(seq_len(nrow(x)))
    pull(data(x), ik)
})
#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData
setMethod("instances", "SingleCellExperiment", \(x) int_colData(x)[[instance_key(x)]])
