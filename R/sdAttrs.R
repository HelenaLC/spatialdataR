#' @name SpatialDataAttrs
#' @title The `SpatialDataAttrs` class
#' 
#' @aliases region region<- 
#' @aliases regions regions<- 
#' @aliases instances instances<- 
#' @aliases region_key region_key<- 
#' @aliases feature_key feature_key<- 
#' @aliases instance_key instance_key<- 
#' 
#' @param x element or list extracted from a OME-NGFF compliant .zattrs file.
#' @param name character string for extraction (see ?base::`$`).
#' @param type character string; either "array" (image/label) or "frame" (point/shape).
#' @param label flag; when \code{type="frame"}, should attributes be for a label?
#' @param trans list of coordinate transformations; defaults to identity only.
#' @param value character string (for one \code{region} and \code{_key}s), 
#'   or vector (for many \code{region}s, \code{instances} and \code{regions}).
#' @param ver character string; specified the .zarr version to comply with.
#' @param nch scalar integer; how many channels should there be?
#'   (ignored unless \code{type="frame"} and \code{label=FALSE}). 
#' @param ... additional attributes (e.g., version, feature_key).
#' 
#' @details 
#' When \code{x} is a spatial element, the following applies:
#' \code{SpatialDataFrame}: \code{feature/instance_key},
#' \code{SingleCellExperiment}: \code{region}, \code{region/instance_key}.
#' 
#' When missing \code{x}, \code{SpatialDataAttrs} will generate a valid object 
#' with default axes (array: cyx, frame: xy) and transformations (identify) 
#' according to the specified type.
#' 
#' @return character string
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#' 
#' # tables
#' region(table(x))
#' region_key(table(x))
#' 
#' # points
#' instance_key(point(x))
#' fk <- feature_key(point(x))
#' base::table(point(x)[[fk]])
#' 
#' # transformations
#' (z <- meta(label(x)))
#' CTname(z)
#' CTtype(z)
#' CTdata(z, "scale")
#' 
#' # constructor
#' SpatialDataAttrs(type="frame")
#' SpatialDataAttrs(type="array")
#' SpatialDataAttrs(type="array", nch=7)
#' SpatialDataAttrs(type="array", label=TRUE)
#' 
#' @export
SpatialDataAttrs <- \(x, type=c("array", "frame"), 
    label=FALSE, trans=NULL, ver="0.4", nch=3, ...) 
{
    if (!missing(x)) return(.SpatialDataAttrs(x))
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
            omero=list(channels=list(label=letters[seq_len(nch)])),
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
    SpatialDataAttrs(res)
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
.DollarNames.SpatialDataAttrs <- \(x, pattern="") names(x)

#' @rdname SpatialDataAttrs
#' @exportMethod $
setMethod("$", "SpatialDataAttrs", \(x, name) x[[name]])

# internal use only!
#' @noRd 
.ome_ver <- \(x) {
    v <- 
        x$multiscales[[1]]$version %||%
        x$omero$version %||% 
        x$ome$version
    if (!length(v)) stop("couldn't find 'version' in 'spatialdata_attrs'")
    ok <- length(v) == 1 && is.character(v) && (v <- gsub("-.*", "", v)) %in% sprintf("0.%d", seq_len(6))
    if (!ok) stop("invalid OME 'version'; expected '0.x' where x is 1-6")
    return(v)
}

# internal use only!
#' @noRd 
setMethod("multiscales", "list", \(x) {
    v <- tryCatch(.ome_ver(x), error=\(e) NULL)
    if (is.null(v)) return()
    switch(v, "0.5"=x$ome$multiscales, x$multiscales)
})

# internal use only!
#' @noRd 
setMethod("datasets", "list", \(x, ...) {
  vapply(multiscales(x)[[1]]$datasets, \(.){
    .$path
  }, character(1))
})

# features ----

#' @export
#' @rdname SpatialDataAttrs
setMethod("feature_key", "SpatialDataPoint", \(x) feature_key(meta(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("feature_key", "SpatialDataAttrs", \(x) x$spatialdata_attrs$feature_key)
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("feature_key", c("SpatialDataAttrs", "character"), 
    \(x, value) { x$spatialdata_attrs$feature_key <- value; x })

# region(s) ----

#' @export
#' @rdname SpatialDataAttrs
setMethod("region_key", "SingleCellExperiment", \(x) meta(x)$region_key)

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) == 1, nchar(value) > 0)
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "NULL"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
setMethod("region", "SingleCellExperiment", \(x) meta(x)[["region"]])

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData
setMethod("regions", "SingleCellExperiment", \(x) {
    rk <- region_key(x)
    if (is.null(rk)) return(NULL)
    int_colData(x)[[rk]]
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(all(nchar(value) > 0, na.rm=TRUE))
    if (is.null(rk <- region_key(x))) 
        rk <- region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x)))
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) %in% c(1, ncol(x)))
    stopifnot(all(nchar(value) > 0, na.rm=TRUE))
    if (is.null(rk <- region_key(x))) region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    int_colData(x)[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x))) {
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
        int_colData(x)[[rk]] <- value
    }
    region_key(x) <- value
    return(x)
})

# instances ----

# NOTE: does not apply to images
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "list", \(x) x$instance_key)
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SingleCellExperiment", \(x) instance_key(meta(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SpatialDataFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instance_key", "SpatialDataLabel", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("instance_key", c("SpatialDataAttrs", "character"), \(x, value) {
    x$spatialdata_attrs$instance_key <- value
    return(x)
})
#' @export
#' @rdname SpatialDataAttrs
setReplaceMethod("instance_key", c("SingleCellExperiment", "character"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$instance_key <- value
    return(x)
})

#' @export
#' @rdname SpatialDataAttrs
setMethod("instances", "SpatialDataLabel", \(x) {
    # unique values in first scale, excluding 0
    z <- data(x, 1)
    as.integer(setdiff(unique(as.vector(z)), 0))
})
#' @export
#' @rdname SpatialDataAttrs
#' @importFrom dplyr pull
setMethod("instances", "SpatialDataPoint", \(x) pull(data(x), instance_key(x)))
#' @export
#' @rdname SpatialDataAttrs
setMethod("instances", "SpatialDataShape", \(x) {
    ik <- tryCatch(instance_key(x), error=\(e) NULL)
    if (is.null(ik)) return(seq_len(nrow(x)))
    pull(data(x), ik)
})
#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData
setMethod("instances", "SingleCellExperiment", \(x) {
    if (is.null(ik <- instance_key(x))) 
        stop("no 'instance_key' found in 'x'")
    int_colData(x)[[ik]]
})

#' @export
#' @rdname SpatialDataAttrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("instances", c("SingleCellExperiment", "ANY"), \(x, value) {
    ik <- instance_key(x)
    if (is.null(ik)) 
        ik <- "instance_id"
    int_colData(x)[[ik]] <- value
    return(x)
})
