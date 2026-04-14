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
#' (z <- meta(label(x)))
#' 
#' CTname(z)
#' CTtype(z)
#' CTdata(z, "scale")
#' 
#' feature_key(point(x))
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

# internal use only!
#' @noRd 
setMethod("multiscales", "list", \(x) {
    v <- x$spatialdata_attrs$version
    if (!length(v)) stop("couldn't find 'version' in 'spatialdata_attrs'")
    switch(v, "0.3"=x$ome$multiscales, x$multiscales)
})

.showZattrs <- function(object) {
    cat("class: Zattrs\n")
    ax <- axes(object)
    cat(sprintf("axes(%d):\n", length(ax)))
    if (is.character(ax[[1]])) {
        cat("- name:", unlist(ax), "\n")
    } else {
        cat("- name:", vapply(ax, \(.) .$name, character(1)), "\n")
        cat("- type:", vapply(ax, \(.) .$type, character(1)), "\n")
    }
    # TODO: more detailed 'sequence' display
    cat(sprintf("coordTrans(%d):\n", n <- length(CTname(object))))
    g <- \(.) {
        . <- paste(unlist(.), collapse=",")
        if (!grepl(",", .)) return(.)
        sprintf("[%s]", .)
    }
    f <- \(.) {
        if (is.null(.)) return("")
        paste0(":", g(lapply(., g)))
    }
    for (i in seq_len(n))
        cat(sprintf("- %s: (%s%s)\n",
            CTname(object)[i],
            CTtype(object)[i],
            f(CTlist(object)[[i]][[CTtype(object)[i]]])))
    ms <- object$multiscales[[1]]
    if (!is.null(ms)) {
        ds <- ms$datasets
        ps <- vapply(ds, \(.) .$path, character(1))
        coolcat("datasets(%d): %s\n", ps)
        for (i in seq_along(ds)) {
            ct <- ds[[i]]$coordinateTransformations[[1]]
            cat(sprintf("- %s: (%s:%s)\n", 
                ps[i], ct$type, g(ct[[ct$type]]))) 
        }
    }
    cs <- unlist(channels(object))
    if (!is.null(cs)) coolcat("channels(%d): %s\n", cs)
}
setMethod("show", "Zattrs", .showZattrs)

#' @name SDattrs
#' @title \code{SpatialData} attributes
#' 
#' @aliases
#' region
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
setMethod("region", "SingleCellExperiment", \(x) meta(x)[[region_key(x)]])

#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) > 0)
    int_metadata(x)$spatialdata_attrs$region <- value
    return(x)
})

# TODO: only tables and points can have this?
#' @export
#' @rdname SDattrs
setMethod("instance_key", "list", \(x) x$instance_key)
#' @export
#' @rdname SDattrs
setMethod("instance_key", "PointFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "SingleCellExperiment", \(x) instance_key(meta(x)))
