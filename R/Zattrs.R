#' @name Zattrs
#' @title The `Zattrs` class
#' 
#' @aliases feature_key
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

#' @export
setMethod("feature_key", "Zattrs", \(x) x$spatialdata_attrs$feature_key)

#' @export
setMethod("feature_key", "SpatialDataElement", \(x) feature_key(meta(x)))