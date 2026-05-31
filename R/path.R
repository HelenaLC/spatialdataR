#' @name path
#' @title Retrieve \code{SpatialData} on-disk paths
#' 
#' @param object \code{\link{SpatialData}} object or one of its elements.
#' @param simplify logical scalar; whether to flatten paths into a tibble.
#' @param ... ignored.
#' 
#' @returns
#' for single elements, a character string; 
#' for \link{SpatialData} objects, if \code{simplify=TRUE} (default), 
#' a \code{tibble} where rows=elements and columns=layers/elements/paths. 
#' if \code{simplify=FALSE}, a depth-3 list where levels=layers/elements/paths.
#' 
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' sd <- readSpatialData(zs)
#' 
#' # element-wise
#' path(shape(sd))
#' 
#' # object-wide
#' path(sd)
#' path(sd, FALSE)$labels
#' 
#' @importFrom BiocGenerics path
NULL

#' @export
#' @rdname path
#' @importFrom ZarrArray path
setMethod("path", "SpatialDataArray", \(object, ...) {
    pa <- tryCatch(path(data(object)), error=\(e) NULL)
    if (is.null(pa)) return(NA_character_)
    dirname(pa)
})

#' @export
#' @rdname path
setMethod("path", "SpatialDataFrame", \(object, ...) {
    pa <- attr(data(object), "source_path")
    if (is.null(pa)) return(NA_character_)
    pa
})

#' @export
#' @rdname path
#' @importFrom SingleCellExperiment int_metadata
setMethod("path", "SingleCellExperiment", \(object, ...) {
    pa <- int_metadata(object)$source_path
    if (is.null(pa)) return(NA_character_)
    pa
})

#' @export
#' @rdname path
#' @importFrom dplyr tibble
setMethod("path", "SpatialData", \(object, simplify=TRUE, ...) {
    names(ls) <- ls <- rownames(object)
    ps <- lapply(ls, \(l) {
        names(es) <- es <- names(object[[l]])
        lapply(es, \(e) path(object[[l]][[e]]))
    })

    if (!simplify) return(ps)
    
    do.call(rbind, lapply(names(ps), \(l) 
        do.call(rbind, lapply(names(ps[[l]]), \(e)
            tibble(layer = l, element = e, path = ps[[l]][[e]])
        ))
    ))
})
