# All of the below is designated to .zattrs handling,
# in order to guarantee that we can eventually produce
# a valid OME-Zarr store with the same specs as pySpatialData.
# Ideally, if OME specs change, only this part of the code will
# have to be adapted (with potentially version-specific methods).

# this class is defined solely for the purpose
# of validity checks & method dispatching...
#' @rdname SpatialData
#' @export
Zattrs <- \(x=list()) {
    y <- .Zattrs(x)
    if (length(x)) 
        attr(y, "names") <- names(x)
    return(y)
}

.validityZattrs <- \(object) {
    msg <- NULL
    # TODO: write specs checker so that we don't accidentally do
    # anything stupid & can eventually write out a valid file...
    
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    df <- if (is.null(ms <- object[[ms]]))
        object[[ct]] else ms[[ct]][[1]]
    for (fd in split(df, seq_len(nrow(df)))) {
        stopifnot(
            is.character(fd$type), 
            fd$type %in% c(
                "identity", "sequence", "affine", 
                "scale", "rotate", "translation"),
            is.data.frame(ip <- fd$input), 
            is.data.frame(op <- fd$output),
            names(ip) == c("axes", "name"),
            names(op) == c("axes", "name"),
            is.character(ip$name), 
            is.character(op$name))
    }
    
    if (is.null(msg)) TRUE else msg
}
setValidity("Zattrs", .validityZattrs)

# get ----

#' @importFrom SingleCellExperiment int_metadata
setMethod("zattrs", "SingleCellExperiment", \(x) int_metadata(x)$zattrs)
setMethod("zattrs", "SpatialDataElement", \(x) x@zattrs)

# set ----

setReplaceMethod("zattrs",
    c("ZarrArray_OR_ShapeFrame", "Zattrs"),
    \(x, value) { x@zattrs <- value; x })

setReplaceMethod("zattrs",
    c("ZarrArray_OR_ShapeFrame", "list"),
    \(x, value) `zattrs<-`(x, Zattrs(value)))
