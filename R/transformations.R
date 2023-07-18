# coords ----

#' @rdname ZarrArray
#' @export
setMethod("coords", "SpatialDataElement",
    function(x) getCoordTrans(x, name=NULL))

#' @rdname ShapeFrame
#' @export
setMethod("coords", "SpatialData",
    function(x) getCoordTrans(x, name=NULL))

#' @rdname ZarrArray
#' @export
setMethod("coord", "SpatialDataElement",
    function(x, name=1) getCoordTrans(x, name))

# transformation ----

#' @rdname ZarrArray
#' @export
setMethod(
    "transformElement",
    "SpatialDataElement",
    function(x, coord=NULL) {
        df <- if (is.data.frame(coord))
            coord else coord(x, coord)
        t <- df[[df$type]][[1]]
        switch(
            df$type,
            "identity"=x,
            "scale"=scaleElement(x, t),
            "rotate"=rotateElement(x, t),
            "translation"=translateElement(x, t),
            "sequence"={
                ts <- df$transformations[[1]]
                for (. in seq_len(nrow(ts))) {
                    x <- transformElement(x, ts[., ])
                }
                return(x)
            },
            paste0("transformation of type '",
                df$type, "' not (yet) supported"))
    }
)

#' @rdname ZarrArray
#' @export
setMethod("axes", "ImageArray", function(x, type=NULL) {
    l <- zattrs(x)
    ax <- l$multiscales$axes[[1]]
    if (is.null(type)) return(ax$name)
    stopifnot(
        "'type' should be a string"=is.character(type),
        "'type' should be of length 1"=length(type) == 1)
    if (!type %in% ax$type) {
        ax <- dQuote(unique(ax$type))
        ax <- paste(ax, collapse=", ")
        stop("'type' should be one of ", ax)
    }
    ax$name[ax$type == type]
})

#' @rdname ZarrArray
#' @export
setMethod("alignElements", "ANY", function(..., coord=NULL) {
    x <- list(...)
    x <- x[!vapply(x, is.null, logical(1))]
    cs <- lapply(x, \(.) coords(.)$output$name)
    if (is.null(coord)) {
        # if unspecified, default to using
        # first shared coordinate system
        coord <- Reduce(intersect, c)[1]
    } else {
        # otherwise, check that elements
        # share input coordinate system
        valid <- vapply(cs, \(.) coord %in% ., logical(1))
        if (!all(valid)) stop(
            "input element don't share a '",
            coord, "' coordinate system")
    }
    lapply(x, transformElement, coord)
})

# spd <- readSpatialData("inst/extdata/merfish")
# tmp <- spd; p <- point(spd)
# q <- p[sample(length(p), 1e3)]
# tmp@points[[1]] <- q
