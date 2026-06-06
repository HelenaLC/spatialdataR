#' @name CTutils
#' @title Coord. trans. utilities
#' @aliases axes CTlist CTname CTtype CTdata addCT rmvCT
#' 
#' @param x \code{SpatialData}, an element, or \code{SpatialDataAttrs}.
#' @param y NULL (default) returns a list where each element is 
#'   an axis: a list with name/type/unit (e.g., x/space/micrometer);
#'   \code{y="name/type/unit"} extracts specific data over all axiis.
#' @param i for \code{CTpath}, source node label; else, string or 
#'   scalar integer giving the name or index of a coordinate space.
#' @param name character(1); name of coordinate space
#' @param type character(1); type of transformation
#' @param data transformation data; size and shape depend on transformation and
#'   element type (e.g., numeric(1) for rotation, numeric(2) for scaling in 2D)
#' @param ... option arguments passed to and from other methods.
#' 
#' @returns
#' \itemize{
#' \item \code{CTname}: character string; 
#'   transformation name (e.g., "global")
#' \item \code{CTtype}: character string; 
#'   transformation type (e.g., "affine")
#' \item \code{CTdata}: list;
#'   transformation data (e.g., scalar numeric for rotation)
#' \item \code{CTlist}: list;
#'   list of transformation specifications per OME-NGFF spec
#' \item \code{add/rmvCT}: 
#'   \code{SpatialDataElement} or \code{SpatialDataAttrs} 
#'   with transformation(s) added/removed
#' \item \code{axes}: list; 
#'   each element is a character string (name), or list 
#'   with axis name and type (e.g., "space" or "channel")
#' }
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # view available target coordinate systems
#' CTname(z <- meta(label(x)))
#'
#' # add
#' addCT(z, "scale", "scale", c(12, 34)) # overwrite
#' CTname(addCT(z, "new", "translation", c(12, 34)))
#' 
#' # rmv
#' CTname(rmvCT(z, 2))        # by index
#' CTname(rmvCT(z, "scale"))  # by name
#' CTname(rmvCT(z, "global")) # identity is protected
NULL

# axes() ----

#' @rdname CTutils
#' @export
setMethod("axes", "SpatialDataAttrs", \(x, y=NULL, ...) {
    x <- .get_ms(x)$axes
    if (is.null(x)) stop("couldn't find 'axes'") 
    if (is.null(y)) return(x)
    y <- match.arg(y, c("name", "type", "unit"))
    vapply(x, `[[`, character(1), y)
})

# CTlist/data/type/name() ----

#' @rdname CTutils
#' @export
setMethod("CTlist", "SpatialDataAttrs", \(x, ...) {
    ct <- "coordinateTransformations"
    .get_ms(x)[[ct]]
})

#' @rdname CTutils
#' @export
setMethod("CTdata", "SpatialDataAttrs", \(x, i=1, ...) {
    i <- .val_id(i, CTname(x))
    t <- CTtype(x)[i]
    if (t != "sequence") 
        return(CTlist(x)[[i]][[t]])
    ts <- CTlist(x)[[i]]$transformations
    names(ts) <- vapply(ts, \(.) .$type, character(1))
    mapply(x=ts, i=names(ts), \(x, i) x[[i]], SIMPLIFY=FALSE)
})

#' @rdname CTutils
#' @export
setMethod("CTtype", "SpatialDataAttrs", \(x, ...) {
    vapply(CTlist(x), \(.) .$type, character(1))
})

#' @rdname CTutils
#' @export
setMethod("CTname", "SpatialDataAttrs", \(x, ...) {
    vapply(CTlist(x), \(.) .$output$name, character(1))
})

# SpatialDataElement ----

#' @rdname CTutils
#' @export
setMethod("axes", "SpatialDataElement", \(x, y=NULL, ...) axes(meta(x), y, ...))

#' @rdname CTutils
#' @export
setMethod("CTlist", "SpatialDataElement", \(x, ...) CTlist(meta(x), ...))

#' @rdname CTutils
#' @export
setMethod("CTtype", "SpatialDataElement", \(x, ...) CTtype(meta(x), ...))

#' @rdname CTutils
#' @export
setMethod("CTname", "SpatialDataElement", \(x, ...) CTname(meta(x), ...))

#' @rdname CTutils
#' @export
setMethod("CTdata", "SpatialDataElement", \(x, i=1, ...) CTdata(meta(x), i, ...))

#' @rdname CTutils
#' @export
setMethod("CTname", "SpatialData", \(x, ...) {
    g <- CTgraph(x)
    t <- nodeData(g, nodes(g), "type")
    names(t)[unlist(t) == "space"]
})

# rmv ----

#' @rdname CTutils
#' @export
setMethod("rmvCT", "SpatialDataElement", 
    \(x, i) { meta(x) <- rmvCT(meta(x), i); x })

#' @rdname CTutils
#' @export
setMethod("rmvCT", "SpatialDataAttrs", \(x, i) {
    nms <- CTname(x)
    if (is.numeric(i)) {
        if (any(i > length(nms)))
            stop("invalid 'i'")
        i <- nms[i]
    }
    nan <- setdiff(i, nms)
    if (length(nan)) stop(
        "couldn't find 'coordTrans' of name(s) ", 
        paste(dQuote(nan), collapse=","))
    i <- match(i, nms)
    # protect against dropping identity
    i <- i[CTtype(x)[i] != "identity"]
    if (!length(i)) {
        warning("can't drop identity")
        return(x)
    }
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    if (length(i)) {
        if (is.null(x[[ms]])) {
            x[[ct]] <- x[[ct]][-i]
        } else {
            y <- x[[ms]][[1]][[ct]][-i]
            x[[ms]][[1]][[ct]] <- y
        }
    }
    return(x)
})

# add ----

#' @rdname CTutils
#' @export
setMethod("addCT", "SpatialDataElement", 
    \(x, name, type="identity", data=NULL) {
    meta(x) <- addCT(meta(x), name, type, data); x })

.check_ct <- \(x, type, data) {
    d <- length(axes(x))
    f <- \(t) stop("invalid 'data' for transformation of 'type' ", dQuote(t))
    t <- match.arg(type, c("identity", "scale", "rotate", "translation", "affine"))
    . <- switch(t, 
        identity=is.null(data),
        translation=length(data) == d & is.numeric(data),
        rotate=length(data) == 1 & is.numeric(data) & data > 0,
        scale=length(data) == d & is.numeric(unlist(data)) & all(unlist(data) > 0),
        TRUE)
    if (!.) f(t)
}

#' @rdname CTutils
#' @export
setMethod("addCT", "SpatialDataAttrs", \(x, name, type="identity", data=NULL) {
    stopifnot(
        is.character(name), length(name) == 1,
        is.character(type), length(type) == 1)
    .check_ct(x, type, data)
    # use existing as skeleton
    new <- .default_ct(axes(x))[[1]]
    new$type <- type
    new$output$name <- name
    new[[new$type]] <- data
    # append/overwrite
    old <- CTlist(x)
    i <- match(name, CTname(x))
    if (is.na(i)) {
        new <- c(old, list(new))
    } else {
        old[[i]] <- new
        new <- old
    }
    # update .zattrs
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    if (is.null(multiscales(x))) {
        x[[ct]] <- new
    } else {
        switch( 
            tryCatch(.ome_ver(x), error=\(e) "9.9"), 
            "0.3"=x$ome[[ms]][[1]][[ct]] <- new,
            x[[ms]][[1]][[ct]] <- new)
    }
    return(x)
})
