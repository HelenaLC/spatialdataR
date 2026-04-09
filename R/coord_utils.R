#' @name coord-utils
#' @title Coordinate transformations
#' @aliases axes CTlist CTname CTtype CTdata addCT rmvCT
#' 
#' @param x \code{SpatialData}, an element, or \code{Zattrs}.
#' @param i for \code{CTpath}, source node label; else, string or 
#'   scalar integer giving the name or index of a coordinate space.
#' @param j character string; name of target coordinate space.
#' @param name character(1); name of coordinate space
#' @param type character(1); type of transformation
#' @param data transformation data; size and shape depend on transformation and
#'   element type (e.g., numeric(1) for rotation, numeric(2) for scaling in 2D)
#' @param ... option arguments passed to and from other methods.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
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

# TODO: currently applying transformations only on 'data.frame's for plotting,
# not the actual data (e.g., image)... but this might be necessary for queries?

# TODO: for all layers, implement all transformations 
# (translate, scale, rotate, affine, and sequential)

# axes() ----

#' @rdname coord-utils
#' @export
setMethod("axes", "Zattrs", \(x, ...) {
    if (!is.null(ms <- x$multiscales)) x <- ms[[1]]
    if (is.null(x <- x$axes)) stop("couldn't find 'axes'") 
    return(x)
})

#' @rdname coord-utils
#' @export
setMethod("axes", "SpatialDataElement", \(x, ...) axes(meta(x)))

# CTlist/data/type/name() ----

#' @rdname coord-utils
#' @export
setMethod("CTlist", "Zattrs", \(x, ...) {
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    if (is.null(x[[ms]])) return(x[[ct]])
    x[[ms]][[1]][[ct]]
})

#' @rdname coord-utils
#' @export
setMethod("CTdata", "Zattrs", \(x, i=1, ...) {
    stopifnot(length(i) == 1)
    if (is.character(i)) {
        match.arg(i, CTname(x))
        i <- match(i, CTname(x))
    } else if (is.numeric(i)) {
        stopifnot(
            i == round(i), 
            i %in% seq_along(CTlist(x)))
    } else stop("Invlid 'i'; should be a scalar character or integer")
    t <- CTtype(x)[i]
    if (t != "sequence") 
        return(CTlist(x)[[i]][[t]])
    ts <- CTlist(x)[[i]]$transformations
    names(ts) <- vapply(ts, \(.) .$type, character(1))
    mapply(x=ts, i=names(ts), \(x, i) x[[i]], SIMPLIFY=FALSE)
})

#' @rdname coord-utils
#' @export
setMethod("CTtype", "Zattrs", \(x, ...) vapply(CTlist(x), \(.) .$type, character(1)))

#' @rdname coord-utils
#' @export
setMethod("CTname", "Zattrs", \(x, ...) vapply(CTlist(x), \(.) .$output$name, character(1)))

#' @rdname coord-utils
#' @export
setMethod("CTlist", "SpatialDataElement", \(x, ...) CTlist(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTdata", "SpatialDataElement", \(x, i=1, ...) CTdata(meta(x), i))

#' @rdname coord-utils
#' @export
setMethod("CTtype", "SpatialDataElement", \(x, ...) CTtype(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTname", "SpatialDataElement", \(x, ...) CTname(meta(x)))

#' @rdname coord-utils
#' @export
setMethod("CTname", "SpatialData", \(x, ...) {
    g <- CTgraph(x)
    t <- nodeData(g, nodes(g), "type")
    names(t)[unlist(t) == "space"]
})

# rmv ----

#' @rdname coord-utils
#' @export
setMethod("rmvCT", "SpatialDataElement", 
    \(x, i) { x@meta <- rmvCT(meta(x), i); x })

#' @rdname coord-utils
#' @export
setMethod("rmvCT", "Zattrs", \(x, i) {
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

#' @rdname coord-utils
#' @export
setMethod("addCT", "SpatialDataElement", \(x, name, type, data) {
    x@meta <- addCT(meta(x), name, type, data); x })

.check_ct <- \(x, type, data) {
    d <- length(axes(x))
    f <- \(t) stop("invalid 'data' for transformation of 'type' ", dQuote(t))
    t <- match.arg(type, c("identity", "scale", "rotate", "translation", "affine"))
    . <- switch(t, 
        identity=is.null(data),
        translation=length(data) == d & is.numeric(data),
        rotate=length(data) == 1 & is.numeric(data) & data > 0,
        scale=length(data) == d & is.numeric(data) & all(data > 0),
        TRUE)
    if (!.) f(t)
}

#' @rdname coord-utils
#' @export
setMethod("addCT", "Zattrs", \(x, name, type="identity", data=NULL) {
    stopifnot(
        is.character(name), length(name) == 1,
        is.character(type), length(type) == 1)
    .check_ct(x, type, data)
    # use existing as skeleton
    old <- CTlist(x)
    new <- old[[1]][c("input", "output", "type")]
    new$type <- type
    new$output$name <- name
    new[[new$type]] <- list(data)
    # append/overwrite & stash
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    i <- match(name, CTname(x))
    if (is.na(i)) {
        new <- c(old, list(new))
    } else {
        old[[i]] <- new
        new <- old
    }
    if (is.null(x[[ms]])) {
        x[[ct]] <- new
    } else {
        x[[ms]][[1]][[ct]] <- new
    }
    return(x)
})

# Dec 8 VJC -- why is this in doc comment mode?  changing to plain comment
# # @importFrom EBImage resize
# setMethod("scale", "ImageArray", \(x, t, ...) {
#     a <- as.array(data(x)) 
#     # TODO: this should be done w/o realizing 
#     # into memory, but EBImage needs an array?
#     d <- length(dim(a))
#     if (missing(t)) 
#         t <- rep(1, d)
#     b <- resize(aperm(a),
#         w=dim(a)[d]*t[d],
#         h=dim(a)[d-1]*t[d-1])
#     x@data <- aperm(b)
#     x
# })
# 
# # @importFrom EBImage resize
# setMethod("translation", "ImageArray", \(x, t, ...) {})
# setMethod("transform", "ImageArray", \(x, t) get(t$type)(x, unlist(t[[t$type]])))
