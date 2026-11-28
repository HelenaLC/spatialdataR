#' @name CTutils
#' @title Coord. trans. utilities
#' @aliases axes CTlist CTname CTtype CTunit CTdata addCT rmvCT
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
#' \item \code{CTunit}: list of lists;
#'   for each transformation, the unit of each axis
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
#' CTname(y <- label(x))
#'
#' # add
#' CTname(addCT(y, "new", "translation", c(12, 34))) 
#' CTdata(addCT(y, "scale", "scale", c(12, 34)), "scale") # overwrite
#' 
#' # rmv
#' CTname(rmvCT(y, 2))        # by index
#' CTname(rmvCT(y, "scale"))  # by name
#' CTname(rmvCT(y, "global")) # identity is protected
NULL

# axes() ----

#' @rdname CTutils
#' @export
setMethod("axes", "SpatialDataAttrs", \(x, y=NULL, ...) {
    x <- .get_ms(x)$axes
    if (is.null(x)) stop("couldn't find 'axes'") 
    if (is.null(y)) return(x)
    y <- match.arg(y, c("name", "type", "unit"))
    # shape/point axes have no type and unit, return axes names as given
    if (y == "name" && is.null(names(x[[1]]))) {
        unlist(x)
    } else {
        vapply(x, `[[`, character(1), y)
    }
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

#' @rdname CTutils
#' @importFrom stats setNames
#' @export
setMethod("CTunit", "SpatialDataAttrs", \(x, ...) {
    lapply(CTlist(x), \(.) {
        ax <- .$output$axes
        setNames(
            lapply(ax, \(.) .$unit),
            vapply(ax, \(.) .$name, character(1)))
    }) |> setNames(CTname(x))
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
setMethod("CTunit", "SpatialDataElement", \(x, ...) CTunit(meta(x), ...))

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
        if (is.null(multiscales(x))) {
            x[[ct]] <- x[[ct]][-i]
        } else {
            .ome_root(x)[[ms]][[1]][[ct]] <- .ome_root(x)[[ms]][[1]][[ct]][-i]
        }
    }
    return(x)
})

# add ----

#' @rdname CTutils
#' @export
setMethod("addCT", "SpatialDataElement", 
    \(x, name, type="identity", data=NULL) {
    stopifnot(
        is.character(name), length(name) == 1,
        is.character(type), length(type) == 1)
    y <- meta(x)
    .check_ct(y, type, data)
    dim <- length(dim(x))
    axes <- .default_ax(
        type=. <- tolower(sub("^SpatialData", "", class(x)[1])),
        dim=if (. == "image") dim-1 else dim) # ignore 'channel'
    new <- .default_ct(axes)[[1]]
    new$type <- type
    new$output$name <- name
    new[[new$type]] <- data
    # append/overwrite
    old <- CTlist(y)
    i <- match(name, CTname(y))
    if (is.na(i)) {
        new <- c(old, list(new))
    } else {
        old[[i]] <- new
        new <- old
    }
    # update .zattrs
    ms <- "multiscales"
    ct <- "coordinateTransformations"
    if (is.null(multiscales(y))) {
        y[[ct]] <- new
    } else {
        .ome_root(y)[[ms]][[1]][[ct]] <- new
    }
    meta(x) <- y
    x
})

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
