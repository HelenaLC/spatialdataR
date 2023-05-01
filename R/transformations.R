# internals ----

setMethod("zattrs", "ZarrArray", function(x) x@zattrs)
setMethod("zattrs", "ShapeFrame", function(x) x@zattrs)
setReplaceMethod("zattrs",
    c("ZarrArray", "list"),
    function(x, value) {
        x@zattrs <- value
        return(x)
    })
setReplaceMethod("zattrs",
    c("ShapeFrame", "list"),
    function(x, value) {
        x@zattrs <- value
        return(x)
    })

.get_ct <- \(x) {
    l <- zattrs(x)
    ms <- l$multiscales
    if (!is.null(ms)) l <- ms
    l$coordinateTransformations[[1]]
}
.set_ct <- \(x, df) {
    l <- zattrs(x)
    ms <- l$multiscales
    if (is.null(ms)) {
        l$coordinateTransformations[[1]] <- df
        zattrs(x) <- l
    } else {
        l$multiscales$coordinateTransformations[[1]] <- df
        zattrs(x) <- l
    }
    return(x)
}
.add_coord <- \(x, name, type, data) {
    df <- coords(x)
    fd <- df[1, ]
    fd$type <- type
    fd$output$name <- name
    fd[[type]] <- list(data)
    # there has to be an easier way than this...
    # but 'data.frame' keeps flattening stuff
    l <- vector("list", nrow(df)+1)
    . <- data.frame(
        input=I(l), output=I(l),
        type=character(1),
        data=character(1))
    .$input <- rbind(df$input, fd$input)
    .$output <- rbind(df$output, fd$output)
    .$type <- c(df$type, fd$type)
    .$data <- c(df[[ncol(df)]], fd[[ncol(fd)]])
    typ <- names(df)[ncol(df)]
    # need an example with different transformations
    # to see what specs actually look like in the wild...
    if (typ != "type") {
        names(.)[ncol(.)] <- typ
    } else {
        .$data <- NULL
    }
    .set_ct(x, .)
}
.rmv_coord <- \(x, name) {
    df <- .get_ct(x)
    idx <- match(name, coords(x)$output$name)
    df <- df[-idx[!is.na(idx)], ]
    .set_ct(x, df)
}

# coords ----

.coords <- function(x) {
    l <- zattrs(x)
    ms <- l$multiscales
    if (is.null(ms)) {
        l$coordinateTransformations
    } else {
        ms$coordinateTransformations[[1]]
    }
}

.coord <- function(x, name) {
    df <- coords(x)
    if (is.null(name))
        name <- df$output$name[1]
    if (is.character(name)) {
        idx <- match(name, df$output$name)
        if (is.na(idx))
            stop("couldn't find coords '", name, "'")
    } else {
        stopifnot(
            is.numeric(name),
            length(name) == 1,
            name == round(name))
        if (name > nrow(df))
            stop("only", nrow(df), "coords available")
            idx <- name
    }
    return(df[idx,])
}

#' @rdname ZarrArray
#' @export
setMethod("coords", "ZarrArray", function(x) .coords(x))

#' @rdname ShapeFrame
#' @export
setMethod("coords", "ShapeFrame", function(x) .coords(x))

#' @rdname ZarrArray
#' @export
setMethod("coord", "ZarrArray", function(x, name=1) .coord(x, name))

#' @rdname ZarrArray
#' @export
setMethod("coord", "ShapeFrame", function(x, name=1) .coord(x, name))

# translation ----

.translateZarrArray <- function(x, t) {
    a <- as.array(x)
    d <- length(dim(a))
    if (d == 2) a <- array(a, c(1, dim(a)))
    y <- apply(a, 1, translate, t, simplify=FALSE)
    y <- abind(y, along=0)
    if (d == 2) y <- y[1, , ]
    fun <- get(class(x))
    fun(y, zattrs(x))
}

.translateShapeFrame <- function(x, t) {
    a <- as.array(x)
    a[, 1] <- a[, 1]+t[2]
    a[, 2] <- a[, 2]+t[1]
    y <- switch(x$type[1],
        circle={
            asplit(a, 1)
        },
        polygon={
            i <- rep.int(x$index, vapply(x$data, nrow, integer(1)))
            i <- split(seq(nrow(a)), i)
            lapply(i, \(.) a[., ])
        }
    )
    x$data <- y
    return(x)
}
.translate <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be of length 2"=length(t) == 2,
        "'t' should be whole numbers"=round(t) == t)
    if (all(t == 0)) return(x)
    fun <- if (is(x, "ZarrArray")) {
        .translateZarrArray
    } else {
        .translateShapeFrame
    }
    fun(x, t)
}

#' @rdname ZarrArray
#' @importFrom EBImage abind translate
#' @export
setMethod("translateElement", "ZarrArray",
    function(x, t=numeric(2)) .translateZarrArray(x, t))

#' @rdname ZarrArray
#' @export
setMethod("translateElement", "ShapeFrame",
    function(x, t=numeric(2)) .translateShapeFrame(x, t))

# rotation ----

.rotateZarrArray <- function(x, t) {
    a <- as.array(x)
    y <- apply(a, 1, rotate, t, simplify=FALSE)
    y <- abind(y, along=0)
    fun <- get(class(x))
    fun(y, zattrs(x))
}
.rotateShapeFrame <- function(x, t) {
    t <- t*pi/180
    R <- matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)
    y <- lapply(x$data, \(xy) (xy %*% R))
    x$data <- y
    return(x)
}

#' @importFrom EBImage rotate
.rotate <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be of length 1"=length(t) == 1)
    if (t == 0) return(x)
    fun <- if (is(x, "ZarrArray")) {
        .rotateZarrArray
    } else {
        .rotateShapeFrame
    }
    fun(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("rotateElement", "ZarrArray",
    function(x, t=0) .rotate(x, t))

#' @rdname ZarrArray
#' @export
setMethod("rotateElement", "ShapeFrame",
    function(x, t=0) .rotate(x, t))

# scaling ----

#' @importFrom EBImage abind resize
.scaleZarrArray <- function(x, t) {
    a <- as.array(x)
    d <- length(dim(a))
    if (length(t) != d) stop("'t' should be of length ", d)
    # TODO: this is slow as hell...
    y <- apply(a, 1, simplify=FALSE, \(.)
        resize(., nrow(.)*t[d-1], ncol(.)*t[d]))
    y <- abind(y, along=0)
    fun <- get(class(x))
    fun(y, zattrs(x))
}
.scaleShapeFrame <- function(x, t) {
    switch(x$type[1],
        circle={
            stopifnot("'t' should be of length 1"=length(t) == 1)
            r <- t*x$radius
            x$radius <- r
        },
        polygon={
            stopifnot("'t' should be of length 2"=length(t) == 2)
            a <- t*as.array(x)
            n <- vapply(x$data, nrow, integer(1))
            i <- rep.int(x$index, n)
            i <- split(seq(nrow(a)), i)
            y <- lapply(i, \(.) a[., ])
            x$data <- y
        }
    )
    return(x)
}
.scale <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be greater than zero"=t > 0)
    if (all(t == 1)) return(x)
    fun <- if (is(x, "ZarrArray")) {
        .scaleZarrArray
    } else {
        .scaleShapeFrame
    }
    fun(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("scaleElement", "ZarrArray",
    function(x, t=rep(1, length(channels(x)))) .scale(x, t))

#' @rdname ZarrArray
#' @export
setMethod("scaleElement", "ShapeFrame",
    function(x, t=c(1, 1)) .scale(x, t))

# transformation ----

.transform <- function(x, coord) {
    df <- coord(x, coord)
    switch(
        df$type,
        "identity"=x,
        "scale"=scaleElement(x, df$data[[1]]),
        "rotate"=rotateElement(x, df$data[[1]]),
        sprintf("transformation of type '%s' yet to be supported.", df$type))
}

#' @rdname ZarrArray
#' @export
setMethod("transformElement", "ZarrArray",
    function(x, coord=NULL) .transform(x, coord))

#' @rdname ZarrArray
#' @export
setMethod("transformElement", "ShapeFrame",
    function(x, coord=NULL) .transform(x, coord))

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
    c <- lapply(x, \(.) coords(.)$output$name)
    if (is.null(coord)) {
        # if unspecified, default to using
        # first shared coordinate system
        coord <- Reduce(intersect, c)[1]
    } else {
        # otherwise, check that elements
        # share input coordinate system
        valid <- vapply(c, \(.) . %in% coord, logical(1))
        if (!all(valid)) stop(
            "input element don't share a '",
            coord, "' coordinate system")
    }
    lapply(x, transformElement, coord)
})
