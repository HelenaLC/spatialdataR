# coords ----

#' @rdname ZarrArray
#' @export
setMethod("coords", "ZarrArray", function(x) getCoordTrans(x, name=NULL))

#' @rdname ShapeFrame
#' @export
setMethod("coords", "ShapeFrame", function(x) getCoordTrans(x, name=NULL))

#' @rdname ZarrArray
#' @export
setMethod("coord", "ZarrArray", function(x, name=1) getCoordTrans(x, name))

#' @rdname ZarrArray
#' @export
setMethod("coord", "ShapeFrame", function(x, name=1) getCoordTrans(x, name))

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
            if (length(t) != 1) stop(
                "'t' should be scalar for scaling ",
                "for scaling 'circle' shapes")
            r <- t*x$radius
            x$radius <- r
        },
        polygon={
            if (length(t) != 2) stop(
                "'t' should be of length 2 ",
                "for scaling 'polygon' shapes")
            x$data <- lapply(x$data, \(df) sweep(df, 2, t, `*`))
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

#' @rdname ZarrArray
#' @export
setMethod("transformElement",
    "ZarrArray_OR_ShapeFrame",
    function(x, coord=NULL) {
        df <- coord(x, coord)
        switch(
            df$type,
            "identity"=x,
            "scale"=scaleElement(x, df$data[[1]]),
            "rotate"=rotateElement(x, df$data[[1]]),
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
