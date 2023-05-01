.coords <- function(x) {
    md <- metadata(x)
    df <- if (!is.null(md$multiscales)) {
        md$multiscales$coordinateTransformations[[1]]
    } else {
        md$coordinateTransformations
    }
    data <- lapply(seq(nrow(df)), \(.)
        ifelse(df$type[.] == "identity",
            list(NA), I(df[., df$type[.]])))
    DataFrame(
        input.name=df$input$name,
        output.name=df$output$name,
        input.axes=I(df$input$axes),
        output.axes=I(df$output$axes),
        type=df$type,
        data=I(unlist(data, recursive=FALSE)))
}

.coord <- function(x, name) {
    df <- coords(x)
    if (is.null(name))
        name <- df$output.name[1]
    if (is.character(name)) {
        idx <- match(name, df$output.name)
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
    fun(y, metadata(x))
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
    fun(y, metadata(x))
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
    y <- apply(a, 1, \(.)
        resize(., nrow(.)*t[d-1], ncol(.)*t[d]),
        simplify=FALSE)
    y <- abind(y, along=0)
    fun <- get(class(x))
    fun(y, metadata(x))
}
.scaleShapeFrame <- function(x, t) {
    a <- as.array(x)
    a[, 1] <- a[, 1]*t[1]
    a[, 2] <- a[, 2]*t[2]
    n <- vapply(x$data, nrow, integer(1))
    i <- rep.int(x$index, n)
    i <- split(seq(nrow(a)), i)
    y <- lapply(i, \(.) a[., ])
    x$data <- y
    return(x)
}
.scale <- function(x, t) {
    d <- length(dim(x))
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be greater than zero"=t > 0)
    fun <- if (is(x, "ZarrArray")) {
        d <- length(channels(x))
        if (length(t) != d)
            stop("length of 't' (", length(t), ") should be",
                " the same as the number of channels (", d, ")")
        .scaleZarrArray
    } else {
        stopifnot("'t' should be of length 2"=length(t) == 2)
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
    t <- df$data[[1]]
    switch(
        df$type,
        "identity"=x,
        "scale"=scaleElement(x, t),
        "rotate"=rotateElement(x, t),
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
setMethod("alignElements", "ANY", function(..., coord=NULL) {
    x <- list(...)
    c <- lapply(x, \(.) coords(.)$output.name)
    if (is.null(coord)) {
        # if unspecified, default to using
        # first shared coordinate system
        coord <- Reduce(intersect, c)[1]
    } else {
        # otherwise, check that elements
        # share input coordinate system
        valid <- vapply(c, \(.) coord %in% c, logical(1))
        if (!all(valid)) stop(
            "input element don't share a '",
            coord, "' coordinate system")
    }
    lapply(x, transformElement, coord)
})
