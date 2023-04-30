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

#' @importFrom EBImage abind resize
.scale <- function(x, t) {
    a <- as.array(x)
    d <- length(dim(a))
    stopifnot(is.numeric(t), length(t) == d)
    y <- apply(a, 1, \(.)
        resize(., nrow(.)*t[d-1], ncol(.)*t[d]),
        simplify=FALSE)
    y <- abind(y, along=0)
}

#' @importFrom EBImage rotate
.rotate <- function(x, t) {
    stopifnot(
        is.numeric(t),
        length(t) == 1)
    if (is(x, "ShapeFrame")) {
        t <- t*pi/180
        R <- matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)
        lapply(x$data, \(xy) (xy %*% R))
    } else {
        a <- as.array(x)
        y <- apply(a, 1, rotate, t, simplify=FALSE)
        y <- abind(y, along=0)
    }
}

.translate <- function(x, t) {
    stopifnot(
        is.numeric(t),
        length(t) == 2,
        round(t) == t)
    a <- as.array(x)
    y <- apply(a, 1, translate, t, simplify=FALSE)
    y <- abind(y, along=0)
}

#' @rdname ZarrArray
#' @export
setMethod("scaleElement", "ZarrArray",
    function(x, t=rep(1, length(dim(x)))) {
        y <- .scale(x, t)
        fun <- get(class(x))
        fun(y, metadata(x))
    }
)
#' @rdname ZarrArray
#' @export
setMethod("scaleElement", "ShapeFrame",
    function(x, t=c(1, 1)) {
        y <- .scale(x, t)
        y <- asplit(y, 1)
        x$data <- y
        return(x)
    }
)

#' @rdname ZarrArray
#' @export
setMethod("rotateElement", "ZarrArray",
    function(x, t=0) {
        y <- .rotate(x, t)
        fun <- get(class(x))
        fun(y, metadata(x))
    }
)
#' @rdname ZarrArray
#' @export
setMethod("rotateElement", "ShapeFrame",
    function(x, t=0) {
        x$data <- .rotate(x, t)
        ShapeFrame(x, metadata(x))
    }
)

#' @rdname ZarrArray
#' @importFrom EBImage abind translate
#' @export
setMethod("translateElement", "ZarrArray",
    function(x, t=c(0,0)) {
        y <- .translate(x, t)
        fun <- get(class(x))
        fun(y, metadata(x))
    }
)

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
setMethod("transformElement", "ZarrArray", function(x, coord=NULL) .transform(x, coord))

#' @rdname ZarrArray
#' @export
setMethod("transformElement", "ShapeFrame", function(x, coord=NULL) .transform(x, coord))

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
