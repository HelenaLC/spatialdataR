# utils ----

.whZarrArray <- \(x) {
    md <- metadata(x)
    w <- md$w; h <- md$h
    if (is.null(w) || is.null(h)) {
        d <- dim(x)
        n <- length(d)
        w <- c(0, d[n])
        h <- c(0, d[n-1])
    }
    list(w=w, h=h)
}

# translate ----

#' @importFrom EBImage translate
.translateZarrArray <- function(x, t) {
    md <- metadata(x)
    wh <- .whZarrArray(x)
    md$w <- wh$w+t[length(t)]
    md$h <- wh$h+t[length(t)-1]
    metadata(x) <- md
    return(x)
    # a <- aperm(as.array(x))
    # d <- dim(a)[-3]+t[-1]
    # b <- aperm(translate(a, t[-1], output.dim=d))
    # get(class(x))(b, zattrs=zattrs(x))
}

.translateShapeFrame <- function(x, t) {
    a <- as.array(x)
    a[, 1] <- a[, 1]+t[2]
    a[, 2] <- a[, 2]+t[1]
    y <- switch(x$type[1],
        circle=asplit(a, 1),
        polygon={
            ns <- vapply(x$data, nrow, integer(1))
            i <- rep.int(x$index, ns)
            i <- split(seq(nrow(a)), i)
            lapply(i, \(.) a[., ])
        })
    x$data <- y
    return(x)
}

#' @importFrom dplyr mutate
.translatePointFrame <- function(x, t) {
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x+t[1], y=y+t[2])
    return(x)
}

.translate <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be whole numbers"=round(t) == t)
    # do nothing when all coordinate shifts are 0
    if (all(t == 0)) return(x)
    # otherwise call corresponding method
    c <- ifelse(is(x, za <- "ZarrArray"), za, class(x))
    get(paste0(".translate", c))(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("translateElement", "SpatialDataElement",
    function(x, t=numeric(2)) .translate(x, t))

# scale ----

#' @importFrom EBImage resize
.scaleZarrArray <- function(x, t) {
    d <- length(dim(x))
    if (length(t) != d)
        stop("'t' should be of length ", d)
    y <- as.array(x)
    y <- resize(aperm(y),
        w=dim(y)[d]*t[d],
        h=dim(y)[d-1]*t[d-1])
    y <- aperm(y)
    fun <- get(class(x))
    md <- metadata(x)
    wh <- .whZarrArray(x)
    x0 <- wh$w[1]; y0 <- wh$h[1]
    md$w <- c(x0, x0+dim(x)[d])
    md$h <- c(y0, y0+dim(x)[d-1])
    fun(y, zattrs=zattrs(x), metadata=md)
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

.scalePointFrame <- function(x, t) {
    y <- NULL # R CMD check 
    stopifnot("'t' should be of length 2 for scaling points"=length(t) == 2)
    x@data <- x@data |> mutate(x=x*t[1], y=y*t[2])
    return(x)
}

.scale <- function(x, t) {
    # do nothing if all factors are 0
    if (all(t == 1)) return(x)
    # otherwise call corresponding method
    c <- ifelse(is(x, za <- "ZarrArray"), za, class(x))
    get(paste0(".scale", c))(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("scaleElement",
    "SpatialDataElement",
    function(x, t) {
        if (missing(t)) t <- rep(1, length(dim(x)))
        stopifnot(
            "'t' should be numeric"=is.numeric(t),
            "'t' should be greater than zero"=t > 0)
        .scale(x, t)
    }
)

# rotate ----

# rotation matrix to rotate points
# counter-clockwise through an angle 't'
.R <- function(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

.rotate2d <- function(x, t) {
    y <- .y <- .x <- NULL # R CMD check
    R <- .R(t*pi/180)
    switch(class(x),
        ShapeFrame={
            x@listData$data <- lapply(x$data, \(xy) (xy %*% R))
        },
        PointFrame={
            x@data <- x@data |>
                mutate(.x=x*R[1,1], .y=y*R[1,2]) |>
                mutate(x=.x+.y) |>
                mutate(.x=x*R[2,1], .y=y*R[2,2]) |>
                mutate(y=.x+.y) |>
                select(-.x, -.y)
        })
    return(x)
}

# 'aperm's needed here because channels are ordered as
# tczyx in OME-NGFF, but 'EBImage' needs c to be last
#' @importFrom EBImage rotate
.rotate3d <- function(x, t) {
    y <- as.array(x)
    y <- aperm(rotate(aperm(y), -t, "none"))
    get(class(x))(y, zattrs=zattrs(x))
}

.rotate <- function(x, t) {
    stopifnot(
        "'t' should be numeric"=is.numeric(t),
        "'t' should be of length 1"=length(t) == 1)
    # do nothing when angle's divisible by 360
    if (t %% 360 == 0) return(x)
    # otherwise call corresponding method
    d <- ifelse(is(x, "ZarrArray"), 3, 2)
    get(sprintf(".rotate%sd", d))(x, t)
}

#' @rdname ZarrArray
#' @export
setMethod("rotateElement", "SpatialDataElement",
    function(x, t=0) .rotate(x, t))

# transform ----

#' @rdname ZarrArray
#' @export
setMethod(
    "transformElement",
    "SpatialDataElement",
    function(x, coord=1) {
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
