#' @name trans
#' @rdname trans
#' @title Transformations
#' @aliases scale rotate translation flip flop mirror
#' 
#' @param x \code{SpatialData} element.
#' @param t transformation data; exceptions: for \code{mirror}, controls
#'   whether to perform \bold{v}ertical or \bold{h}orizontal reflection;
#'   no data is needed for \code{flip} (\bold{v}) and \code{flop} (\bold{h}).
#' @param k scalar index specifying which scale to use; 
#'   \code{Inf} to use lowest available resolution;
#'   only applies to \code{sdArray}s (images, labels).
#' @param ... option arguments passed to and from other methods.
#' 
#' @returns \code{SpatialData} element with transformation(s) applied.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # image
#' y <- x
#' image(y) <- scale(image(y), c(1, 1, 1/3))
#' dim(image(x))
#' dim(image(y))
#'   
#' # point
#' y <- x
#' point(y, "rot") <- rotate(point(y), 20)
#' point(y, "wide") <- scale(point(y), c(1.2, 1))
#' 
#' xy0 <- as.data.frame(point(y))
#' xy1 <- as.data.frame(point(y, "rot"))
#' xy2 <- as.data.frame(point(y, "wide"))
#' 
#' plot(xy0[, c(1, 2)], asp=1)
#' points(xy1[, c(1, 2)], col=2)
#' points(xy2[, c(1, 2)], col=4)
#'   
#' # shape
#' y <- x
#' shape(y, "rot") <- rotate(shape(y), 5)
#' shape(y, "wide") <- scale(shape(y), c(1.2, 1))
#' shape(y, "left") <- translation(shape(y), c(-5, 0))
#' y["shapes", c("rot", "wide", "left")]
NULL

# rotation matrix to rotate points counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), sin(t), -sin(t), cos(t)), 2, 2)

# array ----

.mirror <- \(x, t, k=1) {
    d <- length(dim(x)) == 3
    i <- if (d) c(1, 3, 2) else c(2, 1)
    x@data <- list(aperm(data(x, k), i))
    rotate(x, t, k=1)
}

#' @export
#' @rdname trans
setMethod("mirror", "sdArray", \(x, t=c("v", "h"), k=1, ...) 
    switch(match.arg(t), v=flip, h=flop)(x, k))

#' @export
#' @rdname trans
setMethod("flip", "sdArray", \(x, k=1, ...) .mirror(x, -90, k))

#' @export
#' @rdname trans
setMethod("flop", "sdArray", \(x, k=1, ...) .mirror(x, 90, k))

#' @importFrom methods as
#' @importFrom S4Vectors metadata<-
.trans_a <- \(x, f, k=1) {
    a <- f(aperm(as.array(data(x, k))))
    metadata(x)$data_type <- data_type(x)
    x@data <- list(as(aperm(a), "SparseArray"))
    return(x)
}

#' @export
#' @rdname trans
#' @importFrom EBImage resize
setMethod("scale", c("sdArray", "numeric"), \(x, t, k=1, ...) {
    stopifnot(length(t) == length(dim(x)), is.finite(t), t > 0)
    if (all(t == 1)) return(x)
    n <- length(d <- dim(data(x, k)))
    f <- \(.) resize(.,
        w=d[n]*t[n],
        h=d[n-1]*t[n-1])
    .trans_a(x, f, k)
})

#' @export
#' @rdname trans
#' @importFrom EBImage rotate
setMethod("rotate", c("sdArray", "numeric"), \(x, t, k=1,...) {
    # negate angle since 'EBImage' rotates clockwise
    stopifnot(length(t) == 1, is.finite(t))
    if (t %% 360 == 0) return(x)
    f <- \(.) EBImage::rotate(., -t) 
    if (length(d <- dim(data(x, k))) == 3) d <- d[-1]
    metadata(x)$wh <- lapply(rev(d), \(.) c(c(0, .) %*% .R(-t*pi/180)))
    .trans_a(x, f, k)
})

#' @export
#' @rdname trans
#' @importFrom EBImage translate
setMethod("translation", c("sdArray", "numeric"), \(x, t, k=1, ...) {
    stopifnot(length(t) == length(dim(x)), is.finite(t))
    if (all(t == 0)) return(x)
    d <- dim(data(x, k))
    if (length(d) == 3) {
        # protect non-spatial dim.
        t <- t[-1]; d <- d[-1]
    }
    t <- rev(t); d <- rev(d)
    metadata(x)$wh <- list(
        c(t[1], t[1]+d[1]), 
        c(t[2], t[2]+d[2]))
    #f <- \(.) translate(., t, output.dim=d+t)
    #.trans_a(x, f)
    x
})

# point ----

#' @export
#' @rdname trans
#' @importFrom dplyr mutate
setMethod("scale", c("PointFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, t > 0, is.finite(t))
    if (all(t == 1)) return(x)
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x*t[1]) |>
        mutate(y=y*t[2])
    return(x)
})

#' @export
#' @rdname trans
#' @importFrom dplyr mutate select
setMethod("rotate", c("PointFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 1, is.finite(t))
    if (t %% 360 == 0) return(x)
    y <- a <- b <- c <- d <- NULL # R CMD check
    R <- .R(t*pi/180)
    x@data <- x@data |>
        mutate(a=x*R[1,1], b=y*R[1,2]) |>
        mutate(c=x*R[2,1], d=y*R[2,2]) |>
        mutate(x=a+b, y=c+d) |>
        select(-c(a,b, c,d))
    return(x)
})

#' @rdname trans
#' @importFrom dplyr mutate select
#' @export
setMethod("translation", c("PointFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, is.finite(t))
    if (all(t == 0)) return(x)
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x+t[1]) |>
        mutate(y=y+t[2])
    return(x)
})

# shape ----

# TODO: do this w/o realizing
#' @importFrom sf st_as_sf st_geometry st_geometry<-
.trans_s <- \(x, f) {
    y <- st_as_sf(data(x))
    xy <- st_coordinates(y)
    xy <- data.frame(f(xy))
    xy <- st_as_sf(xy, coords=names(xy))
    st_geometry(y) <- st_geometry(xy)
    x@data <- y
    return(x)
}

#' @rdname trans
#' @importFrom sf st_as_sf st_coordinates
#' @export
setMethod("scale", c("ShapeFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, t > 0, is.finite(t))
    .trans_s(x, \(xy) sweep(xy, 2, t, `*`))
})

#' @rdname trans
#' @importFrom sf st_as_sf st_coordinates
#' @export
setMethod("rotate", c("ShapeFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 1, is.finite(t))
    .trans_s(x, \(xy) xy %*% .R(t*pi/180))
})

#' @rdname trans
#' @importFrom sf st_as_sf st_coordinates
#' @export
setMethod("translation", c("ShapeFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, is.finite(t))
    .trans_s(x, \(xy) sweep(xy, 2, t, `+`))
})
