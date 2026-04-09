#' @name trans
#' @rdname trans
#' @title Transformations
#' @aliases scale rotate translation
#' 
#' @param x \code{SpatialData} element
#' @param j scalar character or numeric; 
#'   name or index of coordinate space.
#' @param t transformation data.
#' @param ... option arguments passed to and from other methods.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' # image
#' y <- x
#' image(y) <- scale(image(y), 1, c(1, 1, 1/3))
#' CTpath(image(x), "global")
#' CTpath(image(y), "global")
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

# array ----

#' @importFrom methods as
#' @importFrom S4Vectors metadata<-
.trans_a <- \(x, f) {
    a <- f(aperm(as.array(data(x))))
    x@data <- list(as(aperm(a), "SparseArray"))
    metadata(x) <- list(data_type=data_type(x))
    return(x)
}

#' @importFrom EBImage resize
setMethod("scale", c("sdArray", "numeric"), \(x, t, ...) {
    stopifnot(length(t) == length(dim(x)), is.finite(t), t > 0)
    if (all(t == 1)) return(x)
    d <- length(dim(a))
    f <- \(.) resize(.,
        w=dim(.)[d]*t[d],
        h=dim(.)[d-1]*t[d-1])
    .trans_a(x, f)
})

#' @importFrom EBImage rotate
setMethod("rotate", c("sdArray", "numeric"), \(x, t, ...) {
    # negate angle since 'EBImage' rotates clockwise
    stopifnot(length(t) == 1, is.finite(t))
    if (t %% 360 == 0) return(x)
    f <- \(.) rotate(., -t) 
    .trans_a(x, f)
})

#' @importFrom EBImage translate
setMethod("translation", c("sdArray", "numeric"), \(x, t, ...) {
    stopifnot(length(t) == length(dim(x)), is.finite(t))
    if (all(t == 0)) return(x)
    f <- \(.) translate(., t[-1])
    .trans_a(x, f)
})

# point ----

#' @rdname trans
#' @importFrom dplyr mutate
#' @export
setMethod("scale", c("PointFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, t > 0, is.finite(t))
    if (all(t == 1)) return(x)
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x*t[1]) |>
        mutate(y=y*t[2])
    return(x)
})

#' @rdname trans
#' @importFrom dplyr mutate select
#' @export
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

# utils ----

# rotation matrix to rotate points counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), sin(t), -sin(t), cos(t)), 2, 2)

# count occurrences of each coordinate space;
# return most frequent (in order of appearance)
.guess_space <- \(x) {
    nms <- lapply(rownames(x), \(l) 
        lapply(colnames(x)[[l]], \(e) 
            CTname(x[[l]][[e]])))
    cnt <- table(nms <- unlist(nms))
    cnt <- cnt[unique(nms)]
    names(which.max(cnt))
}

.trans_xy <- \(xy, ts, rev=FALSE) {
    if (rev) ts <- rev(ts)
    for (. in seq_along(ts)) {
        t <- ts[[.]]$type
        d <- ts[[.]]$data
        d <- unlist(d)
        if (length(d) == 3)
            d <- d[-1]
        switch(t, 
            identity={},
            scale={
                op <- ifelse(rev, `/`, `*`)
                xy$x <- op(xy$x, d[2])
                xy$y <- op(xy$y, d[1])
            },
            rotate={
                xy <- xy %*% .R(d*pi/180)
            },
            translation={
                op <- ifelse(rev, `-`, `+`)
                xy$x <- op(xy$x, d[2])
                xy$y <- op(xy$y, d[1])
            })
    }
    return(xy)
}
