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
#' point(y, "wide") <- scale(point(y), c(1, 1.2))
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
#' shape(y, "high") <- scale(shape(y), c(1.2, 1))
#' shape(y, "left") <- translation(shape(y), c(-5, 0))
#' 
#' graph::plot(CTgraph(y))
NULL

# image ----

#' @rdname trans
#' @export
setMethod("scale", c("ImageArray", "numeric"), \(x, j, t, ...) {
    stopifnot(length(t) == 3, t > 0)
    if (all(t == 1)) return(x)
    if (is.numeric(j)) j <- CTname(x)
    j <- match.arg(j, CTname(x))
    addCT(x, name=j, type="scale", data=t)
})

# label ----

#TODO

# point ----

#' @rdname trans
#' @importFrom dplyr mutate
#' @export
setMethod("scale", c("PointFrame", "numeric"), \(x, t, ...) {
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
    y <- .y <- .x <- NULL # R CMD check
    R <- .R(t*pi/180)
    x@data <- x@data |>
        mutate(.x=x*R[1,1], .y=y*R[1,2]) |>
        mutate(x=.x+.y) |>
        mutate(.x=x*R[2,1], .y=y*R[2,2]) |>
        mutate(y=.x+.y) |>
        select(-.x, -.y)
    return(x)
})

#' @rdname trans
#' @importFrom dplyr mutate select
#' @export
setMethod("translation", c("PointFrame", "numeric"), \(x, t, ...) {
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x+t[1]) |>
        mutate(y=y+t[2])
    return(x)
})

# shape ----

#' @rdname trans
#' @importFrom sf st_as_sf st_coordinates
#' @export
setMethod("scale", c("ShapeFrame", "numeric"), \(x, t, ...) {
    stopifnot(is.numeric(t), length(t) == 2, t > 0, all(is.finite(t)))
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
.R <- function(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

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

# transform 'ShapeFrame' by realizing,
# and updating 'sf' geometry coordinates
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