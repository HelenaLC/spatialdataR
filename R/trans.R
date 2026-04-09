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

# image ----

#' @rdname trans
#' @export
setMethod("scale", c("sdArray", "numeric"), \(x, j, t, ...) {
    stopifnot(length(t) == length(dim(x)), t > 0)
    if (all(t == 1)) return(x)
    if (is.numeric(j)) j <- CTname(x)
    j <- match.arg(j, CTname(x))
    addCT(x, name=j, type="scale", data=t)
})

# label ----

#' @rdname trans
#' @importFrom DelayedArray cbind rbind ConstantArray
#' @importFrom methods as
#' @export
setMethod("translation", c("sdArray", "numeric"), \(x, t, ...) {
    stopifnot(
        length(t) == length(dim(x)), 
        t == round(t), all(is.finite(t)))
    if (all(t == 0)) return(x)
    ys <- data(x, NULL)
    if (length(ys) == 1) {
        ts <- list(t)
    } else {
        ds <- vapply(ys, ncol, integer(1))
        sf <- c(1, ds[-1]/ds[-length(ds)])
        ts <- lapply(cumprod(sf), `*`, t)
    } 
    x@data <- lapply(seq_along(ys), \(k) {
        t <- ts[[k]]
        y <- as(ys[[k]], "DelayedArray")
        # TODO: no 'abind' support so that we 
        # permute, 'c/rbind', and back-permute;
        # surely there has to be a better way?
        if (length(dim(y)) == 2) {
            n <- NULL
        } else {
            t <- t[-1]
            n <- dim(x)[1] 
            y <- aperm(y, c(2,3,1))
        }
        if (t[2] != 0) {
            d <- c(nrow(y), abs(t[2]), n)
            z <- ConstantArray(0, dim=d)
            y <- if (t[2] > 0) cbind(z, y) else cbind(y, z)
        }
        if (t[1] != 0) {
            d <- c(abs(t[1]), ncol(y), n)
            z <- ConstantArray(0, dim=d)
            y <- if (t[1] > 0) rbind(z, y) else rbind(y, z)
        }
        if (!is.null(n)) aperm(y, c(3,1,2)) else y
    })
    return(x)
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
