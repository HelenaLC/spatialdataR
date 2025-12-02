#' @name coord-trans
#' @title Coordinate transformations
#' 
#' @aliases ct_transform ct_translate ct_scale ct_rotate
#' 
#' @param x \code{SpatialData} element.
#' @param i target coordinate system.
#' @param t transformation data.
#' @param ... ignored.
#' 
#' @return Object of same type as input.
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' gg <- sd_plot()
#' 
#' # point
#' 
#' x <- sd@points[[1]]
#' sd@points$a <- ct_translate(x, c(-10, 10))
#' sd@points$b <- ct_scale(x, c(0.5, 1.2))
#' sd@points$c <- ct_rotate(x, 5)
#' 
#' gg + 
#'   sd_plot_point(sd, 1, col=1) + 
#'   sd_plot_point(sd, "a", col=2) + 
#'   sd_plot_point(sd, "b", col=3) + 
#'   sd_plot_point(sd, "c", col=4) 
#' 
#' # shape
#' 
#' x <- sd@shapes[[2]]
#' sd@shapes$a <- ct_translate(x, c(-10, 10))
#' sd@shapes$b <- ct_scale(x, c(0.5, 1.2))
#' sd@shapes$c <- ct_rotate(x, 5)
#' 
#' gg + 
#'   sd_plot_shape(sd, 2, col=1) + 
#'   sd_plot_shape(sd, "a", col=2) + 
#'   sd_plot_shape(sd, "b", col=3) + 
#'   sd_plot_shape(sd, "c", col=4) 
NULL

# transform ----

#' @rdname coord-trans
#' @export
ct_transform <- new_generic("ct_transform", c("x", "i"))

method(ct_transform, list(sdArrayFrame, class_character)) <- \(x, i) {
    i <- match.arg(i, ct_name(x))
    i <- match(i, ct_name(x))
    ct_transform(x, i)
}

method(ct_transform, list(sdArrayFrame, class_numeric)) <- \(x, i) {
    t <- ct_args(x, i)
    switch(ct_type(x)[i],
        identity=return(x),
        scale=ct_scale(x, t),
        rotation=ct_rotate(x, t),
        translation=ct_translate(x, t),
        stop("not supported."))
}

# translate ----

#' @rdname coord-trans
#' @export
ct_translate <- new_generic("ct_translate", c("x", "t"))

method(ct_translate, list(PointFrame, class_numeric)) <- \(x, t) {
    stopifnot(length(t) == 2, is.finite(t))
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x+t[1]) |>
        mutate(y=y+t[2])
    return(x)
}

#' @importFrom sfarrow read_sf_dataset
method(ct_translate, list(ShapeFrame, class_numeric)) <- \(x, t) {
    stopifnot(length(t) == 2, is.finite(t))
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry+t
    return(x)
}

# scale ----

#' @rdname coord-trans
#' @export
ct_scale <- new_generic("ct_scale", c("x", "t"))

#' @importFrom dplyr mutate
method(ct_scale, list(PointFrame, class_numeric)) <- \(x, t) {
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x*t[1]) |>
        mutate(y=y*t[2])
    return(x)
}

# NOTE: this shifts the origin if it's not (0,0); 
#       could fix, but unclear what's expected.
#' @importFrom sfarrow read_sf_dataset
method(ct_scale, list(ShapeFrame, class_numeric)) <- \(x, t) {
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry*t(t)
    return(x)
}

# rotate ----

#' @rdname coord-trans
#' @export
ct_rotate <- new_generic("ct_rotate", c("x", "t"))

# rotation matrix to rotate points 
# counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

#' @importFrom dplyr mutate select
method(ct_rotate, list(PointFrame, class_numeric)) <- \(x, t) {
    y <- .y <- .x <- NULL # R CMD check
    R <- .R(t*pi/180)
    x@data <- x@data |>
        mutate(.x=x*R[1,1], .y=y*R[1,2]) |>
        mutate(x=.x+.y) |>
        mutate(.x=x*R[2,1], .y=y*R[2,2]) |>
        mutate(y=.x+.y) |>
        select(-.x, -.y)
    return(x)
}

#' @importFrom sfarrow read_sf_dataset
method(ct_rotate, list(ShapeFrame, class_numeric)) <- \(x, t) {
    R <- .R(t*pi/180)
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry * R
    return(x)
}
