#' @name coord-trans
#' @title Coordinate transformations
#' 
#' @aliases ct_translate ct_scale ct_rotate
#' 
#' @param x \code{SpatialData} element.
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

# translate ----

#' @rdname coord-trans
#' @export
ct_translate <- new_generic("ct_translate", "x")

method(ct_translate, PointFrame) <- \(x, t) {
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x+t[1]) |>
        mutate(y=y+t[2])
    return(x)
}

#' @importFrom sfarrow read_sf_dataset
method(ct_translate, ShapeFrame) <- \(x, t) {
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry+t
    return(x)
}

# scale ----

#' @rdname coord-trans
#' @export
ct_scale <- new_generic("ct_scale", "x")

#' @importFrom dplyr mutate
method(ct_scale, PointFrame) <- \(x, t) {
    y <- NULL # R CMD check
    x@data <- x@data |>
        mutate(x=x*t[1]) |>
        mutate(y=y*t[2])
    return(x)
}

# NOTE: this shifts the origin if it's not (0,0); 
#       could fix, but unclear what's expected.
#' @importFrom sfarrow read_sf_dataset
method(ct_scale, ShapeFrame) <- \(x, t) {
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry*t(t)
    return(x)
}

# rotate ----

#' @rdname coord-trans
#' @export
ct_rotate <- new_generic("ct_rotate", "x")

# rotation matrix to rotate points 
# counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

#' @importFrom dplyr mutate select
method(ct_rotate, PointFrame) <- \(x, t) {
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
method(ct_rotate, ShapeFrame) <- \(x, t) {
    R <- .R(t*pi/180)
    x@data <- read_sf_dataset(x@data)
    x@data$geometry <- x@data$geometry * R
    return(x)
}
