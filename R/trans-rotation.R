# 'aperm's needed here because channels are ordered as
# tczyx in OME-NGFF, but 'EBImage' needs c to be last
#' @importFrom EBImage rotate
.rotate3d <- function(x, t) {
    y <- as.array(x)
    y <- aperm(rotate(aperm(y), -t, "none"))
    get(class(x))(y, zattrs=zattrs(x))
}

# rotation matrix to rotate points
# counter-clockwise through an angle 't'
.R <- function(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

.rotate2d <- function(x, t) {
    R <- .R(t*pi/180)
    switch(class(x),
        ShapeFrame={
            x@listData$data <- lapply(x$data, \(xy) (xy %*% R))
        },
        PointFrame={
            x@data <- x@data %>%
                mutate(.x=x*R[1,1], .y=y*R[1,2]) %>%
                mutate(x=.x+.y) %>%
                mutate(.x=x*R[2,1], .y=y*R[2,2]) %>%
                mutate(y=.x+.y) %>%
                select(-.x, -.y)
        })
    return(x)
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
