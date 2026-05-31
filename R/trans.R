#' @name trans
#' @rdname trans
#' @title Transformations
#' @aliases transform scale rotate translation flip flop mirror sequence
#' 
#' @param x \code{SpatialData} element.
#' @param i scalar integer or string; target coordinate space.
#' @param t transformation data; exceptions: for \code{mirror}, controls
#'   whether to perform \bold{v}ertical or \bold{h}orizontal reflection;
#'   no data is needed for \code{flip} (\bold{v}) and \code{flop} (\bold{h}).
#' @param k scalar index specifying which scale to use; 
#'   \code{Inf} to use lowest available resolution;
#'   only applies to \code{SpatialDataArray}s (images, labels).
#' @param ... option arguments passed to and from other methods.
#' @param rev flag; should transformation(s) be reversed?
#' 
#' @returns \code{SpatialData} element with transformation(s) applied.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
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
#' xy0 <- centroids(point(y))
#' xy1 <- centroids(point(y, "rot"))
#' xy2 <- centroids(point(y, "wide"))
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

#' @export
#' @rdname trans
#' @importFrom BiocGenerics transform
setMethod("transform", "SpatialDataElement", \(x, i=1, ...) {
    stopifnot(
        length(i) == 1, is.character(i) | 
        (is.numeric(i) && i == round(i)))
    if (is.character(i)) {
        i <- match.arg(i, CTname(x))
        i <- match(i, CTname(x))
    }
    f <- CTtype(x)[i]
    t <- CTdata(x, i)
    if (f == "sequence") {
        t <- lapply(t, unlist)
    } else t <- unlist(t)
    if (f == "identity") return(x)
    do.call(f, list(x, t, ...))
})

#' @export
#' @rdname trans
#' @importFrom BiocGenerics sequence
setMethod("sequence", "SpatialDataElement", \(x, t, ..., rev=FALSE) {
    if (rev) t <- rev(t)
    for (. in seq_along(t)) {
        if (is.null(t[[.]])) next
        f <- names(t)[.]
        x <- do.call(f, list(x, t[[.]], ..., rev=rev))
    }
    return(x)
})

# array ----

.mirror <- \(x, t, k=1) {
    d <- length(dim(x)) == 3
    i <- if (d) c(1, 3, 2) else c(2, 1)
    # data(x) <- list(aperm(data(x, k), i))
    data(x) <- ImageArray::aperm(data(x, NULL), perm = i)
    rotate(x, t, k=1)
}

#' @export
#' @rdname trans
setMethod("mirror", "SpatialDataArray", \(x, t=c("v", "h"), k=1, ...) 
    switch(match.arg(t), v=flip, h=flop)(x, k))

#' @export
#' @rdname trans
setMethod("flip", "SpatialDataArray", \(x, ...) .mirror(x, 270))
# TODO: allow -90 as angle in ImageArray
# setMethod("flip", "SpatialDataArray", \(x, ...) .mirror(x, -90))

#' @export
#' @rdname trans
setMethod("flop", "SpatialDataArray", \(x, ...) .mirror(x, 90))

# rotation matrix to rotate points counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

#' @export
#' @rdname trans
#' @importFrom methods as
#' @importFrom BiocGenerics rotate
#' @importFrom S4Vectors metadata<-
setMethod("rotate", "SpatialDataArray", \(x, t, ..., rev=FALSE) {
  # complement angle with 360 to turn counterclockwise
    stopifnot(length(t) == 1, is.finite(t))
    if (t %% 360 == 0) return(x)
    if (rev) t <- 360-t
    data(x) <- ImageArray::rotate(data(x, NULL), t)
    return(x)
})

.trans_a_scale <- \(x, t, rev=FALSE) {
  n <- length(d <- dim(x))
  
  # validation & identity check
  stopifnot(is.numeric(t), is.finite(t), length(t) == 2)
  if (all(t == 0)) return(x)
  
  # scale
  if (rev) t <- 1/t
  
  # project to spatial (XY) dims
  if (n == 3) { d <- d[-1] }
  t <- rev(t); d <- rev(d)
  
  data(x) <- ImageArray::scale(data(x, NULL), 
                               output.dim = as.integer(round(d*t)))
  x
}

#' @export
#' @rdname trans
#' @importFrom BiocGenerics scale
setMethod("scale", "SpatialDataArray",
    \(x, t, ...) .trans_a_scale(x, t, ...))

.trans_a_trans <- \(x, t, rev=FALSE) {

  # validation & identity check
  stopifnot(is.numeric(t), is.finite(t), length(t) == 2)
  if (all(t == 0)) return(x)
  
  # project to spatial (XY) dims
  t <- rev(t)

  data(x) <- ImageArray::translation(data(x, NULL), shift = t)
  x
}
  
#' @export
#' @rdname trans
setMethod("translation", 
    c("SpatialDataArray", "numeric"), 
    \(x, t, ...) .trans_a_trans(x, t, ...))

# point/shape ----

#' @importFrom dplyr mutate
#' @importFrom rlang call2 !!
.trans_f <- \(x, t, f=c("scale", "rotate", "translation"), rev=FALSE) {
    ST_Scale <- ST_Rotate <- ST_Translate <- radius <- NULL # R CMD check
    
    f <- match.arg(f)
    n <- length(axes(x))
    
    # setup: length, identity, function
    map <- list(
        len=c(scale=n, rotate=1, translation=n),
        ids=c(scale=1, rotate=0, translation=0),
        fns=c(scale="ST_Scale", rotate="ST_Rotate", translation="ST_Translate"))
    
    # validation
    stopifnot(
        is.numeric(t), is.finite(t), 
        f != "scale" || all(t > 0),
        length(t) == map$len[f]) 

    # skip identity
    id <- switch(f, 
        rotate=(t %% 360 == 0), 
        all(t == map$ids[f]))
    if (id) return(x)

    # (optional) reverse
    if (rev) t <- switch(f, scale=1/t, -t)
    
    # edge case: rescale radii
    if (f == "scale" && "radius" %in% names(x))
        data(x) <- mutate(data(x), radius=!!t[1]*radius)
    
    # dynamic injection 'ST_*(geo, v1, v2, ...)'
    v <- switch(f, rotate=t*pi/180, t) # radians
    data(x) <- mutate(data(x), geometry=!!call2(map$fns[f], quote(geometry), !!!v))
    return(x)
}

#' @export
#' @rdname trans
#' @importFrom BiocGenerics rotate
setMethod("rotate", "SpatialDataFrame",
    \(x, t, ...) .trans_f(x, t, "rotate", ...))

#' @export
#' @rdname trans
#' @importFrom BiocGenerics scale
setMethod("scale", "SpatialDataFrame",
    \(x, t, ...) .trans_f(x, t, "scale", ...))

#' @export
#' @rdname trans
setMethod("translation", 
    c("SpatialDataFrame", "numeric"), 
    \(x, t, ...) .trans_f(x, t, "translation", ...))
