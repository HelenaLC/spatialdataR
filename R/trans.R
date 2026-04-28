#' @name trans
#' @rdname trans
#' @title Transformations
#' @aliases transform scale rotate translation flip flop mirror sequence
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
setMethod("transform", c("SpatialDataElement", "missing"), \(x, i, ...) transform(x, 1, ...))

#' @export
#' @rdname trans
setMethod("transform", c("SpatialDataElement", "numeric"), \(x, i, ...) transform(x, CTname(x)[i], ...))

#' @export
#' @rdname trans
setMethod("transform", c("SpatialDataElement", "character"), \(x, i, ...) {
    t <- CTdata(x, i)
    f <- CTtype(x)[match(i, CTname(x))]
    if (f == "sequence") {
        t <- lapply(t, unlist)
    } else t <- unlist(t)
    if (f == "identity") return(x)
    do.call(f, list(x, t, ...))
})

#' @export
#' @rdname trans
setMethod("sequence", c("SpatialDataElement", "list"), \(x, t, ..., rev=FALSE) {
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
.trans_a. <- \(x, f, k=1) {
    a <- f(aperm(as.array(data(x, k))))
    metadata(x)$data_type <- data_type(x)
    x@data <- list(as(aperm(a), "SparseArray"))
    return(x)
}

# rotation matrix to rotate points counter-clockwise through an angle 't'
.R <- \(t) matrix(c(cos(t), -sin(t), sin(t), cos(t)), 2, 2)

#' @export
#' @rdname trans
#' @importFrom EBImage rotate
setMethod("rotate", c("sdArray", "numeric"), \(x, t, k=1, ..., rev=FALSE) {
    # negate angle since 'EBImage' rotates clockwise
    stopifnot(length(t) == 1, is.finite(t))
    if (t %% 360 == 0) return(x)
    if (rev) t <- -t
    f <- \(.) EBImage::rotate(., -t) 
    if (length(d <- dim(data(x, k))) == 3) d <- d[-1]
    metadata(x)$wh <- lapply(rev(d), \(.) c(c(0, .) %*% .R(t*pi/180)))
    .trans_a.(x, f, k)
})

.trans_a <- \(x, t, f=c("scale", "translation"), k=1, rev=FALSE) {
    f <- match.arg(f)
    n <- length(d <- dim(data(x, k)))
    
    # setup: identity, operator
    map <- list(
        ids=c(scale=1, translation=0),
        ops=c(scale="*", translation="+"))
    
    # validation & identity check
    stopifnot(is.numeric(t), is.finite(t), length(t) == n)
    if (all(t == map$ids[f])) return(x)
    if (rev) t <- if (f == "scale") 1/t else -t
    
    # project to spatial (XY) dims
    if (n == 3) { t <- t[-1]; d <- d[-1] }
    t <- rev(t); d <- rev(d)
    
    # update 'wh' metadata
    wh <- metadata(x)$wh %||% list(c(0, d[1]), c(0, d[2]))
    op <- get(map$ops[f])
    metadata(x)$wh <- mapply(op, t, wh, SIMPLIFY=FALSE)
    return(x)
}

#' @export
#' @rdname trans
setMethod("scale", c("sdArray", "numeric"), \(x, t, ...) .trans_a(x, t, "scale", ...))

#' @export
#' @rdname trans
setMethod("translation", c("sdArray", "numeric"), \(x, t, ...) .trans_a(x, t, "translation", ...))

# point/shape ----

#' @importFrom dplyr mutate
#' @importFrom rlang call2 !!
.trans_f <- \(x, t, f=c("scale", "rotate", "translation"), rev=FALSE) {
    f <- match.arg(f)
    n <- length(axes(x))
    ST_Scale <- ST_Rotate <- ST_Translate <- NULL # R CMD check
    
    # setup: length, identity, function
    map <- list(
        len=c(scale=n, translation=n, rotate=1),
        ids=c(scale=1, translation=0, rotate=0),
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
    
    # dynamic injection 'ST_*(geo, v1, v2, ...)'
    v <- switch(f, rotate=t*pi/180, t) # radians
    x@data <- mutate(x@data, geometry=!!call2(map$fns[f], quote(geometry), !!!v))
    return(x)
}

#' @export
#' @rdname trans
setMethod("rotate", 
    c("sdFrame", "numeric"), \(x, t, ...) .trans_f(x, t, "rotate", ...))

#' @export
#' @rdname trans
setMethod("scale", c("sdFrame", "numeric"), \(x, t, ...) .trans_f(x, t, "scale", ...))

#' @export
#' @rdname trans
setMethod("translation", c("sdFrame", "numeric"), \(x, t, ...) .trans_f(x, t, "translation", ...))
