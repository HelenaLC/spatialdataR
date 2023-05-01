#' @name plotting
#' @title Plot `SpatialData` elements
#' @aliases
#' plotSD plotElement
#' plotSD,SpatialData
#' plotElement,ImageArray
#' plotElement,LabelArray
#' plotElement,ShapeFrame
#'
#' @description ...
#'
#' @param x A \code{\link{SpatialData}} object.
#' @param image,label,shape
#'   A scalar integer (index) or character string (identifier)
#'   specifying the entity to include (can be \code{NULL}).
#' @param alpha.label,alpha.shape,fill.shape,col.shape Plotting aesthetics.
#' @param coord A character string specifying the target coordinate system.
#'   If \code{NULL}, defaults to the first available shared coordinates.
#' @param ... Additional graphical parameters passed to
#'   \code{\link[ggplot2]{geom_polygon}} for shapes;
#'   for labels, an \code{alpha} value can be passed;
#'   ignored otherwise.
#'
#' @return \code{ggplot2}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' path <- system.file("extdata", "raccoon", package="SpatialData")
#' spd <- readSpatialData(path)
#'
#' # by element
#' plotElement(image(spd))
#' plotElement(label(spd))
#' plotElement(shape(spd))
#'
#' # layered
#' plotSD(spd)
#' plotSD(spd, image=NULL)
#' plotSD(spd, label=NULL, fill.shape="pink")
#' plotSD(spd, shape=NULL, alpha.label=0.2)
NULL

#' @import ggplot2
.plotElement <- function(geom, w, h) {
    ggplot() + geom +
        xlim(w) +
        scale_y_reverse(limits=rev(h)) +
        coord_fixed(expand = FALSE) +
        theme_linedraw() + theme(
            panel.grid=element_blank(),
            axis.title=element_blank())
}

#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotElement", "ImageArray", function(x, coord, ...) {
    if (!missing(coord))
        x <- transformElement(x, coord)
    a <- as.array(x)
    a <- aperm(a, c(2, 3, 1))
    r <- as.raster(a/max(a))
    w <- dim(a)[2]; h <- dim(a)[1]
    geom <- annotation_raster(r, 0, w, 0, -h)
    .plotElement(geom, c(0, w), c(0, h))
})
#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotElement", "LabelArray", function(x, coord, ...) {
    if (!missing(coord))
        x <- transformElement(x, coord)
    dots <- list(...)
    alpha <- ifelse(is.null(dots$alpha), 1, dots$alpha)
    a <- as.array(x)
    w <- dim(a)[2]; h <- dim(a)[1]
    n <- length(unique(c(a)))
    c <- rainbow(n, alpha=alpha)
    c <- matrix(c[a+1], h, w)
    c[a == 0] <- NA
    r <- as.raster(c)
    geom <- annotation_raster(r, 0, w, 0, -h)
    .plotElement(geom, c(0, w), c(0, h))
})
#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 aes geom_polygon
#' @export
setMethod("plotElement", "ShapeFrame", function(x, coord, ...) {
    if (!missing(coord))
        x <- transformElement(x, coord)
    switch(x$type[1],
        # TODO: other options?
        circle={
            a <- as.array(x)
            x$x <- a[, 1]
            x$y <- a[, 2]
            df <- .circles(x)
        },
        polygon={
            df <- by(x, x$index, \(.)
                data.frame(
                    id=.$index[1],
                    x=.$data[[1]][, 1],
                    y=.$data[[1]][, 2])
            ) |> do.call(what=rbind)
        })
    geom <- geom_polygon(data=df, aes(x, y, group=id), ...)
    .plotElement(geom, range(df$x), range(df$y))
})

#' @rdname plotting
#' @import ggplot2
#' @importFrom grDevices as.raster rainbow
#' @export
plotSD <- function(x,
    image, label, shape,
    alpha.label=1/3, alpha.shape=1,
    fill.shape="lightgrey", col.shape=NA, coord=NULL) {
# x <- sd
# image <- 1
# label <- NULL
# shape <- NULL

    stopifnot(
        is(x, "SpatialData"),
        is.numeric(alpha.label),
        is.numeric(alpha.shape),
        alpha.label >= 0, alpha.label <= 1,
        alpha.shape >= 0, alpha.shape <= 1)

    # for all unspecified elements,
    # default to first available
    if (missing(image)) image <- imageNames(x)[1]
    if (missing(label)) label <- labelNames(x)[1]
    if (missing(shape)) shape <- shapeNames(x)[1]

    # retrieve elements to include
    i <- if (!is.null(image)) image(x, image)
    l <- if (!is.null(label)) label(x, label)
    s <- if (!is.null(shape)) shape(x, shape)

    # align elements
    lys <- list(i, l, s)
    nan <- vapply(lys, is.null, logical(1))
    tmp <- do.call(alignElements, c(lys[!nan], list(coord=coord)))
    lys[!nan] <- tmp
    if (!nan[1]) i <- lys[[1]]
    if (!nan[2]) l <- lys[[2]]
    if (!nan[3]) s <- lys[[3]]

    ps <- list(
        if (!is.null(i)) plotElement(i),
        if (!is.null(l)) plotElement(l, alpha=alpha.label),
        if (!is.null(s)) plotElement(s, alpha=alpha.shape, fill=fill.shape, col=col.shape))
    ps <- ps[!vapply(ps, is.null, logical(1))]
    .get_lim <- \(p, i) p$scales$scales[[i]]$limits
    xs <- vapply(ps, \(p) .get_lim(p, 1), numeric(2))
    ys <- vapply(ps, \(p) .get_lim(p, 2), numeric(2))
    xlim <- c(min(xs[1, ]), max(xs[2, ]))
    ylim <- c(min(ys[1, ]), max(ys[2, ]))

    gg <- ps[[1]]
    if (length(ps) > 1)
        for (p in ps[-1])
            gg <- gg + p$layer[[1]]
    suppressMessages(gg + xlim(xlim) + ylim(abs(ylim)))
}

# construct table of circle coordinates
# (polygons) given xy-coordinates & radii
.circles <- function(df, n=360){
    angle <- seq(-pi, pi, length=n)
    .circ <- function(x, y, r, id)
        data.frame(id,
            x=x+r*cos(angle),
            y=y+r*sin(angle))
    mapply(.circ,
        id=seq_len(nrow(df)),
        x=df$x, y=df$y, r=df$radius,
        SIMPLIFY=FALSE) |> do.call(what=rbind)
}
