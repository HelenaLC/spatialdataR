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
#' @param image,label,shape,point
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
#' plotSD(spd, label=NULL)
#' plotSD(spd, shape=NULL, alpha.label=0.2)
NULL

#' @import ggplot2
.plotElement <- function(x, w, h) {
    thm <- list(
        coord_fixed(expand=FALSE),
        scale_x_continuous(limits=w),
        scale_y_continuous(limits=rev(h)),
        theme_linedraw(), theme(
            panel.grid=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank()))
    if (is.data.frame(x)) {
        ggplot(x, aes(x, y))
    } else {
        ggplot() + x
    } + thm
}

#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotElement", "ImageArray", function(x, coord=NULL, ...) {
    #x <- transformElement(x, coord)
    a <- as.array(x)
    a <- aperm(a, c(2, 3, 1))
    # TODO: rasterized images have one channel only;
    # not sure this hack generalizes to other cases...
    if (dim(a)[3] == 1) a <- abind(a, a, a, along = 3)
    r <- as.raster(a/max(a))
    # get axis limits
    md <- metadata(x); w <- md$w; h <- md$h
    # else anchor to origin
    if (is.null(h)) h <- c(0, dim(a)[1])
    if (is.null(w)) w <- c(0, dim(a)[2])
    # not really sure why, but this works somehow...
    #geom <- annotation_raster(r, w[1], w[2], h[1], h[2])
    geom <- annotation_raster(r, -Inf, Inf, -Inf, Inf)
    .plotElement(geom, w, h)
})

#' @rdname plotting
#' @importFrom grDevices as.raster
#' @importFrom ggplot2 annotation_raster
#' @export
setMethod("plotElement", "LabelArray", function(x, coord=NULL, ...) {
    #x <- transformElement(x, coord)
    dots <- list(...)
    alpha <- 1/3#ifelse(is.null(dots$alpha), 1, dots$alpha)
    a <- as.array(x)
    # TODO: this'll definitely need fixing once I have a
    # suitable test case where coordinates are shifted...
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
setMethod("plotElement", "ShapeFrame", function(x, coord=NULL, ...) {
    #x <- transformElement(x, coord)
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
    df$id <- factor(df$id, sort(unique(df$id)))
    geom <- geom_polygon(data=df,
        aes(x, y, group=id, fill=id),
        show.legend = FALSE, ...)
    .plotElement(geom, range(df$x), rev(range(df$y)))
})

#' @rdname plotting
#' @importFrom ggplot2 aes geom_point
#' @export
setMethod("plotElement", "PointFrame", function(x, coord=NULL, ...) {
    #x <- transformElement(x, coord)
    df <- as.data.frame(x)
    geom <- geom_point(aes(x, y), df)
    .plotElement(geom, range(df$x), rev(range(df$y)))
})

#' @rdname plotting
#' @import ggplot2
#' @importFrom grDevices as.raster rainbow
#' @export
plotSD <- function(x,
    image=1, label=1, shape=1, point=1,
    alpha.label=1/3, alpha.shape=1,
    col.point=NULL, coord=NULL)
    # TODO: single argument for aes of every element?
    # TODO: this is all pretty damn hacky, do better?
    # TODO: also, does 'scverse' allow multiple elements?
    # 'cause I ain't seeing this in any of the demos...
{
    stopifnot(
        is(x, "SpatialData"),
        is.numeric(alpha.label),
        is.numeric(alpha.shape),
        alpha.label >= 0, alpha.label <= 1,
        alpha.shape >= 0, alpha.shape <= 1)

    # retrieve elements to include
    i <- if (length(images(x))) image(x, image)
    l <- if (length(labels(x))) label(x, label)
    s <- if (length(shapes(x))) shape(x, shape)
    p <- if (length(points(x))) point(x, point)

    # TODO: something like this, but need to check that
    # 'table' 'region' matches with specified layer...
    # sce <- table(tmp)
    # gs <- rownames(sce)
    # if (col.point %in% gs) {
    #     p <- as.data.frame(p)
    #     y <- assay(sce, assay)
    #     p[[col.point]] <- y[col.point, ]
    # }

    # coordinate system alignment
    old <- list(i, l, s, p)
    arg <- c(old, list(coord=coord))
    new <- do.call(alignElements, arg)
    # nan <- vapply(old, is.null, logical(1))
    # typ <- c("image", "label", "shape", "point")
    # names(new) <- typ[!nan]

    # render graphics
    # aes <- list(
    #     point=list(col=col.point))
    # p <- mapply(
    #     SIMPLIFY=FALSE,
    #     obj=new, aes=aes[names(new)],
    #     \(obj, aes) plotElement(obj, aes))
    p <- lapply(new, plotElement)
    q <- do.call(.new_lim, p)
    return(q)
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

# extract axis limits from a 'ggplot'
.get_lim <- \(p, i) p$scales$scales[[i]]$limits
.xlim <- \(p) .get_lim(p, 1)
.ylim <- \(p) .get_lim(p, 2)

.new_lim <- function(...) {
    l <- list(...)
    l <- l[!vapply(l, is.null, logical(1))]
    # get shared axis limits
    xs <- vapply(l, .xlim, numeric(2))
    ys <- vapply(l, .ylim, numeric(2))
    xlim <- c(min(xs[1, ]), max(xs[2, ]))
    ylim <- c(min(ys[1, ]), max(ys[2, ]))
    # use 1st as base & stack others
    p <- l[[1]]; l <- l[-1]
    while (length(l)) {
        p <- p + l[[1]]$layers
        l <- l[-1]
    }
    suppressMessages(p +
            scale_x_continuous(limits=xlim) +
            scale_y_reverse(limits=rev(ylim)))
}
