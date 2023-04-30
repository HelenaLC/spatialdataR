#' @name plotting
#' @title Plot `SpatialData` elements
#' @aliases plotSD
#'
#' @description ...
#'
#' @param x A \code{\link{SpatialData}} object.
#' @param image,label,shape
#'   A scalar integer (index) or character string (identifier)
#'   specifying the entity to include (can be \code{NULL}).
#' @param alpha.label,alpha.shape,color.shape Plotting aesthetics.
#' @param coord A character string specifying the target coordinate system.
#'   If \code{NULL}, defaults to the first available shared coordinates.
#'
#' @return \code{ggplot2}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' path <- system.file("extdata", "raccoon", package="SpatialData")
#' sd <- readSpatialData(path)
#' plotSD(sd)
#'
#' plotSD(sd, image=NULL)
#' plotSD(sd, label=NULL, color.shape="pink")
#' plotSD(sd, shape=NULL, alpha.label=0.2)

#' image(sd, 1) <- scaleArray(image(sd), c(1, 1, 0.5))
#' plotSD(sd)
NULL

#' @rdname plotting
#' @import ggplot2
#' @importFrom grDevices as.raster rainbow
#' @export
plotSD <- function(x,
    image, label, shape,
    alpha.label=1/3, alpha.shape=1,
    color.shape="lightgrey", coord=NULL) {

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
    if (!is.null(image)) i <- image(x, image)
    if (!is.null(label)) l <- label(x, label)
    if (!is.null(shape)) s <- shape(x, shape)

    # visually align elements
    # TODO: shapes & points
    if (!is.null(image) && !is.null(label)) {
        y <- alignElements(i, l, coord=coord)
        i <- y[[1]]; l <- y[[2]]
    }

    # construct image layer
    if (!is.null(image)) {
        i <- as.array(i)
        i <- aperm(i, c(2, 3, 1))
        hi <- dim(i)[1]
        wi <- dim(i)[2]
        ri <- as.raster(i/max(i))
        image_geom <- annotation_raster(ri, 0, wi, 0, -hi)
    } else hi <- wi <- 0

    if (!is.null(label)) {
        l <- as.array(l)
        hl <- dim(l)[1]
        wl <- dim(l)[2]
    } else hl <- wl <- 0

    # construct shape layer
    if (!is.null(shape)) {
        switch(s$type[1],
            # TODO: other options?
            circle={
                a <- as.array(s)
                s$x <- a[, 1]
                s$y <- a[, 2]
                s <- .circles(s)
            },
            polygon={
                s <- by(s, s$index, \(.)
                    data.frame(
                        id=.$index[1],
                        x=.$data[[1]][, 1],
                        y=.$data[[1]][, 2])
                ) |> do.call(what=rbind)
            })
        shape_geom <- geom_polygon(
            fill=color.shape,
            alpha=alpha.shape,
            data=s, aes_string("x", "y", group="id"))
    }

    # use largest element width/height
    h <- max(hi, hl)
    w <- max(wi, wl)

    # construct label layer
    if (!is.null(label)) {
        n <- length(unique(c(l)))
        c <- rainbow(n, alpha=alpha.label)
        c <- matrix(c[l+1], h, w)
        c[l == 0] <- NA
        rl <- as.raster(c)
        label_geom <- annotation_raster(rl, 0, wl, 0, -hl)
    }

    # put it all together...
    ggplot() +
        (if (!is.null(image)) image_geom) +
        (if (!is.null(label)) label_geom) +
        (if (!is.null(shape)) shape_geom) +
        # TODO: not so sure about this part yet...
        xlim(0, w) + #ylim(0, h) +
        scale_y_reverse(limits=c(h,0)) +
        coord_fixed(expand = FALSE) +
        theme_linedraw() + theme(
            axis.title=element_blank())
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
