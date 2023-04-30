#' @name plotting
#' @title Plot `SpatialData` elements
#' @aliases plotSD
#'
#' @description ...
#'
#' @inheritParams ZarrArray
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' path <- system.file("extdata", "raccoon", package="SpatialData")
#' sd <- readSpatialData(path)
#' plotSD(sd)
#' plotSD(sd, image=NULL)
#' plotSD(sd, label=NULL, color.shape="pink")
#' plotSD(sd, shape=NULL, alpha.label=0.2)
NULL

#' @rdname plotting
#' @import ggplot2
#' @export
plotSD <- function(x,
    image=1, label=1, shape=1,
    alpha.label=1/3, alpha.shape=1,
    color.shape="lightgrey", ...) {

    stopifnot(
        is(x, "SpatialData"),
        is.numeric(alpha.label),
        is.numeric(alpha.shape),
        alpha.label >= 0, alpha.label <= 1,
        alpha.shape >= 0, alpha.shape <= 1)

    if (!is.null(image)) {
        .check_i(x, "image", image)
        i <- image(x, image)
        i <- as.array(i)
        i <- aperm(i, c(2, 3, 1))
        hi <- dim(i)[1]
        wi <- dim(i)[2]
        ri <- as.raster(i/max(i))
        image_geom <- annotation_raster(ri, 0, wi, 0, -hi)
    } else hi <- wi <- 0

    if (!is.null(label)) {
        .check_i(x, "label", label)
        l <- label(x, label)
        l <- as.array(l)
        hl <- dim(l)[1]
        wl <- dim(l)[2]
    } else hl <- wl <- 0

    if (!is.null(shape)) {
        .check_i(x, "shape", shape)
        s <- shape(x, shape)
        switch(s$type[1],
            circle={
                s$x <- sapply(s$data, .subset, 1)
                s$y <- sapply(s$data, .subset, 2)
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
            data=s, aes(x, y, group=id))
    }

    h <- max(hi, hl)
    w <- max(wi, wl)

    if (!is.null(label)) {
        n <- length(unique(c(l)))
        c <- rainbow(n, alpha=alpha.label)
        c <- matrix(c[l+1], h, w)
        c[l == 0] <- NA
        rl <- as.raster(c)
        label_geom <- annotation_raster(rl, 0, wl, 0, -hl)
    }

    ggplot() +
        (if (!is.null(image)) image_geom) +
        (if (!is.null(label)) label_geom) +
        (if (!is.null(shape)) shape_geom) +
        xlim(0, w) + #ylim(0, h) +
        scale_y_reverse(limits=c(h,0)) +
        coord_fixed(expand = FALSE) +
        theme_linedraw() + theme(
            axis.title=element_blank())
}

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

# blobs <- readSpatialData("inst/extdata/blobs/")
# raccoon <- readSpatialData("inst/extdata/raccoon/")
plotSD(blobs)
plotSD(raccoon)
