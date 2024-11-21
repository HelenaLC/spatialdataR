#' @name plotSpatialData
#' @title `SpatialData` visualization
#' @aliases plotImage plotShape
#' 
#' @description ...
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i element to use from a given layer.
#' @param j name of target coordinate system. 
#' @param k index of the scale of an image (default: NULL)
#' @param c,f,s,a plotting aesthetics; color, fill, size, alpha.
#' @param pal character vector of colors; will interpolate 
#'   automatically when insufficient values are provided.
#'
#' @return ggplot
#'
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' (x <- readSpatialData(base, tables=FALSE))
#' 
#' plotImage(x)
#'
#' @importFrom rlang .data
#' @import ggplot2
NULL

#' @importFrom grDevices col2rgb
.str_is_col <- \(x) !inherits(tryCatch(error=\(e) e, col2rgb(x)), "error")

.theme <- list(
    coord_equal(), theme_bw(), theme(
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.background=element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text=element_text(color="grey"),
        axis.ticks=element_line(color="grey"))
)

#' @rdname plotSpatialData
#' @export
plotSpatialData <- \() ggplot() + scale_y_reverse() + .theme 

# image ----

.guess_scale <- \(x, w, h) {
    n <- length(dim(x))
    i <- ifelse(n == 3, -1, TRUE)
    d <- vapply(x@data, dim, numeric(n))
    d <- apply(d, 2, \(.) sum(abs(.[i]-c(h, w))))
    which.min(d)
}

.get_plot_data <- \(x, k=NULL, w=800, h=800) {
    if (!is.null(k)) return(data(x, k))
    data(x, .guess_scale(x, w, h))
}

#' @importFrom methods as
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
.df_i <- \(x, k=NULL) {
    a <- .get_plot_data(x, k)
    a <- if (dim(a)[1] == 1) a[rep(1,3),,] else a
    a <- realize(as(a, "DelayedArray"))
    img <- rgb(
        maxColorValue=max(a),
        c(a[1,,]), c(a[2,,]), c(a[3,,]))
    array(img, dim(a)[-1])
}

.get_wh <- \(x, i, j) {
    ds <- dim(data(image(x, i), 1))
    ts <- .get_path(.coord2graph(x), i, j)
    wh <- data.frame(x=c(0, ds[3]), y=c(0, ds[2]))
    wh <- .trans_xy(wh, ts)
    list(w=wh[, 1], h=wh[, 2])
}

.gg_i <- \(x, w, h, dpi) list(
    scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
    ggplot2::annotation_raster(x, w[2],w[1], -h[1],-h[2], interpolate=FALSE))

#' @rdname plotSpatialData
#' @export
setMethod("plotImage", "SpatialData", \(x, i=1, j=1, k=NULL) {
    if (is.numeric(i)) 
        i <- imageNames(x)[i]
    y <- image(x, i)
    if (is.numeric(j)) 
        j <- CTname(y)[j]
    df <- .df_i(y, k)
    wh <- .get_wh(x, i, j)
    .gg_i(df, wh$w, wh$h)
})

# shape ----

#' @rdname plotSpatialData
#' @importFrom ggforce geom_circle
#' @importFrom sf st_as_sf st_coordinates st_geometry_type
#' @export
setMethod("plotShape", "SpatialData", \(x, i=1, c=NULL, f="white", s="radius", a=0.2) {
    if (is.numeric(i)) 
        i <- shapeNames(x)[i]
    df <- data(shape(x, i))
    df <- st_as_sf(df)
    xy <- st_coordinates(df)
    typ <- st_geometry_type(df)
    typ <- as.character(typ[1])
    aes <- aes(.data[["x"]], .data[["y"]])
    dot <- list(fill=f, alpha=a)
    # TODO: need separate plotting for different types of shapes
    switch(typ,
        # POINT means circle
        POINT={
            names(xs) <- xs <- setdiff(names(df), "geometry")
            df <- data.frame(xy, lapply(xs, \(.) df[[.]]))
            names(df) <- c("x", "y", xs)
            if (.str_is_col(c)) {
                dot$col <- c
            } else if (is.character(c)) {
                if (!c %in% names(df)) stop("invalid 'c'")
                aes$colour <- aes(.data[[c]])[[1]]
            }
            if (is.numeric(s)) {
                geo <- geom_point
                dot$size <- s
            } else if (!is.null(s)) {
                geo <- geom_circle
              aes$x0 <- df$x
              aes$y0 <- df$y
              aes$r <- aes(.data[[s]])[[1]]
            } else stop("invalid 's'")
        },
        POLYGON={
            geo <- geom_polygon
            df <- data.frame(xy)
            names(df) <- c("x", "y", "z", "i")
            if (is.null(c)) {
                aes$colour <- aes(factor(.data$i))[[1]]
                dot$show.legend <- FALSE
            } else if (.str_is_col(c)) {
                dot$col <- c
            } else stop("invalid 'c'")
        })
    list(
        theme(legend.key.size=unit(0.5, "lines")),
        do.call(geo, c(list(data=df, mapping=aes), dot)))
})
