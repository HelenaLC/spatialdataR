#' @name plotSpatialData
#' @title `SpatialData` visualization
#' @aliases plotImage plotLabel plotPoint plotShape
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

#' @importFrom methods as
#' @importFrom abind abind
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
.df_i <- \(x, k=NULL) {
    a <- .get_plot_data(x, k)
    if (max(a) > 1) a <- a/255
    a <- if (dim(a)[1] == 1) a[rep(1,3),,] else a
    a <- realize(as(a, "DelayedArray"))
    apply(a, c(2, 3), \(.) do.call(rgb, as.list(.)))
}

.get_wh <- \(x, i, j) {
    g <- .coord2graph(x)
    p <- .get_path(g, i, j)
    d <- dim(data(image(x, i), 1))
    h <- c(0, d[2]); w <- c(0, d[3])
    for (. in seq_along(p)) {
        t <- p[[.]]$type
        d <- p[[.]]$data
        switch(t, 
            identity={ },
            translation={ h <- h+d[2]; w <- h+d[3] }, 
            scale={ h <- h*d[2]; w <- h*d[3] },
            stop("transformation of type '", t, "' not yet supported"))
    }
    list(w=w, h=h)
}

.gg_i <- \(x, w, h) list(
    ggplot2::annotation_raster(x, w[2], w[1], -h[1], -h[2], interpolate=FALSE),
    scale_y_reverse(limits=c(h[2], h[1])),
    xlim(w[1], w[2])
)

#' @rdname plotSpatialData
#' @export
setMethod("plotImage", "SpatialData", \(x, i=1, j=1, k=NULL) {
    if (is.numeric(i)) 
        i <- imageNames(x)[i]
    y <- image(x, i)
    if (is.numeric(j)) 
        j <- coordTransName(y)[j]
    df <- .df_i(y, k)
    wh <- .get_wh(x, i, j)
    .gg_i(df, wh$w, wh$h)
})
    
# label ----

#' @rdname plotSpatialData
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom S4Vectors metadata
#' @importFrom abind abind
#' @importFrom methods as
#' @export
setMethod("plotLabel", "SpatialData", \(x, i=1, c=NULL, a=0.5,
    pal=hcl.colors(11, "Spectral")) {
    .data <- NULL # R CMD check
    y <- as.matrix(as(data(label(x, i)), "DelayedArray"))
    df <- data.frame(x=c(col(y)), y=c(row(y)), z=c(y))
    if (!is.null(c)) {
        se <- table(x)
        md <- metadata(se)[[1]]
        re <- labelNames(x)[i]
        se <- se[, se[[md$region_key]] == re]
        cs <- match(df$z, se[[md$instance_key]])
        df$z <- se[[c]][cs]
        thm <- list(
            theme(legend.key.size=unit(0.5, "lines")),
            guides(fill=guide_legend(override.aes=list(alpha=1))))
    } else {
        thm <- guides(fill="none")
    }
    val <- sort(setdiff(unique(df$z), NA))
    pal <- colorRampPalette(pal)(length(val))
    list(thm, 
        geom_tile(aes(x, y, fill=factor(.data$z)), df, alpha=a),
        scale_fill_manual(c, values=pal, breaks=val, na.value=NA))
})

# point ----

.gg_p <- \(df, c, s, a) {
    aes <- aes(.data[["x"]], .data[["y"]])
    dot <- list()
    if (!is.null(c)) {
        if (c %in% names(df)) {
            aes$colour <- aes(.data[[c]])[[1]]
            lgd <- scale_type(df[[c]])
        } else if (.str_is_col(c)) {
            lgd <- "none"
            dot$colour <- c
        }
    } else lgd <- "none"
    if (is.character(s)) {
        if (s %in% names(df)) {
            aes$size <- aes(.data[[s]])[[1]]
        }
    } else if (is.numeric(s)) {
        dot$size <- s
    }
    dot$alpha <- a
    list(
        do.call(geom_point, c(list(data=df, mapping=aes), dot)), 
        if (lgd == "discrete") list(
            theme(legend.key.size=unit(0, "lines")),
            guides(col=guide_legend(override.aes=list(alpha=1, size=2)))
        ) else list(
            theme(
                legend.key.width=unit(0.5, "lines"),
                legend.key.height=unit(1, "lines")),
            scale_color_gradientn(colors=rev(hcl.colors(11, "Rocket")))
        )
    )
}

#' @param x SpatialData 
#' @param i Index of which slot of the Shape layer. Default value is 1.
#' @param c Color border of the shape to plot. Default value is NULL.
#' @param s Column name of interest in the shape coordinate file. If the geometry
#' is "POINT" (i.e. a circle), the default column name is "radius". Otherwise it 
#' is not needed for other geometry shapes such as "POLYGON".
#' @param a Transparency of the shape to plot. A value ranges from 0 to 1, 
#' with decreasing visibility. Default value is 0.2.
#'
#' @rdname plotSpatialData
#' @export
setMethod("plotPoint", "SpatialData", \(x, i=1, c=NULL, s=1, a=1) {
    .gg_p(as.data.frame(data(point(x, i))), c, s, a)
})

#' @rdname plotSpatialData
#' @export
setMethod("plotPoint", "PointFrame", \(x, c=NULL, s=1, a=1) {
    plotSpatialData() + .gg_p(as.data.frame(data(x)), c, s, a)
})

# shape ----

#' @param s Size of the shape to plot. Default is "radius".
#'
#' @rdname plotSpatialData
#' @importFrom sf st_as_sf st_coordinates st_geometry_type
#' @importFrom ggforce geom_circle
#' @export
setMethod("plotShape", signature = "SpatialData", \(x, i=1, c=NULL, f="white", s="radius", a=0.2) {
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
            geo <- geom_circle
            names(xs) <- xs <- setdiff(names(df), "geometry")
            df <- data.frame(xy, lapply(xs, \(.) df[[.]]))
            names(df) <- c("x", "y", xs)
            if (.str_is_col(c)) {
                dot$col <- c
            } else if (is.character(c)) {
                if (!c %in% names(df)) {
                    df <- .get_tbl(df, x, i)
                }
                aes$colour <- aes(.data[[c]])[[1]]
            } else stop("invalid 'c'")
            if (is.numeric(s)) {
                dot$size <- s
            } else if (!is.null(s)) {
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


#' @importFrom SummarizedExperiment colData
#' @importFrom S4Vectors metadata
.get_tbl <- \(df, x, i) {
  md <- metadata(se <- table(x))[[1]]
  se <- se[, se[[md$region_key]] == i]
  j <- setdiff(names(colData(se)), names(df))
  i <- match(df[[md$instance_key]], se[[md$instance_key]])
  cbind(df, colData(se)[i, j])
}
