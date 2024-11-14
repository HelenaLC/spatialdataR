#' @name plotSpatialData
#' @title `SpatialData` visualization
#' @aliases plotImage plotLabel plotPoint plotShape
#' 
#' @description ...
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

#' @importFrom abind abind
#' @importFrom grDevices rgb
.df_i <- \(x) {
  plot_data <- as.array(.get_plot_data(x)/255)
  if (dim(plot_data)[1] == 1) plot_data[rep(1,3),,] else plot_data
}

.gg_i <- \(x, w, h) list(
  # ggplot2::annotation_raster(aperm(x, perm = c(2,3,1)), dat[1,3], dat[2,3], -dat[1,2], -dat[2,2], interpolate = FALSE),
  # scale_y_reverse(limits = c(dat[2,2], dat[1,2])),
  # xlim(dat[1,3], dat[2,3])
  ggplot2::annotation_raster(aperm(x, perm = c(2,3,1)), w[2], w[1], -h[1], -h[2], interpolate = FALSE),
  scale_y_reverse(limits = c(h[2], h[1])),
  xlim(w[1], w[2])
)

#' @rdname plotSpatialData
#' @export
setMethod("plotImage", "SpatialData", \(x, i=1, j=1) {
  df <- .df_i(y <- image(x, i))
  # extent <- rbind(rep(0,3), dim(data(y,1)))
  extent <- dim(data(y,1))
  w <- c(0, extent[3])
  h <- c(0, extent[2])
  if (!is.null(t <- getTS(y, j))){
    for (. in seq(nrow(t))) {
      print(t)
      typ <- t$type[.]
      dat <- t[[typ]][.][[1]]
      switch(typ,
             translation={
               # extent[,2] <- extent[,2]+dat[2]
               # extent[,3] <- extent[,3]+dat[3]
               h <- h+dat[2]
               w <- w+dat[3]
             },
             scale={
               # extent[,2] <- extent[,2]*dat[2]
               # extent[,3] <- extent[,3]*dat[3]
               h <- h*dat[2]
               w <- w*dat[3]
             })
    } 
  }
  # .gg_i(df, extent)
  .gg_i(df, w, h)
})

temp <- function(t){
  print(t)
  print(t)
  print(t)
}
    
# label ----

#' @rdname plotSpatialData
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom abind abind
#' @export
setMethod("plotLabel", "SpatialData", \(x, i=1, c=NULL, a=0.5,
    pal=hcl.colors(11, "Spectral")) {
    y <- as.matrix(data(label(x, i)))
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
        geom_tile(aes(x, y, fill=factor(z)), df, alpha=a),
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

#' @rdname plotSpatialData
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
        POINT={
            geo <- geom_point
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
                aes$size <- aes(.data[[s]])[[1]]
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
.get_tbl <- \(df, x, i) {
    md <- metadata(se <- table(x))[[1]]
    se <- se[, se[[md$region_key]] == i]
    j <- setdiff(names(colData(se)), names(df))
    i <- match(df[[md$instance_key]], se[[md$instance_key]])
    cbind(df, colData(se)[i, j])
}
