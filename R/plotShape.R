#' @name plotShape
#' @title \code{SpatialData} shape viz.
#' 
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param c the default, NULL, gives a binary image of whether or not 
#'   a given pixel is non-zero; alternatively, a character string specifying
#'   a \code{colData} column or row name in a \code{table} annotating \code{i}.
#' @param f,s,a used to control plotting aesthetics;
#'   fill, size, alpha value passed to \code{geom_polygon}.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#' 
#' p <- plotSpatialData()
#' a <- p + plotShape(x, "blobs_polygons")
#' b <- p + plotShape(x, "blobs_multipolygons")
#' c <- p + plotShape(x, "blobs_circles")
#' patchwork::wrap_plots(a, b, c)
#' 
#' # layered
#' p + 
#'   plotShape(x, "blobs_circles", f="pink") +
#'   plotShape(x, "blobs_polygons", c="red")
#' 
#' patchwork::wrap_plots(a, b)
NULL

#' @rdname plotShape
#' @importFrom sf st_as_sf st_coordinates st_geometry_type
#' @importFrom ggforce geom_circle
#' @importFrom utils tail
#' @export
setMethod("plotShape", "SpatialData", \(x, i=1, c=NULL, f="white", s="radius", a=0.2, assay=1) {
    df <- data(shape(x, i))
    df <- st_as_sf(df)
    xy <- st_coordinates(df)
    typ <- st_geometry_type(df)
    typ <- as.character(typ[1])
    aes <- aes(.data[["x"]], .data[["y"]])
    dot <- list(fill=f, alpha=a)
    # TODO: tables support
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
        },{
            geo <- geom_polygon
            df <- data.frame(xy)
            if (ncol(df) == 4) {
                ij <- "i"
            } else {
                ij <- cbind(df[[4]], df[[5]])
                ij <- apply(ij, 1, paste, collapse=",")
                df[, 5] <- ij
                ij <- c("i", "j")
            }
            names(df) <- c("x", "y", "z", ij)
            g <- tail(names(df), 1) # grouping
            aes$group <- aes(.data[[g]])[[1]]
            if (is.null(c)) {
                aes$colour <- aes(factor(.data$i))[[1]]
                dot$show.legend <- FALSE
            } else if (.str_is_col(c)) {
                dot$colour <- c
            } else if (is.character(c)) {
                df[[c]] <- valTable(x, i, c, assay=assay)
                if (scale_type(df[[c]]) == "discrete")
                    df[[c]] <- factor(df[[c]])
                aes$colour <- aes(.data[[c]])[[1]]
            } else stop("invalid 'c'")
        })
    list(
        theme(legend.key.size=unit(0.5, "lines")),
        do.call(geo, c(list(data=df, mapping=aes), dot)))
})
