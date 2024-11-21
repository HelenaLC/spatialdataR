#' @name plotImage
#' @title \code{SpatialData} image viz.
#' @aliases plotImage
#' 
#' @description ...
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i element to use from a given layer.
#' @param j name of target coordinate system. 
#' @param k index of the scale of an image; by default (NULL), will auto-select 
#'   scale in order to minimize memory-usage and blurring for a target size of 
#'   800 x 800px; use Inf to plot the lowest resolution available.
#'
#' @return ggplot
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, tables=FALSE)
#' 
#' ms <- lapply(seq(3), \(.) 
#'   plotSpatialData() +
#'   plotImage(x, i=2, k=.))
#' patchwork::wrap_plots(ms)
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

#' @rdname plotImage
#' @export
plotSpatialData <- \() ggplot() + scale_y_reverse() + .theme 

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

#' @import ggplot2
.gg_i <- \(x, w, h, dpi) list(
    scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
    annotation_raster(x, w[2],w[1], -h[1],-h[2], interpolate=FALSE))

#' @rdname plotImage
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