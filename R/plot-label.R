#' @name sd_plot_label
#' @title Plot `LabelArray`
#' 
#' @param x \code{\link{SpatialData}} object.
#' @param i scalar integer or string; 
#'   specifies which \code{labels} to plot.
#' @param c character vector of colors to use;
#'   if NULL (default), using \code{rainbow()}.
#' @param k resolution; if NULL (default), picking 
#'   best for given \code{w}idth and \code{h}eight.
#' @param w,h render width and height in pixel.
#' 
#' @return \code{ggplot}
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' sd_plot() + sd_plot_label(sd)
#' sd_plot() + sd_plot_label(sd, c="pink")
#' sd_plot() + sd_plot_label(sd, c=c("lavender", "blue"))
#' 
#' @importFrom methods as
#' @importFrom DelayedArray realize
#' @importFrom grDevices rgb rainbow colorRampPalette
#' @export
sd_plot_label <- \(x, i=1, c=NULL, k=NULL, w=800, h=800) {
    la <- x@labels[[i]]
    if (is.null(k)) 
        k <- .guess_scale(la, w, h)
    a <- data(la, k)
    a <- realize(as(a, "DelayedArray"))
    b <- as.integer(as.factor(a))
    n <- max(b)-1
    if (is.null(c)) {
        c <- rainbow(n)
    } else if (n > length(c)) {
        c <- colorRampPalette(c)(n)
    }
    a <- matrix(c(NA, c)[b], nrow(a), ncol(a))
    w <- c(0, dim(ia)[3])
    h <- c(0, dim(ia)[2])
    # lgd <- if (!is.null(pal)) list(
    #     guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
    #     scale_color_identity(NULL, guide="legend", labels=names(pal)),
    #     geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(
        scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
        annotation_raster(a, w[2],w[1], h[1],h[2], interpolate=FALSE))
}
