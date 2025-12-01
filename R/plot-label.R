#' @name sd_plot_label
#' @title Plot `LabelArray`
#' 
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' devtools::load_all()
#' sd_plot() + sd_plot_label(sd)
#' 
#' @importFrom methods as
#' @importFrom grDevices rgb
#' @importFrom DelayedArray realize
#' @export
sd_plot_label <- \(x, i=1, k=NULL, c=NULL, w=800, h=800) {
    x <- sd; i <- 1; w=h=800; c <- k <- NULL; ch <- 2
    la <- x@labels[[i]]
    if (is.null(k)) 
        k <- .guess_scale(la, w, h)
    a <- data(la, k)
    dt <- .get_dt(la)
    a <- as(a, "DelayedArray")
    dim(a) <- c(1, dim(a))
    n <- length(setdiff(unique(c(a)), 0))
    a <- do.call(arbind, replicate(n, a))
    a <- .norm_ia(realize(a), dt)
    a <- .chs2rgb(a, 1, c)
    a <- apply(a, c(2, 3), \(.) do.call(rgb, as.list(.))) 
    w <- c(0, dim(ia)[3])
    h <- c(0, dim(ia)[2])
    # TODO: coloring by value?
    pal <- NULL
    lgd <- if (!is.null(pal)) list(
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))),
        scale_color_identity(NULL, guide="legend", labels=names(pal)),
        geom_point(aes(col=.data$foo), data.frame(foo=pal), x=0, y=0, alpha=0))
    list(lgd,
        scale_x_continuous(limits=w), scale_y_reverse(limits=rev(h)),
        annotation_raster(a, w[2],w[1], h[1],h[2], interpolate=FALSE))
}
