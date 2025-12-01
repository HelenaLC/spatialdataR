#' @name sd_plot_shape
#' @title Plot `ShapeFrame`
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i scalar integer or string; 
#'   specifies which \code{shapes} to plot.
#' @param c string; can be a color name,
#'   or TRUE to color by shape identifier.
#' @param ... (optional) aesthetics passed to \code{geom_sf}.
#'
#' @return \code{ggplot}
#'
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' sd_plot() + sd_plot_shape(sd, 1) # point
#' sd_plot() + sd_plot_shape(sd, 3) # polygon
#' sd_plot() + sd_plot_shape(sd, 2) # multi-polygon
#'
#' # aesthetics
#' sd_plot() + sd_plot_shape(sd, 3, 
#'   fill=NA, c="red", linewidth=1)
#'
#' @importFrom sfarrow read_sf_dataset
#' @importFrom rlang .data
#' @import ggplot2
#' @export
sd_plot_shape <- \(x, i=1, c=TRUE, ...) {
    if (is.numeric(i)) 
        i <- names(x)$shapes[i]
    s <- x@shapes[[i]]
    df <- read_sf_dataset(s@data)
    aes <- aes()
    thm <- list()
    dot <- list(...)
    if (isTRUE(c)) {
        df$.id <- factor(seq(nrow(df)))
        aes$colour <- aes(.data$.id)[[1]]
        thm <- c(thm, list(guides(col="none")))
    } else if (.str_is_col(c)) {
        ex <- grepv("col", names(dot))
        dot <- dot[setdiff(names(dot), ex)]
        dot$colour <- c
    }
    if (!is.null(df$radius)) 
        aes$size <- aes(.data$radius)[[1]]
    arg <- c(list(data=df, mapping=aes), dot)
    list(do.call(geom_sf, arg), thm)
}
