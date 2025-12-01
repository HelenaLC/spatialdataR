#' @name sd_plot
#' @title Plotting `SpatialData`
#'
#' @return \code{ggplot}
#'
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' sd_plot() + 
#'   sd_plot_image(sd) +
#'   sd_plot_point(sd, c="x") 
#' 
#' pal <- c("cyan", "magenta", "gold")
#' sd_plot() + sd_plot_image(sd, c=pal)
#'
#' @seealso
#' \code{\link{sd_plot_image}}
#' \code{\link{sd_plot_label}}
#' \code{\link{sd_plot_point}}
#' \code{\link{sd_plot_shape}}
#'
#' @import ggplot2 
#' @export
sd_plot <- \() ggplot() + 
    #scale_y_reverse() +
    coord_equal() + 
    theme_bw() + theme(
        panel.grid=element_blank(),
        legend.key=element_blank(),
        legend.key.size=unit(0, "lines"),
        legend.background=element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text=element_text(color="grey"),
        axis.ticks=element_line(color="grey"))
