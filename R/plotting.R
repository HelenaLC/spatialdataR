#' @name plotting
#' @title `SpatialData` plotting
#' @aliases plotImage plotLabel plotShape plotPoint
#'
#' @description ...
#'
#' @param x A \code{\link{SpatialData}} object.
#' @param i Scalar integer or character string specifying which element to plot.
#' @param coord A character string specifying the target coordinate system.
#'   If \code{NULL}, defaults to the first available shared coordinates.
#' @param width Scalar integer specifying the output plot's width (in pixel).
#' @param ... Additional graphical parameters passed to
#'   \code{\link[ggplot2]{geom_polygon}} for shapes;
#'   for labels, an \code{alpha} value can be passed;
#'   ignored otherwise.
#'
#' @return \code{ggplot2}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' path <- system.file("extdata", "raccoon", package="SpatialData")
#' spd <- readSpatialData(path)
#'
#' # by element
#' i <- plotImage(spd)
#' l <- plotLabel(spd)
#' s <- plotShape(spd)
#'
#' # putting it together
#' i+l+s
NULL

#' @import ggplot2
.plotElement <- function(x, w, h) {
    thm <- list(
        coord_fixed(expand=FALSE),
        scale_x_continuous(limits=w),
        scale_y_reverse(limits=h),
        theme_linedraw(), theme(
            panel.grid=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            legend.key.size=unit(0.5, "lines")),
        guides(col=guide_legend(override.aes=list(alpha=1, size=2))))
    gg <- if (is.data.frame(x)) {
        ggplot(x, aes(x, y))
    } else {
        ggplot() + x
    } + thm
    ggSD(gg, metadata=list(peter="pan"))
}

# extract axis limits from a 'ggplot'
.wh <- \(p, i) abs(p$scales$scales[[i]]$limits)
.w <- \(p) .wh(p, 1)
.h <- \(p) .wh(p, 2)

.new_lim <- function(...) {
    ps <- list(...)
    ps <- ps[!vapply(ps, is.null, logical(1))]
    # get shared axis limits
    ws <- vapply(ps, .w, numeric(2))
    hs <- vapply(ps, .h, numeric(2))
    w <- c(min(ws), max(ws))
    h <- c(max(hs), min(hs))
    # use 1st as base & stack others
    gg <- asS3(ps[[1]])
    ps <- ps[-1]
    while (length(ps)) {
        gg <- gg + ps[[1]]$layers
        ps <- ps[-1]
    }
    suppressMessages(gg <- gg +
        scale_x_continuous(limits=w) +
        scale_y_reverse(limits=h))
    return(ggSD(gg))
}

# TODO: need to somehow track which coordinate system is being used
# TODO: thrown an error when any element does not have those coordinates
#' @export
setMethod("+", c("ggSD", "ggSD"),
    function(e1, e2) .new_lim(e1, e2))

#' @export
setMethod("+", c("ggSD", "gg"),
    function(e1, e2) e1+as(e2, "ggSD"))

#' @export
setMethod("+", c("gg", "ggSD"),
    function(e1, e2) as(e1, "ggSD")+e2)

#' @export
setMethod("+", c("ggSD", "ANY"),
    function(e1, e2) as(asS3(e1)+e2, "ggSD"))
