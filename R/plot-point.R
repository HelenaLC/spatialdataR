#' @name sd_plot_point
#' @title Plot `PointFrame`
#'
#' @param x \code{\link{SpatialData}} object.
#' @param i scalar integer or string; 
#'   specifies which \code{points} to plot.
#' @param c string; can be a color name or 
#'   \code{PointFrame} column to color by.
#' @param a assay to use when coloring by \code{tables}.
#' @param ... (optional) aesthetics passed to \code{geom_point}.
#'
#' @return \code{ggplot}
#'
#' @examples
#' pa <- file.path("extdata", "blobs.zarr")
#' pa <- system.file(pa, package="SpatialData")
#' sd <- readSpatialData(pa)
#' 
#' sd_plot() + sd_plot_point(sd, c="x")
#' sd_plot() + sd_plot_point(sd, c="pink", size=1)
#'
#' @importFrom SingleCellExperiment int_metadata
#' @importFrom SummarizedExperiment assay
#' @importFrom rlang .data
#' @import ggplot2
#' @export
sd_plot_point <- \(x, i, c, a, ...) {
    dot <- list(...)
    if (missing(c)) c <- NULL
    if (missing(i)) i <- 1
    if (is.numeric(i)) i <- names(x)$points[i]
    p <- x@points[[i]]
    df <- as.data.frame(p@data)
    aes <- aes(.data[["x"]], .data[["y"]])
    lgd <- "none"
    if (!is.null(c)) {
        if (.str_is_col(c)) {
            dot$colour <- c
        } else if (c %in% names(df)) {
            aes$colour <- aes(.data[[c]])[[1]]
        } else {
            stopifnot(length(x@tables) > 0)
            id <- int_metadata(t)$zattrs$region
            stopifnot(id == i)
            t <- x@tables[[1]]
            ik <- instance_key(p)
            idx <- match(df[[ik]], t[[ik]])
            a <- ifelse(missing(a), 1, a)
            if (c %in% rownames(t))
                t[[c]] <- assay(t, a)[c, ]
            df[[c]] <- t[[c]][idx]
            aes$colour <- aes(.data[[c]])[[1]]
        }
        if (c %in% names(df)) 
            lgd <- scale_type(df[[c]])
    } 
    arg <- c(list(data=df, mapping=aes), dot)
    list(
        do.call(geom_point, arg), 
        switch(lgd,
            discrete=list(
                theme(legend.key.size=unit(0, "lines")),
                guides(col=guide_legend(override.aes=list(alpha=1, size=2)))
            ),
            continuous=list(
                theme(
                    legend.key.width=unit(0.4, "lines"),
                    legend.key.height=unit(0.8, "lines")),
                scale_color_gradientn(colors=pals::jet())
            )
        )
    )
}