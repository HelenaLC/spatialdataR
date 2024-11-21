#' @name plotLabel
#' @title \code{SpatialData} label viz.
#' 
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param c the default, NULL, gives a binary image of whether or not 
#'   a given pixel is non-zero; alternatively, a character string specifying
#'   a \code{colData} column or row name in a \code{table} annotating \code{i}.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#' @param a scalar numeric in [0, 1]; alpha value passed to \code{geom_tile}.
#' @param pal character vector; color for discrete/continuous values
#'   (interpolated automatically when insufficient values are provided).
#' @param nan character string; color for missing values (hidden by default).
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#' 
#' i <- "blobs_labels"
#' p <- plotSpatialData()
#' 
#' # simple binary image
#' p + plotLabel(x, i)
#' 
#' # coloring by 'colData'
#' p + plotLabel(x, i, "instance_id")
#' 
#' # coloring by 'assay' data
#' p + plotLabel(x, i, "channel_1_sum")
NULL

#' @rdname plotLabel
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom S4Vectors metadata
#' @importFrom abind abind
#' @importFrom rlang .data
#' @importFrom methods as
#' @export
setMethod("plotLabel", "SpatialData", \(x, i=1, c=NULL, 
    a=0.5, pal=c("red", "green"), nan=NA, assay=1) {
    if (is.numeric(i)) i <- labelNames(x)[i]
    i <- match.arg(i, labelNames(x))
    y <- as.matrix(as(data(label(x, i)), "DelayedArray"))
    df <- data.frame(x=c(col(y)), y=c(row(y)), z=c(y))
    aes <- aes(.data[["x"]], .data[["y"]])
    if (!is.null(c)) {
        stopifnot(length(c) == 1, is.character(c))
        t <- table(x, hasTable(x, i, name=TRUE))
        md <- int_metadata(t)$spatialdata_attrs
        idx <- match(df$z, t[[md$instance_key]])
        df$z <- valTable(x, i, c, assay=assay)[idx]
        if (c == md$instance_key) df$z <- factor(df$z)
        aes$fill <- aes(.data[["z"]])[[1]]
        switch(scale_type(df$z), 
            discrete={
                val <- sort(setdiff(unique(df$z), NA))
                pal <- colorRampPalette(pal)(length(val))
                thm <- list(
                    theme(legend.key.size=unit(0.5, "lines")),
                    guides(fill=guide_legend(override.aes=list(alpha=1))),
                    scale_fill_manual(c, values=pal, breaks=val, na.value=nan))
            },
            continuous=thm <- list(
                theme(legend.key.size=unit(0.5, "lines")),
                scale_fill_gradientn(c, colors=pal, na.value=nan)))
    } else {
        thm <- guides(fill="none")
        aes$fill <- aes(.data$z != 0)[[1]]
        thm <- list(
            theme(legend.position="none"),
            scale_fill_manual(NULL, values=pal))
    }
    list(thm, do.call(geom_tile, list(data=df, mapping=aes, alpha=a)))
})