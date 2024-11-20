#' @name plotPoint
#' @title \code{SpatialData} point viz.
#' 
#' @param x \code{SpatialData} object.
#' @param i character string or index; the label element to plot.
#' @param c character string giving a color; alternatively, may specify a
#'   \code{colData} column or row name in a \code{table} annotating \code{i}.
#' @param s scalar numeric; size value passed to \code{geom_point}.
#' @param a scalar numeric in [0, 1]; alpha value passed to \code{geom_point}.
#' @param assay character string; in case of \code{c} denoting a row name,
#'   specifies which \code{assay} data to use (see \code{\link{valTable}}).
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=TRUE)
#' 
#' i <- "blobs_points"
#' p <- plotSpatialData()
#' 
#' # mock up a 'table'
#' f <- list(
#'   numbers=\(n) runif(n),
#'   letters=\(n) sample(letters, n, TRUE))
#' x <- setTable(x, i, f)
#' 
#' p + plotPoint(x, i) # simple dots
#' p + plotPoint(x, i, "numbers") # discrete coloring
#' p + plotPoint(x, i, "letters") # continuous coloring
#' 
#' @importFrom grDevices hcl.colors colorRampPalette
#' @importFrom S4Vectors metadata
#' @importFrom abind abind
#' @importFrom methods as
#' @export
NULL

.gg_p <- \(df, c, s, a, assay, ik) {
    aes <- aes(.data[["x"]], .data[["y"]])
    dot <- list()
    if (!is.null(c)) {
        if (c %in% names(df)) {
            aes$colour <- aes(.data[[c]])[[1]]
        } else if (.str_is_col(c)) {
            dot$colour <- c
        } else {
            if (is.null(ik)) stop("missing 'instance_key' in 'table' annotating 'i'")
            stopifnot(length(c) == 1, is.character(c))
            t <- table(x, hasTable(x, i, name=TRUE))
            md <- int_metadata(t)$spatialdata_attrs
            idx <- match(df[[ik]], t[[md$instance_key]])
            df[[c]] <- valTable(x, i, c, assay=assay)[idx]
            aes$colour <- aes(.data[[c]])[[1]]
        } 
        lgd <- if (c %in% names(df)) scale_type(df[[c]]) else "none"
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

#' @rdname plotPoint
#' @export
setMethod("plotPoint", "SpatialData", \(x, i=1, c=NULL, s=1, a=1, assay=1) {
    ik <- meta(point(x, i))$spatialdata_attrs$instance_key
    .gg_p(as.data.frame(data(point(x, i))), c, s, a, assay, ik)
})

#' @rdname plotPoint
#' @export
setMethod("plotPoint", "PointFrame", \(x, c=NULL, s=1, a=1, assay=1) {
    plotSpatialData() + .gg_p(as.data.frame(data(x)), c, s, a, assay, ik=NULL)
})