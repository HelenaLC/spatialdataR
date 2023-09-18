#' @rdname plotting
#' @importFrom dplyr filter
#' @import ggplot2
#' @export
setMethod("plotPoint", "SpatialData",
    function(x, ..., i=1, coord=NULL) {
        y <- point(x, i)
        y <- transformElement(y, coord)
        # fil <- "filter(y@data, %s)"
        # fil <- sprintf(fil, .fil(...))
        # y@data <- eval(parse(text=fil))
        df <- as.data.frame(y)
        . <- list(...)
        .is_col <- FALSE
        .col_lab <- NULL
        args <- if (!is.null(.$col)) {
            if (length(.$col) == 1) {
                if (.$col %in% names(df)) {
                    .col_val <- df[[.$col]]
                    .is_disc <- is.factor(.col_val) || is.character(.col_val)
                    if (.is_disc) .col_val <- droplevels(factor(.col_val))
                    df[[.$col]] <- .col_val
                    .col <- .$col
                    .$col <- NULL
                } else {
                    .to_col <- tryCatch(col2rgb(.$col), error=\(e) e)
                    .is_col <- !inherits(.to_col, "error")
                    if (!.is_col)
                        stop("'col' should be a scalar string representing",
                            " a valid color (see '?col2rgb()'), or one of ",
                            paste(dQuote(names(df)), collapse=", "))
                }
            } else df[[.$col]] <- .$col
            if (!.is_col) c(list(aes(x, y, col=.data[[.col]]), df), .) else
                c(list(aes(x, y), df), .)
        } else c(list(aes(x, y), df), .)
        .data <- NULL
        geom <- do.call(geom_point, args)
        .plotElement(geom, range(df$x), rev(range(df$y)))
})

.in2or <- function(x) {
    # 'arrow' filtering currently lacks support for '%in%'
    # as a work around, convert to a series of '|' statements?
}

.fil <- function(...) {
    fil <- deparse(substitute(c(...)))
    fil <- gsub("c\\((.*)\\)", "\\1", fil)
    fil <- gsub("\"", "'", fil)
    return(fil)

}
