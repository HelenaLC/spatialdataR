#' @importFrom S7 S4_register method<-
S4_register(SpatialData)

#' @export
method(print, SpatialData) <- \(x, ...) { 
    cat("class: SpatialData\n")
    l <- list()
    # images
    i <- names(y <- x@images)
    d <- lapply(y, \(.) paste(dim(.), collapse=","))
    l <- c(l, list(images=list(i, d)))
    # labels
    i <- names(y <- x@labels)
    d <- lapply(y, \(.) paste(dim(.), collapse=","))
    l <- c(l, list(labels=list(i, d)))
    # points
    i <- names(y <- x@points)
    d <- vapply(y, length, integer(1))
    l <- c(l, list(points=list(i, d)))
    # shapes
    i <- names(y <- x@shapes)
    d <- vapply(y, length, integer(1))
    n <- vapply(y, ncol, integer(1))
    geom <- ifelse(n == 1, "polygon", "circle")
    d <- paste(d, geom, sep=",")
    l <- c(l, list(shapes=list(i, d)))
    # tables
    i <- names(y <- x@tables)
    d <- lapply(y, \(.) paste(dim(.), collapse=","))
    l <- c(l, list(tables=list(i, d)))
    # render
    for (. in names(l)) {
        i <- l[[.]][[1]]; d <- l[[.]][[2]]
        cat(sprintf("- %s(%s):\n", ., n <- length(i)))
        for (. in seq_len(n)) cat(sprintf("  - %s (%s)\n", i[.], d[.]))
    }
}

#' @importFrom S7 S7_class
#' @importFrom S4Vectors coolcat
.show_ms_array <- \(object) {
    n <- length(data(object))
    cat("class:", S7_class(object)@name, ifelse(n > 1, "(MultiScale)", ""), "\n")
    scales <- vapply(data(object), \(.) sprintf("(%s)", paste0(dim(.), collapse=",")), character(1))
    coolcat("scale(%d): %s", scales)
}

#' @export
method(print, ImageArray) <- \(x, ...) .show_ms_array(x)

#' @export
method(print, LabelArray) <- \(x, ...) .show_ms_array(x)

.show_sd_frame <- \(x) {
    . <- attr(x, "S7_class")@name
    cat(sprintf("class: %s\n", .))
    cat("count:", length(x), "\n")
    coolcat("data(%d): %s\n", names(x))
}

#' @export
method(print, ShapeFrame) <- \(x, ...) .show_sd_frame(x)

#' @export
method(print, PointFrame) <- \(x, ...) .show_sd_frame(x)

#' @export
method(print, Zattrs) <- \(x, ...) cat(class(x))
