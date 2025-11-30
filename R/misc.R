#' @importFrom S7 S4_register method<-

S4_register(SpatialData)
method(print, SpatialData) <- \(object) { 
    cat("class: SpatialData\n")
    l <- list()
    # images
    i <- names(x <- object@images)
    d <- lapply(x, \(.) paste(dim(.), collapse=","))
    l <- c(l, list(images=list(i, d)))
    # labels
    i <- names(x <- object@labels)
    d <- lapply(x, \(.) paste(dim(.), collapse=","))
    l <- c(l, list(labels=list(i, d)))
    # points
    i <- names(x <- object@points)
    d <- vapply(x, length, integer(1))
    l <- c(l, list(points=list(i, d)))
    # shapes
    i <- names(x <- object@shapes)
    d <- vapply(x, length, integer(1))
    n <- vapply(x, ncol, integer(1))
    geom <- ifelse(n == 1, "polygon", "circle")
    d <- paste(d, geom, sep=",")
    l <- c(l, list(shapes=list(i, d)))
    # tables
    i <- names(x <- object@tables)
    d <- lapply(x, \(.) paste(dim(.), collapse=","))
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

S4_register(ImageArray)
S4_register(LabelArray)
S4_register(ShapeFrame)
S4_register(PointFrame)
S4_register(Zattrs)

method(print, ImageArray) <- \(object) .show_ms_array(object)
method(print, LabelArray) <- \(object) .show_ms_array(object)
method(print, Zattrs) <- \(object) cat(class(object))

method(print, ShapeFrame) <- \(object) {
    cat("class: ShapeFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}
method(print, PointFrame) <- \(object) {
    cat("class: PointFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}
