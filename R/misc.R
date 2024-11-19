#' @name misc
#' @title Miscellaneous `Miro` methods
#' @description ...
#'
#' @param object \code{\link{SpatialData}} object or one of its 
#'   elements, i.e., an Image/LabelArray or Point/ShapeFrame.
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' # TODO
NULL

#' @importFrom RBGL sp.between
#' @importFrom S4Vectors coolcat
#' @importFrom graph nodeData nodes
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    i <- imageNames(object)
    l <- labelNames(object)
    p <- pointNames(object)
    s <- shapeNames(object)
    t <- tableNames(object)
    # images
    d <- lapply(images(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- images(%s):\n", length(i)))
    for (. in seq_along(i)) 
        cat(sprintf("  - %s (%s)\n", i[.], d[.]))
    # labels
    d <- lapply(labels(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- labels(%s):\n", length(l)))
    for (. in seq_along(l)) 
        cat(sprintf("  - %s (%s)\n", l[.], d[.]))
    # points
    d <- lapply(points(object), length)
    cat(sprintf("- points(%s):\n", length(p)))
    for (. in seq_along(p)) 
        cat(sprintf("  - %s (%s)\n", p[.], d[.]))
    # shapes
    nc <- vapply(shapes(object), ncol, numeric(1))
    geom <- ifelse(nc == 1, "polygon", "circle")
    d <- vapply(shapes(object), nrow, numeric(1))
    d <- paste(d, unname(geom), sep=",")
    cat(sprintf("- shapes(%s):\n", length(s)))
    for (. in seq_along(s)) 
        cat(sprintf("  - %s (%s)\n", s[.], d[.]))
    # tables
    d <- lapply(tables(object), dim)
    d <- lapply(d, paste, collapse=",")
    cat(sprintf("- tables(%s):\n", length(t)))
    for (. in seq_along(t)) 
        cat(sprintf("  - %s (%s)\n", t[.], d[.]))
    # spaces
    cat("coordinate systems:\n")
    e <- c(i, l, s, p)
    g <- .coord2graph(object)
    t <- nodeData(g, nodes(g), "type")
    for (c in nodes(g)[t == "space"]) {
        pa <- suppressWarnings(sp.between(g, e, c))
        ss <- strsplit(names(pa), ":")
        ss <- ss[vapply(pa, \(.) !is.na(.$length), logical(1))]
        coolcat(
            paste0("- ", c, "(%d): %s"),
            vapply(ss, \(.) .[1], character(1)))
    }
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

#' @importFrom S4Vectors coolcat
.showImageArray <- function(object) {
    n.object <- length(object@data)
    cat("class: ImageArray", ifelse(n.object > 1, "(MultiScale)", ""),"\n")
    scales <- vapply(object@data, \(x) sprintf("(%s)", paste0(dim(x), collapse=",")), character(1))
    coolcat("Scales (%d): %s", scales)
}

#' @rdname misc
setMethod("show", "ImageArray", .showImageArray)

#' @importFrom S4Vectors coolcat
.showLabelArray <- function(object) {
    cat("class: LabelArray\n")
    cat("dim:", dim(object@data))
}

#' @rdname misc
setMethod("show", "LabelArray", .showLabelArray)

#' @importFrom S4Vectors coolcat
.showPointFrame <- function(object) {
    cat("class: PointFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "PointFrame", .showPointFrame)

#' @importFrom S4Vectors coolcat
.showShapeFrame <- function(object) {
    cat("class: ShapeFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "ShapeFrame", .showShapeFrame)