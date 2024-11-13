#' @name misc
#' @title Miscellaneous `Miro` methods
#' @description ...
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' # TODO
NULL

#' @importFrom S4Vectors coolcat
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    coolcat("- images(%d): %s", i <- imageNames(object))
    coolcat("- labels(%d): %s", l <- labelNames(object))
    coolcat("- shapes(%d): %s", s <- shapeNames(object))
    coolcat("- points(%d): %s", p <- pointNames(object))
    coolcat("- tables(%d): %s", tableNames(object))
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