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
    coolcat("- tables(%d): %s", t <- tableNames(object))
    cat("coordinate systems:\n")
    e <- c(i, l, s, p, t)
    g <- .coord2graph(object)
    for (c in setdiff(nodes(g), e)) {
        coolcat(
            paste0("- ", c, "(%d): %s"), 
            graph::inEdges(c, g)[[1]])
    }
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

#' @importFrom S4Vectors coolcat
.showImageArray <- function(object) {
    n.object <- length(object@data)
    cat("class: ImageArray", ifelse(n.object > 1, "(MultiScale)", ""),"\n")
    if(n.object > 1){
      cat("Scales 1-", n.object, "\n", sep = "")
    }
    for(i in 1:n.object){
      cat(i, ": (", paste(dim(object@data[[i]]), collapse = ","), ")", "\n", sep = "")
    }
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