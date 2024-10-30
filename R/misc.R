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
    coolcat("images(%d): %s", imageNames(object))
    coolcat("labels(%d): %s", labelNames(object))
    coolcat("shapes(%d): %s", shapeNames(object))
    coolcat("points(%d): %s", pointNames(object))
    coolcat("tables(%d): %s", tableNames(object))
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

#' @importFrom S4Vectors coolcat
.showImageArray <- function(object) {
    cat("class: ImageArray\n")
    cat("dim:", dim(object@data))
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