#' @name SD-miscellaneous
#' @title Miscellaneous `SpatialData` methods
#' @description
#' Miscellaneous methods for the \code{\link{SpatialData}}
#' and \code{\link{ImageArray}} classes that do not fit
#' into any other documentation category such as,
#' for example, show methods.
#'
#' @param object \code{\link{SpatialData}} or \code{\link{ImageArray}} object.
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' dir <- system.file("extdata", "raccoon", package="SpatialData")
#' (img <- readArray(file.path(dir, "images", "raccoon")))
#' (spd <- readSpatialData(dir))
NULL

#' @importFrom S4Vectors coolcat
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    coolcat("images(%d): %s", imageNames(object))
    coolcat("labels(%d): %s", labelNames(object))
    coolcat("shapes(%d): %s", shapeNames(object))
    coolcat("points(%d): %s", pointNames(object))
    coolcat("tables(%d): %s", tableNames(object))
    coolcat("coords(%d): %s", coordNames(object))
}

#' @rdname SD-miscellaneous
setMethod("show", "SpatialData", .showSpatialData)

.showZarrArray <- function(object) {
    a <- paste(axiis(object), collapse="")
    if (length(d <- dim(object)) == 1) d <- 0
    cat(sprintf("axiis(%s):", a), d, "\n")
    coolcat("|-time(%d): %s", axiis(object, "time"))
    coolcat("|-space(%d): %s", axiis(object, "space"))
    coolcat("|-channel(%d): %s", axiis(object, "channel"))
    coolcat("coords(%d): %s", coordNames(object))
}
.showImageArray <- function(object) {
    cat("class: ImageArray\n")
    cat("channels:", channels(object), "\n")
    callNextMethod(object)
}
.showLabelArray <- function(object) {
    cat("class: LabelArray\n")
    callNextMethod(object)
}

.showShapeFrame <- \(object) {
    cat("class: ShapeFrame\n")
    cat("count:", n <- length(object), "\n")
    if (n > 0) cat("geoms:", object$type[1], "\n")
    coolcat("coords(%d): %s", coordNames(object))
}

.showPointFrame <- function(object) {
    cat("class: PointFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
    coolcat("coords(%d): %s", coordNames(object))
}

#' @rdname SD-miscellaneous
setMethod("show", "ZarrArray", .showZarrArray)

#' @rdname SD-miscellaneous
setMethod("show", "ImageArray", .showImageArray)

#' @rdname SD-miscellaneous
setMethod("show", "LabelArray", .showLabelArray)

#' @rdname SD-miscellaneous
setMethod("show", "ShapeFrame", .showShapeFrame)

#' @rdname SD-miscellaneous
setMethod("show", "PointFrame", .showPointFrame)
