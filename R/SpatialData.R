#' @rdname SpatialData
#' @title The `SpatialData` class
#' @aliases
#' SpatialData
#' SpatialData-class
#' $,SpatialData-method
#' [[,SpatialData-method
#' image images imageNames
#' label labels labelNames
#' shape shapes shapeNames
#' point points pointNames
#' element elementNames
#' table table<-
#'
#' @description ...
#'
#' @param x A \code{SpatialData} object.
#' @param table A \code{SingleCellExperiment}s.
#' @param images A list of \code{\link{ImageArray}}s.
#' @param labels A list of \code{\link{ImageArray}}s.
#' @param shapes A list of \code{\link{DataFrame}}s.
#' @param points A list of Arrow \code{\link{Dataset}}s.
#' @param elementName,name A character string
#'   specifying the type of element to extract;
#'   should be one of \code{"images"}, \code{"labels"},
#'   \code{"shapes"}, \code{"points"}, or \code{"table"}.
#' @param i Entity of the respective element to extract;
#'   can be an integer index or character string
#'   (one of \code{eNames(x)}, where \code{e}
#'   is the specified \code{elementName}).
#' @param j Ignored.
#' @param value Object of appropriate type; see respective elements.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return
#' \itemize{
#' \item \code{images/labels/shapes/points}
#'   return a list of entities of the corresponding element.
#' \item \code{image/label/shape/point}
#'   return a single entity of the corresponding type.
#' \item \code{image/label/shape/pointNames}
#'   return a character string of available
#'   entities of the corresponding element.
#' }
#'
#' @examples
#' path <- file.path("extdata", "blobs")
#' path <- system.file(path, package="SpatialData")
#' (spd <- readSpatialData(path))
#'
#' # accessors
#' imageNames(spd)
#' image(spd, "blobs_image")
#' spd$images$blobs_image
#'
#' (sce <- table(spd))
#'
#' @author Constantin Ahlmann-Eltze, Helena L. Crowell
#'
#' @export
SpatialData <- function(images, labels, shapes, points, table) {
    if (missing(images)) images <- list()
    if (missing(labels)) labels <- list()
    if (missing(shapes)) shapes <- list()
    if (missing(points)) points <- list()
    if (missing(table)) table <- NULL

    if (!is.list(images)) images <- list(a=images)
    if (!is.list(labels)) labels <- list(a=labels)
    if (!is.list(shapes)) shapes <- list(a=shapes)
    if (!is.list(points)) points <- list(a=points)

    .SpatialData(
        images=images,
        labels=labels,
        shapes=shapes,
        points=points,
        table=table)
}

LAYERS <- setdiff(names(attributes(SpatialData())), c("metadata", "class"))


