#' @name SpatialData
#' @title The `SpatialData` class
#' 
#' @description ...
#' 
#' @param images list of \code{\link{ImageArray}}s
#' @param labels list of \code{\link{LabelArray}}s
#' @param points list of \code{\link{PointFrame}}s
#' @param shapes list of \code{\link{ShapeFrame}}s
#' @param tables list of \code{SingleCellExperiment}s
#'
#' @return \code{SpatialData}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' (x <- readSpatialData(x))
#' 
#' @export
SpatialData <- \(images, labels, points, shapes, tables) {
    if (missing(images)) images <- list()
    if (missing(labels)) labels <- list()
    if (missing(points)) points <- list()
    if (missing(shapes)) shapes <- list()
    if (missing(tables)) tables <- list()
    .SpatialData(
        images=images, labels=labels, 
        shapes=shapes, points=points, tables=tables)
}
    
. <- c("images", "labels", "points", "shapes", "tables")
names(.LAYERS) <- .LAYERS <- .
