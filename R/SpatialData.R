#' @name SpatialData
#' @title The `SpatialData` class
#' 
#' @description ...
#'
#' @return \code{SpatialData}
#'
#' @examples
#' tf = tempfile()
#' dir.create(tf)
#' base = unzip_merfish_demo(tf)
#' (x <- readSpatialData(base, tables=TRUE))
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
