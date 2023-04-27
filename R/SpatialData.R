#' @rdname SpatialData
#' @title The `SpatialData` class
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' dir <- "extdata/mibitof/images/point8_image"
#' zarr <- system.file(file.path(dir, "0"), package = "SpatialData")
#' json <- system.file(file.path(dir, ".zattrs"), package = "SpatialData")
#' 
#' library(Rarr)
#' library(jsonlite)
#' 
#' za <- read_zarr_array(zarr)
#' md <- fromJSON(json)
#' (ia <- ImageArray(za, md))
#' 
#' @export
SpatialData <- function(images, labels, shapes, points, ...) {
  if (missing(images)) images <- list()
  if (missing(labels)) labels <- list()
  if (missing(shapes)) shapes <- list()
  if (missing(points)) points <- list()
    
  if(! is.list(images)){
    images <- list(a = images)
  }
  if(! is.list(labels)){
    labels <- list(a = labels)
  }
  if(! is.list(shapes)){
    shapes <- list(a = shapes)
  }
  if(! is.list(points)){
    points <- list(a = points)
  }
  
  .SpatialData(images = images, labels = labels, shapes = shapes, points = points)
}