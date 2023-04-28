#' @rdname readSpatialData
#' @title Read `SpatialData` OME-Zarr
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' path <- file.path("extdata", "mibitof")
#' path <- system.file(path, package = "SpatialData")
#' (spd <- readSpatialData(path))
#' images(spd)
#' 
#' path <- file.path("extdata", "blobs.zarr")
#' path <- system.file(path, package = "SpatialData")
#' (spd <- readSpatialData(path))
#' shapes(spd)
#' 
#' @export
readSpatialData <- function(path, ...) {
  layers <- list.dirs(path, recursive=FALSE)
#browser()
  images <- if ("images" %in% basename(layers)) {
    images <- list.dirs(file.path(path, "images"), recursive=FALSE)
    names(images) <- basename(images)
    lapply(images, readImageArray)
  } else list()
  
  labels <- if ("labels" %in% basename(layers)) {
    labels <- list.dirs(file.path(path, "labels"), recursive=FALSE)
    names(labels) <- basename(labels)
    lapply(labels, readImageArray)
  } else list()
  
  shapes <- if ("shapes" %in% basename(layers)) {
    shapes <- list.dirs(file.path(path, "shapes"), recursive=FALSE)
    names(shapes) <- basename(shapes)
    lapply(shapes, readShapes)
  } else list()
  
  points <- if ("points" %in% basename(layers)) {
    points <- list.dirs(file.path(path, "points"), recursive=FALSE)
    names(points) <- basename(points)
    lapply(points, readPoints)
  } else list()
  
  SpatialData(images=images, labels=labels, shapes=shapes, points=points)
}

# path <- "~/packages/ImageArray/inst/extdata/mibitof"
# readSpatialData(path)