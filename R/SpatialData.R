#' @rdname SpatialData
#' @title The `SpatialData` class
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' path <- "extdata/mibitof/images/point8_image/mibitof"
#' path <- system.file(path, package = "SpatialData")
#' (spd <- readSpatialData(path))
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

.showSpatialData <- function(object) {
  cat("class: SpatialData")
  cat(sprintf("images(%s):", length(object@images)), "\n")
  cat(sprintf("labels(%s):", length(object@images)), "\n")
  cat(sprintf("shapes(%s):", length(object@images)), "\n")
  cat(sprintf("points(%s):", length(object@images)), "\n")
}

setMethod("show", "SpatialData", .showSpatialData)