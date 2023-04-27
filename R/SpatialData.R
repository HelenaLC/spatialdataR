#' @rdname SpatialData
#' @title The `SpatialData` class
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
  imgs <- images(object)
  labs <- labels(object)
  shps <- shapes(object)
  pnts <- points(object)
  cat("class: SpatialData\n")
  cat(sprintf("images(%s):", length(imgs)), names(imgs), "\n")
  cat(sprintf("labels(%s):", length(labs)), names(labs), "\n")
  cat(sprintf("shapes(%s):", length(shps)), names(shps), "\n")
  cat(sprintf("points(%s):", length(pnts)), names(pnts), "\n")
}

setMethod("show", "SpatialData", .showSpatialData)