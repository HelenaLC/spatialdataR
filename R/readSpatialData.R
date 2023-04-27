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
  
  # shapes <- if ("shapes" %in% layers){
  #   readShapes(file.path(path, "shapes"))
  # }else{
  #   NULL
  # }
  # points <- if("points" %in% layers){
  #   readPoints(file.path(path, "points"))
  # }else{
  #   NULL
  # }
  # table <- if("table" %in% layers){
  #   readTable(file.path(path, "table"))
  # }else{
  #   NULL
  # }
  
  SpatialData(images=images, labels=labels)
}

# path <- "~/packages/ImageArray/inst/extdata/mibitof"
# readSpatialData(path)