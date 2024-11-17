#' @name SpatialData
#' @title The `SpatialData` class
#' 
#' @aliases
#' SpatialData-class
#' $,SpatialData-method
#' [[,SpatialData-method
#' image images image<- images<- imageNames 
#' label labell label<- labels<- labelNames 
#' point points point<- points<- pointNames 
#' image images image<- images<- imageNames 
#' shape shapes shape<- shapes<- shapeNames 
#' table tables table<- tables<- tableNames 
#' 
#' @description ...
#' 
#' @param images list of \code{\link{ImageArray}}s
#' @param labels list of \code{\link{LabelArray}}s
#' @param points list of \code{\link{PointFrame}}s
#' @param shapes list of \code{\link{ShapeFrame}}s
#' @param tables list of \code{SingleCellExperiment}s
#' @param x \code{SpatialData}
#' @param i character string, scalar or vector of indices
#'   specifying the element to extract from a given layer.
#' @param j ignored.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param value (list of) element(s) with layer-compliant object(s), 
#'   or NULL/\code{list()} to remove an element/layer completely.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{SpatialData}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' (x <- readSpatialData(x, tables=FALSE))
#' 
#' # subsetting
#' # layers are taken in order of appearance
#' # (images, labels, points, shapes, tables)
#' x[-4] # drop layer
#' x[4, -2] # drop element
#' x["shapes", c(1, 3)] # subset layer
#' x[c(1, 2), list(1, c(1, 2))] # multiple
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
        points=points, shapes=shapes, tables=tables)
}
    
. <- c("images", "labels", "points", "shapes", "tables")
names(.LAYERS) <- .LAYERS <- .
