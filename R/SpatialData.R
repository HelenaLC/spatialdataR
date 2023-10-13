#' @rdname SpatialData
#' @title The `SpatialData` class
#' @aliases
#' SpatialData
#' SpatialData-class
#' $,SpatialData-method
#' [[,SpatialData-method
#' element elements elementNames
#' image label shape point table
#' images labels shapes points tables
#' image<- label<- shape<- point<- table<-
#' images<- labels<- shapes<- points<- tables<-
#' imageNames labelNames shapeNames pointNames tableNames
#' imageNames<- labelNames<- shapeNames<- pointNames<- tableNames<-
#' image<-,SpatialData,1
#' image<-,SpatialData,character
#' images<-,SpatialData,list
#' label<-,SpatialData,1
#' label<-,SpatialData,character
#' labels<-,SpatialData,list
#' shape<-,SpatialData,1
#' shape<-,SpatialData,character
#' shapes<-,SpatialData,list
#' point<-,SpatialData,1
#' point<-,SpatialData,character
#' points<-,SpatialData,list
#'
#' @description ...
#'
#' @param x A \code{SpatialData} object.
#' @param tables A list of \code{SingleCellExperiment}s.
#' @param images A list of \code{\link{ImageArray}}s.
#' @param labels A list of \code{\link{ImageArray}}s.
#' @param shapes A list of \code{\link{DataFrame}}s.
#' @param points A list of Arrow \code{\link{Dataset}}s.
#' @param e,name A character string specifying the type of element to extract;
#'   should be one of \code{"images","labels","shapes","points","tables"}.
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
#' dir <- file.path("extdata", "blobs")
#' dir <- system.file(dir, package="SpatialData")
#' (spd <- readSpatialData(dir))
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
SpatialData <- function(images, labels, shapes, points, tables) {
    if (missing(images)) images <- list()
    if (missing(labels)) labels <- list()
    if (missing(shapes)) shapes <- list()
    if (missing(points)) points <- list()
    if (missing(tables)) tables <- list()

    if (!is.list(images)) images <- list(images)
    if (!is.list(labels)) labels <- list(labels)
    if (!is.list(shapes)) shapes <- list(shapes)
    if (!is.list(points)) points <- list(points)
    if (!is.list(tables)) tables <- list(tables)
    
    if (is.null(names(images))) names(images) <- seq_along(images)
    if (is.null(names(labels))) names(labels) <- seq_along(labels)
    if (is.null(names(shapes))) names(shapes) <- seq_along(shapes)
    if (is.null(names(points))) names(points) <- seq_along(points)
    
    names(images)[na] <- seq_len(sum(na <- is.na(names(images))))
    names(labels)[na] <- seq_len(sum(na <- is.na(names(labels))))
    names(shapes)[na] <- seq_len(sum(na <- is.na(names(shapes))))
    names(points)[na] <- seq_len(sum(na <- is.na(names(points))))

    .SpatialData(images=images, labels=labels, shapes=shapes, points=points, tables=tables)
}

#LAYERS <- c(""(#setdiff(names(attributes(SpatialData())), c("metadata", "class"))
LAYERS <- c("images", "labels", "shapes", "points", "tables")
