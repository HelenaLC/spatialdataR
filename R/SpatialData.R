#' @rdname SpatialData
#' @title The `SpatialData` class
#' @aliases
#' image images label labels
#' shape shapes point points table
#' @description ...
#'
#' @param x A \code{SpatialData} object.
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return
#' \itemize{
#' \item \code{images/labels/shapes/points}
#'   return a list of entities of the corresponding element.
#' \item \code{image/label/shape/point}
#'   return a single entitiy of the corresponing type.
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
  cat("table:", if (!is.null(table(object))) dim(table(object)) else "nan")
}

setMethod("show", "SpatialData", .showSpatialData)
