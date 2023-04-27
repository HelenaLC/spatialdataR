#' @rdname SpatialData
#' @title The `SpatialData` class
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' library(ggplot2)
#' path <- file.path("extdata", "mibitof")
#' path <- system.file(path, package = "SpatialData")
#' spd <- readSpatialData(path)
#' sce <- SpatialData::aggregate(spd)
#' cd <- data.frame(colData(sce), z = assay(sce)[1, ])
#' ggplot(cd, aes(x, y, col = z)) + geom_point() + 
#'   scale_color_viridis_c() + scale_y_reverse()
#' 
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
aggregateImage <- function(x, which=1) {
  img <- as.array(element(x, "images", which))
  lab <- as.array(element(x, "labels", which))
  pbs <- t(apply(img, 1, tapply, lab, mean))
  xs <- tapply(col(img[1,,]), lab, mean)
  ys <- tapply(row(img[1,,]), lab, mean)
  sce <- SingleCellExperiment(
    assays = list(pbs),
    colData = data.frame(x=xs, y=ys))
}