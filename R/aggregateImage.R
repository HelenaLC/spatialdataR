#' @rdname aggregateImage
#' @title Aggregate to `SingleCellExperiment`
#' @description ...
#'
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param fun Function to use for aggregation.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @examples
#' library(ggplot2)
#' library(SingleCellExperiment)
#' path <- file.path("extdata", "mibitof")
#' path <- system.file(path, package = "SpatialData")
#' spd <- readSpatialData(path)
#' sce <- aggregateImage(spd)
#' cd <- data.frame(colData(sce), z = assay(sce)[1, ])
#' ggplot(cd, aes(x, y, col = z)) + geom_point() +
#'   scale_color_viridis_c() + scale_y_reverse()
#'
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
aggregateImage <- function(x, which=1, fun=mean) {
  img <- as.array(image(x, which))
  lab <- as.array(label(x, which))
  pbs <- t(apply(img, 1, tapply, lab, fun))
  xs <- tapply(col(img[1,,]), lab, fun)
  ys <- tapply(row(img[1,,]), lab, fun)
  cd <- data.frame(x=xs, y=ys)
  rmv <- match("0", colnames(pbs))
  pbs <- pbs[, -1]; cd <- cd[-1, ]
  SingleCellExperiment(list(pbs), colData=cd)
}
