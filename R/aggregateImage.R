#' @rdname aggregateImage
#' @title Aggregate to `SingleCellExperiment`
#' @description ...
#'
#' @param x A \code{\link{SpatialData}} object.
#' @param image,label Index or character string specifying
#'   the image/label to use; if a string is provided,
#'   should be one of \code{image/labelNames(x)}.
#' @param fun Function to use for aggregation.
#'
#' @return
#' An object of class \code{\link{SingleCellExperiment}}
#' where rows = image channels and columns = unique labels.
#'
#' @examples
#' library(ggplot2)
#' library(SingleCellExperiment)
#' path <- file.path("extdata", "blobs")
#' path <- system.file(path, package = "SpatialData")
#' spd <- readSpatialData(path)
#' sce <- aggregateImage(spd)
#' cd <- data.frame(colData(sce), z = assay(sce)[1, ])
#' ggplot(cd, aes(x, y, col = z)) + geom_point() +
#'   scale_color_viridis_c() + scale_y_reverse()
#'
#' @author Helena L. Crowell
#'
#' @importFrom SingleCellExperiment SingleCellExperiment
#' @export
aggregateImage <- function(x, image=1, label=1, fun=mean) {
  img <- as.array(image(x, image))
  lab <- as.array(label(x, label))
  pbs <- t(apply(img, 1, tapply, lab, fun))
  xs <- tapply(col(img[1,,]), lab, fun)
  ys <- tapply(row(img[1,,]), lab, fun)
  cd <- data.frame(x=xs, y=ys)
  rmv <- match("0", colnames(pbs))
  pbs <- pbs[, -1]; cd <- cd[-1, ]
  SingleCellExperiment(list(pbs), colData=cd)
}
