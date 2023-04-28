#' @rdname ImageArray
#' @title The `ImageArray` class
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' path <- "extdata/blobs.zarr/points/blobs_points"
#' path <- system.file(path, package = "SpatialData")
#' (df <- readPoints(path))
#' 
#' @importFrom arrow open_dataset
#' 
#' @export
readPoints <- function(path, ...) {
  dirs <- list.files(path=path, full.names=TRUE, recursive=TRUE)
  dset = grep("*.parquet", dirs, value=TRUE)
  open_dataset(dset)
}
