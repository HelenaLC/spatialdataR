#' @rdname ImageArray
#' @title The `ImageArray` class
#' @description ...
#' 
#' @param path A character string specifying
#'   a .parquet file-containing directory.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' path <- "extdata/blobs/points/blobs_points"
#' path <- system.file(path, package = "SpatialData")
#' (df <- readPoints(path))
#' 
#' @importFrom arrow open_dataset
#' 
#' @export
readPoints <- function(path, ...) {
  dirs <- list.files(path=path, full.names=TRUE, recursive=TRUE)
  dset <- grep("*.parquet", dirs, value=TRUE)
  open_dataset(dset)
}
