#' @rdname readPoints
#' @title Read `points` element
#' @description ...
#'
#' @param path A character string specifying
#'   a .parquet file-containing directory.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return Arrow \code{\link{Dataset}}
#'
#' @examples
#' path <- "extdata/blobs/points/blobs_points"
#' path <- system.file(path, package = "SpatialData")
#' (ao <- readPoints(path))
#'
#' @author Tim Treis
#'
#' @importFrom arrow open_dataset
#' @export
readPoints <- function(path, ...) {
    dirs <- list.files(path=path, full.names=TRUE, recursive=TRUE)
    dset <- grep("*.parquet", dirs, value=TRUE)
    open_dataset(dset)
}
