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
#' (pf <- readPoints(path))
#'
#' @author Tim Treis
#'
#' @importFrom arrow read_parquet
#' @export
readPoints <- function(path, ...) {
    path <- gsub("/$", "", path)
    za <- fromJSON(file.path(path, ".zattrs"))
    dirs <- list.files(path, full.names=TRUE, recursive=TRUE)
    pq <- grep("*\\.parquet$", dirs, value=TRUE)
    at <- read_parquet(pq, as_data_frame=FALSE)
    PointFrame(data=at, zattrs=Zattrs(za))
}
