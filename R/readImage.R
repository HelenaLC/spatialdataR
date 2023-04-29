#' @rdname readImage
#' @title Read `images/labels` element
#' @description ...
#'
#' @param path A character string specifying
#'   a .zarray or .zattrs file-containing directory.
#' @param resolution A charactering specifiying
#'   the image resolution (pyramid level) to read in.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{ImageArray}}
#'
#' @examples
#' path <- system.file("extdata", "blobs", package="SpatialData")
#' (ia <- readImage(file.path(path, "images", "blobs_image")))
#'
#' @author Helena L. Crowell
#'
#' @importFrom jsonlite fromJSON
#' @importFrom Rarr read_zarr_array
#' @export
readImage <- function(path=".", resolution="0", ...) {
    if (file.exists(file.path(path, ".zarray"))) {
        json <- file.path(dirname(path), ".zattrs")
        if (!file.exists(json))
            stop("couldn't find .zattrs upstream of .zarray")
        zarr <- path
    } else {
        json <- file.path(path, ".zattrs")
        zarr <- file.path(path, resolution)
        if (!file.exists(zarr))
            stop("couldn't find .zarray under resolution /", resolution)
    }
    md <- fromJSON(json)
    za <- read_zarr_array(zarr)
    ImageArray(data=za, metadata=md)
}
