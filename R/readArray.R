#' @rdname readArray
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
#' (ia <- readArray(file.path(path, "images", "blobs_image")))
#' (la <- readArray(file.path(path, "labels", "blobs_labels")))
#'
#' @author Helena L. Crowell
#'
#' @importFrom jsonlite fromJSON
#' @importFrom Rarr read_zarr_array
#' @export
readArray <- function(path=".", resolution="0", ...) {
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

    is_img <- !is.null(md$channels_metadata)
    fun <- if (is_img) ImageArray else LabelArray
    fun(data=za, metadata=list(), zattrs=Zattrs(md))
}
