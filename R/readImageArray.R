#' @rdname readImageArray
#' @title Read as `ImageArray`
#' @description ...
#' 
#' @param path A character string specifying
#'   a .zarray or .zattrs file-containing directory.
#' @param resolution A charactering specifiying
#'   the image resolution (pyramid level) to read in.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' path <- "extdata/mibitof/images/point8_image/0"
#' path <- system.file(path, package = "SpatialData")
#' (ia <- readImageArray(path))
#' 
#' @importFrom jsonlite fromJSON
#' @importFrom Rarr read_zarr_array
#' @export
readImageArray <- function(path = ".", resolution = "0", ...) { 
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
