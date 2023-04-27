#' @rdname readImageArray
#' @title Read as `ImageArray`
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
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
  
# path <- "/Users/helucro/Packages/ImageArray/inst/extdata/mibitof/images/point8_image/0"
# readImage(path)
