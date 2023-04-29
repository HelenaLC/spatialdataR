#' @rdname writeImageArray
#' @title Write `ImageArray` to Zarr-array
#' @description ...
#'
#' @param image A "ImageArray" specifying the image
#'   to be saved.
#' @param path A character string specifying
#'   a path
#' @param ... Further arguments to be passed to write_zarr_array.
#'
#' @return \code{NULL}
#'
#' @examples
#' path <- "/path/to/my/image.zarr"
#' path <- system.file(path, package = "SpatialData")
#' writeImageArray(image, path)
#'
#' @importFrom jsonlite toJSON
#' @importFrom Rarr write_zarr_array
#' @export
writeImageArray <- function(image, path, ...) {
  stopifnot("image must be of type 'ImageArray'" =  is(image, "ImageArray"))
  stopifnot("path must be of type 'character'" =  is.character(path))

  if (file.exists(path))
    stop("path already exists")

  # get list of optional arguments
  dots <- list(...)

  # check if chunk_dim has been passed, otherwise assign with no chunk
  if (!is.null(dots$chunk_dim)) {
    chunk_dim <- dots$chunk_dim
  } else {
    chunk_dim <- dim(image)
  }

  # "pop" chunk_dim from dots
  dots <- dots[setdiff(names(dots), "chunk_dim")]

  args = list(x=image@data, zarr_array_path = path,  chunk_dim = chunk_dim)
  if (length(dots) > 0) args <- c(args, dots)
  do.call(write_zarr_array, args)

  # get metadata and write to file
  metadata <- toJSON(image@metadata)

  # TODO: once this is fixed https://github.com/grimbough/Rarr/issues/1 adapt
  write(metadata, file.path(paste0(path,path),"/.zattrs"))
}
