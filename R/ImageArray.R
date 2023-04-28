#' @rdname ImageArray
#' @title The `ImageArray` class
#' @description ...
#' 
#' @param data An \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' 
#' @examples
#' dir <- "extdata/mibitof/images/point8_image"
#' zarr <- system.file(file.path(dir, "0"), package = "SpatialData")
#' json <- system.file(file.path(dir, ".zattrs"), package = "SpatialData")
#' 
#' library(Rarr)
#' library(jsonlite)
#' 
#' za <- read_zarr_array(zarr)
#' md <- fromJSON(json)
#' (ia <- ImageArray(za, md))
#' 
#' @export
ImageArray <- function(data, metadata = list(), ...) { 
  
  # path <- "~/Packages/SpatialData/inst/extdata/mibitof/labels/point16_labels/"
  # data <- read_zarr_array(file.path(path, "0"))
  # metadata <- fromJSON(file.path(path, ".zattrs"))
  
  msc <- as.list(metadata$multiscales)
  axs <- msc$axes[[1]]
  
  nms <- vector("list", nrow(axs))
  names(nms) <- axs$name
  #chs <- metadata$channels_metadata$channels$label
  #idx <- grep("channel", axs$type)
  #nms[[idx]] <- chs

  dimnames(data) <- nms
  
  .ImageArray(data = data, metadata = metadata)
}

.showImageArray <- function(object) {
  axs <- metadata(object)$multiscales$axes[[1]]
  cat("class: ImageArray\n")
  cat(sprintf("axiis(%s):", paste(axs$name, collapse="")), dim(object), "\n")

  t <- axs$type == "time"
  s <- axs$type == "space"
  c <- axs$type == "channel"
  cat(sprintf("|-time(%s):", sum(t)), axs$name[t], "\n")
  cat(sprintf("|-space(%s):", sum(s)), axs$name[s], "\n")
  cat(sprintf("|-channel(%s):", sum(c)), axs$name[c], "\n")

  chs <- metadata(object)$channels_metadata$channels$label
  cat("channels:", chs, "\n")
}

#' @export
setMethod("show", "ImageArray", .showImageArray)