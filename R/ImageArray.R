#' @rdname ImageArray
#' @title The `ImageArray` class
#' @description ...
#' 
#' @param data A \code{array} or \code{\link[S4Arrays]{Array}}.
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
  
  msc <- as.list(metadata$multiscales)
  axs <- msc$axes[[1]]
  
  chs <- metadata$channels_metadata$channels$label
  idx <- grep("channel", axs$type)
  
  nms <- vector("list", 3)
  names(nms) <- axs$name
  nms[[idx]] <- chs
  
  dimnames(data) <- nms
  
  .ImageArray(data = data, metadata = metadata)
}

.showImageArray <- function(object) {
  axs <- md$multiscales$axes[[1]]
  cat("class: ImageArray\n")
  cat(sprintf("axiis(%s):", paste(axs$name, collapse="")), dim(object), "\n")
  
  #cat(sprintf("axes(%s):", nrow(axs)), "\n")
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