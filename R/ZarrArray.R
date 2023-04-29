#' @rdname ZarrArray
#' @title The `ZarrArray` class
#' @aliases
#' ZarrArray ZarrArray-class
#' ImageArray ImageArray-class
#' LabelArray LabelArray-class
#' [,ZarrArray-method
#' dim,ZarrArray-method
#' dimnames,ZarrArray-method
#' coord coords transformImage
#' translateImage scaleImage rotateImage
#'
#' @description ...
#'
#' @param data An \code{array} or \code{\link[S4Arrays]{Array}}.
#' @param metadata A \code{list}.
#' @param ... Further arguments to be passed to or from other methods.
#' @param x An \code{ImageArray} object.
#' @param t Transformation data (see Transformations).
#' @param i,j Indices for subsetting (see \code{?base::Extract}).
#' @param subscripts A list of the same length as
#'   the number of the array's dimensions.
#'   Each entry provides the indices
#'   in that dimensions to subset.
#' @param drop Logical specifying whether or not flat
#'   dimensions should be dropped (see \code{?base::Extract}).
#' @param a An array-like object (see `?base::aperm`).
#' @param perm The subscript permutation vector (see `?base::aperm`).
#' @param name A character string specifying the coordinate system to extract.
#' @param coords A character string specifying the target coordinate system.
#'
#' @section Transformations:
#' In the following examples, \code{ia} is a \code{\link{ImageArray}} object.
#' \itemize{
#' \item{\code{translateImage}:
#'   translates xy coordinates according to \code{t},
#'   an integer vector of length 2.
#'   (see \code{\link[EBImage:resize]{translate}})}
#' \item{\code{scaleImage}:
#'   scales the image to the desired dimensions,
#'   a numeric vector of length \code{length(dim(ia))}.
#'   (see \code{\link[EBImage:resize]{resize}})}
#' \item{\code{rotateImage}:
#'   rotates the image clockwise around the origin
#'   according to the given angle \code{t}, a scalar numeric.
#'   (see \code{\link[EBImage:resize]{rotate}})}
#' }
#'
#' @return \code{ImageArray}
#'
#' @examples
#' path <- system.file("extdata", "blobs", package="SpatialData")
#' imgs <- file.path(path, "images", "blobs_image")
#' zarr <- file.path(imgs, "0")
#' json <- file.path(imgs, ".zattrs")
#'
#' library(Rarr)
#' library(jsonlite)
#'
#' za <- read_zarr_array(zarr)
#' md <- fromJSON(json)
#' (ia <- ImageArray(za, md))
#'
#' @author Helena L. Crowell
#'
#' @export
ZarrArray <- function(data=array(), metadata=list(), ...) {
    .ZarrArray(data=data, metadata=metadata)
}

#' @rdname ZarrArray
#' @export
ImageArray <- function(data=array(), metadata=list(), ...) {
    # TODO: lot's of validity checks needed here...
    if (length(metadata) > 0) {
        msc <- as.list(metadata$multiscales)
        axs <- msc$axes[[1]]
        nms <- vector("list", nrow(axs))
        names(nms) <- axs$name
        chs <- metadata$channels_metadata$channels$label
        idx <- grep("channel", axs$type)
        nms[[idx]] <- chs
        dimnames(data) <- nms
    }
    .ImageArray(data=data, metadata=metadata)
}

#' @rdname ZarrArray
#' @export
LabelArray <- function(data=array(), metadata=list(), ...) {
    # TODO: lot's of validity checks needed here...
    if (length(metadata) > 0) {
        msc <- as.list(metadata$multiscales)
        axs <- msc$axes[[1]]
        nms <- vector("list", nrow(axs))
        names(nms) <- axs$name
    }
    .LabelArray(data=data, metadata=metadata)
}
