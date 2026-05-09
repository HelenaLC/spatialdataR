#' @name SpatialDataArray
#' @title Methods for `SpatialDataArray`s
#' 
#' @param x \code{SpatialDataImage} or \code{SpatialDataLabel}.
#' @param k scalar index specifying which scale to extract.
#' 
#' @return \code{SpatialDataArray}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' 
#' pa <- list.dirs(
#'   file.path(zs, "images"), 
#'   recursive=FALSE, full.names=TRUE)
#' 
#' (x <- readImage(pa[2]))  
#' 
#' data_type(x)
#' dim(data(x, 1))   # highest res.
#' dim(data(x, Inf)) # lowest res.
#' 
#' rgb <- apply(
#'   data(x, 1), c(2, 3), 
#'   \(.) rgb(.[1], .[2], .[3]))
#' plot(
#'   row(rgb), col(rgb), col=rgb, 
#'   pch=15, asp=1, ylim=c(ncol(rgb), 0))
#' 
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
NULL

#' @rdname SpatialDataArray
#' @export
setMethod("data", "SpatialDataArray", \(x, k=1) {
    # direct accession needed here
    # to get at available scales
    x <- x@data 
    if (is.null(k)) return(x)
    stopifnot(length(k) == 1, is.numeric(k), k > 0)
    n <- length(x) # get number of available scales
    if (is.infinite(k)) k <- n # input of Inf uses lowest
    if (k <= n) return(x[[k]]) # return specified scale
    stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @rdname SpatialDataArray
#' @export
setMethod("dim", "SpatialDataArray", \(x) dim(data(x)))

#' @rdname SpatialDataArray
#' @export
setMethod("length", "SpatialDataArray", \(x) length(data(x, NULL)))

#' @export
#' @rdname SpatialDataArray
#' @importFrom S4Vectors metadata
setMethod("data_type", "SpatialDataArray", \(x) {
    if (is(y <- data(x), "DelayedArray")) 
        data_type(y) else metadata(x)$data_type
})

#' @export
#' @rdname SpatialDataArray
#' @importFrom DelayedArray DelayedArray
#' @importFrom Rarr zarr_overview
#' @importFrom ZarrArray path
setMethod("data_type", "DelayedArray", \(x) zarr_overview(path(x), as_data_frame=TRUE)$data_type)
