#' @name SpatialDataLabel
#' @title The \code{SpatialDataLabel} class
#'
#' @description 
#' The \code{SpatialDataLabel} class stores \code{SpatialData} elements from its 
#' \code{"labels"} layers. These are represented as a \code{ZarrMatrix} 
#' (\code{data} slot) associated with \code{\link{SpatialDataAttrs}} 
#' (\code{meta} slot); a list of \code{metadata} stores other arbitrary info.
#'
#' Currently defined methods (here, \code{x} is a \code{SpatialDataLabel}):
#' \itemize{
#' \item \code{data/meta(x)} to access underlying \code{ZarrMatrix/SpatialDataAttrs}
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' }
#'
#' @param x \code{SpatialDataLabel}
#' @param data list of \code{\link[ZarrArray]{ZarrArray}}s
#' @param meta \code{\link{SpatialDataAttrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param multiscale if TRUE (and \code{data} is not a list), 
#' multiscale image will be generated.
#' @param axes axes
#' @param i,j indices specifying elements to extract.
#' @param drop ignored.
#' @param ... option arguments passed to and from other methods.
#' 
#' @return \code{SpatialDataLabel}
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- file.path(x, "labels", "blobs_labels")
#' 
#' (y <- readLabel(x))
#' y[1:10, 1:10]
#' meta(y)
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @export
SpatialDataLabel <- function(data=list(), 
                             meta=SpatialDataAttrs(label = TRUE),
                             version = image(sdFormat(0.1)),
                             metadata=list(),
                             scale_factors = NULL, ...) {
    if(!is.list(data))
      data <- list(data)  
    if(!is.null(scale_factors)){
      data <- .generate_multiscale(data[[1]], 
                                   axes = vapply(axes(meta), 
                                                 \(.) .$name, 
                                                 character(1)), 
                                   scale_factors = scale_factors, 
                                   method = "label")
      meta <- SpatialDataAttrs(scale_factors = scale_factors, label = TRUE) 
    }
    x <- .SpatialDataLabel(data=data, meta=meta, ...)
    metadata(x) <- metadata
    
    # update version if provided
    if(!is.null(version))
      version(x) <- version
    return(x)
}

#' @rdname SpatialDataLabel
#' @importFrom utils head tail
#' @exportMethod [
setMethod("[", "SpatialDataLabel", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE else if (isFALSE(i)) i <- 0 else .check_jk(i, "i")
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    n <- length(data(x, NULL))
    d <- dim(data(x, 1))
    data(x) <- lapply(seq_len(n), \(.) {
        i <- if (isTRUE(i)) seq_len(d[1]) else i
        j <- if (isTRUE(j)) seq_len(d[2]) else j
        ij <- lapply(list(i, j), \(ij) {
            fac <- 2^(.-1)
            seq(floor(head(ij, 1)/fac), 
                ceiling(tail(ij, 1)/fac))
        })
        data(x, .)[ij[[1]], ij[[2]], drop=FALSE]
    })
    x
})