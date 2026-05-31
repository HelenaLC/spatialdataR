#' @name SpatialDataArray
#' @title \code{SpatialDataArray}
#' @aliases data_type channels 
#' 
#' @description
#' The \code{SpatialDataImage} and \code{-Label} classes represent 
#' elements from a \code{SpatialData}'s \code{images/} and \code{labels/} 
#' layers, respectively. In both cases, these  are represented as a 
#' \code{ZarrArray} (\code{data} slot), and associated with .zattrs 
#' represented as \code{\link{SpatialDataAttrs}} (\code{meta} slot); 
#' a list of \code{metadata} stores other arbitrary info.
#' 
#' Currently defined methods (here, \code{x} is a \code{SpatialDataArray}):
#' \itemize{
#' \item \code{data/meta(x)} access underlying data/.zattrs
#' \item \code{data_type(x)} gets the underlying data type (e.g., float64)
#' \item \code{channels(x)} gets channel names (applies to images only)
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' \item \code{length(x)} returns the length of \code{data(x)}
#' }
#' 
#' @param x \code{SpatialDataArray}
#' @param data list of \code{ZarrArray}s
#' @param meta \code{\link{SpatialDataAttrs}}
#' @param metadata optional list of arbitrary additional content.

#' @param ... option arguments passed to and from other methods.
#' @param i,j,k indices specifying elements/slices to extract.
#' @param drop ignored.
#'
#' @return \code{SpatialDataArray}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="spatialdataR")
#' 
#' # get path to 'i'th element in layer 'l'
#' fn <- \(l, i=1) list.dirs(file.path(zs, l), recursive=FALSE)[i]
#' 
#' # label
#' (x <- readLabel(fn("labels")))
#' x[1:10, 1:10]
#' meta(x)
#' 
#' # image
#' readImage(fn("images"))
#' 
#' # multi-scale
#' (x <- readImage(fn("images", 2)))
#' 
#' channels(x)
#' dim(data(x, 1))   # highest res.
#' dim(data(x, Inf)) # lowest res.
#' 
#' # RGB visual
#' rgb <- apply(
#'   data(x, 1), c(2, 3), 
#'   \(.) rgb(.[1], .[2], .[3]))
#' plot(
#'   row(rgb), col(rgb), col=rgb, 
#'   pch=15, asp=1, ylim=c(ncol(rgb), 0))
NULL

# new ----

#' @export
#' @rdname SpatialDataArray
#' @importFrom methods new
#' @importFrom S4Vectors metadata<-
SpatialDataImage <- function(data = list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    x <- .SpatialDataImage(data=as_imgarray(data, 3), meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @export
#' @rdname SpatialDataArray
#' @importFrom methods new
#' @importFrom S4Vectors metadata<-
SpatialDataLabel <- function(data = list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    x <- .SpatialDataLabel(data=as_imgarray(data, 2), meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @noRd
as_imgarray <- function(data, dim){
  if(is(data, "ImageArray")){
    data
  } else {
    if(!is.list(data))
      data <- list(data)
    ImageArray(levels = data, 
               meta = list(axes = c(if(dim == 3) "c" else NULL, "y", "x")))
  }
}

# utils ----

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
setMethod("data_type", "DelayedArray", \(x) {
    df <- zarr_overview(path(x), as_data_frame=TRUE)
    return(df$data_type)
})

# chs ----

# internal use only!
#' @noRd 
.ch <- \(x) {
    v <- tryCatch(.ome_ver(x), error=\(e) NULL)
    if (is.null(v)) return()
    if (v == "0.5") x <- x$ome
    unlist(x$omero$channels)
}

#' @export
#' @rdname SpatialDataArray
setMethod("channels", "SpatialDataAttrs", \(x, ...) .ch(x))

#' @export
#' @rdname SpatialDataArray
setMethod("channels", "SpatialDataImage", \(x, ...) channels(meta(x)))

#' @export
#' @rdname SpatialDataArray
setMethod("channels", "SpatialDataElement", \(x, ...) stop("only 'images' have channels"))

# compares metadata dataset paths to arrays on disk
.validate_multiscales_paths <- function(x, ds) {
    ps <- list.files(x)
    ds <- ds[ds %in% ps]
    if (!length(ds))
        stop("Invalid 'SpatialData' image or label:",
            " metadata does not match the names of Zarr arrays")
    return(ds)
}

# sub ----

#' @exportMethod [
#' @rdname SpatialDataArray
setMethod("[", "SpatialDataArray", \(x, i, j,..., drop=FALSE) {
    cl <- sys.call()
    cl[[2]] <- substitute(data(x, NULL))
    data(x) <- eval(cl, parent.frame())
    x
})