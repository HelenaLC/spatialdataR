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
SpatialDataImage <- \(data=list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    if (is.array(data)) data <- list(data)
    x <- .SpatialDataImage(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @export
#' @rdname SpatialDataArray
#' @importFrom methods new
#' @importFrom S4Vectors metadata<-
SpatialDataLabel <- \(data=list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    if (is.array(data)) data <- list(data)
    x <- .SpatialDataLabel(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
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

.check_jk <- \(x, .) {
    if (isTRUE(x)) return()
    tryCatch(
        stopifnot(
            is.numeric(x), x == round(x),
            diff(range(x)) == length(x)-1,
            (y <- abs(x)) == seq(min(y), max(y))
        ),
        error=\(e) stop(sprintf("invalid '%s'", .))
    )
}

#' @exportMethod [
#' @rdname SpatialDataArray
#' @importFrom utils head tail
setMethod("[", "SpatialDataImage", \(x, i, j, k, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    if (missing(k)) k <- TRUE else if (isFALSE(k)) k <- 0 else .check_jk(k, "k")
    ijk <- list(i, j, k)
    n <- length(data(x, NULL))
    d <- dim(data(x))
    data(x) <- lapply(seq_len(n), \(.) {
        j <- if (isTRUE(j)) seq_len(d[2]) else j
        k <- if (isTRUE(k)) seq_len(d[3]) else k
        jk <- lapply(list(j, k), \(jk) {
            fac <- 2^(.-1)
            seq(floor(head(jk, 1)/fac), 
                ceiling(tail(jk, 1)/fac))
        })
        data(x, .)[i, jk[[1]], jk[[2]], drop=FALSE]
    })
    x
})

#' @exportMethod [
#' @rdname SpatialDataArray
#' @importFrom utils head tail
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
