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

.new_sda <- \(type, data=list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    if (is.array(data)) data <- list(data)
    x <- new(type, data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

SpatialDataImage <- \(...) .new_sda("SpatialDataImage", ...)
SpatialDataLabel <- \(...) .new_sda("SpatialDataLabel", ...)

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

# https://github.com/Huber-group-EMBL/Rarr/blob/1795c676e2ac81a9ba2a592c7210cc59036544b6/R/utils.R#L74-L79
.sub <- \(x, ix) rlang::inject(x[!!!ix, drop=FALSE])

#' @exportMethod [
#' @rdname SpatialDataArray
#' @importFrom utils head tail
.sub_sda <- \(x, yx, z=list()) {
    #x <- label(sd); yx <- list(1:10, 1:10); z <- list()
    # yx: spatial; z: channels
    ls <- seq_along(data(x, NULL))
    data(x) <- lapply(ls, \(l) {
        sf <- 2^(l-1)   
        rc <- tail(dim(data(x, l)), 2) 
        # get spatial indices
        .yx <- lapply(seq_along(yx), \(a) {
            ix <- yx[[a]]
            if (isTRUE(ix)) return(seq_len(rc[a]))
            if (is.numeric(ix)) {
                return(seq(
                    floor(head(ix, 1)/sf),
                    min(ceiling(tail(ix, 1)/sf), rc[a])))
            }
            ix 
        })
        # combine leading & spatial indices
        ix <- c(z, .yx)
        # (optional) prepend additional indices
        nd <- length(dim(data(x)))
        na <- nd-length(ix)
        if (na > 0) {
            na <- !logical(na)
            ix <- c(as.list(na), ix)
        }
        .sub(data(x, l), ix)
    })
    x
}

setMethod("[", "SpatialDataImage", \(x, i, j, k, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    if (missing(k)) k <- TRUE else if (isFALSE(k)) k <- 0 else .check_jk(k, "k")
    .sub_sda(x, yx=list(j, k), z=list(i))
})

setMethod("[", "SpatialDataLabel", \(x, i, j, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE else if (isFALSE(i)) i <- 0 else .check_jk(i, "i")
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    .sub_sda(x, yx=list(i, j), z=list())
})
