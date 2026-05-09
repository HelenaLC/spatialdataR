#' @name SpatialDataImage
#' @title The `SpatialDataImage` class
#' @aliases channels
#' 
#' @param x \code{SpatialDataImage}
#' @param data list of \code{\link{SpatialDataAttrs}}s
#' @param meta \code{\link{SpatialDataAttrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param i,j indices specifying elements to extract.
#' @param k scalar index specifying which scale to extract.
#' @param drop ignored.
#' @param ... option arguments passed to and from other methods.
#'
#' @return \code{SpatialDataImage}
#'
#' @examples
#' zs <- file.path("extdata", "blobs.zarr")
#' zs <- system.file(zs, package="SpatialData")
#' 
#' pa <- list.dirs(
#'   file.path(zs, "images"), 
#'   recursive=FALSE, full.names=TRUE)
#' 
#' # simple
#' readImage(pa[1])
#' 
#' # multi-scale
#' (x <- readImage(pa[2]))
#' 
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
#' @export
SpatialDataImage <- function(data=list(), meta=SpatialDataAttrs(), metadata=list(), ...) {
    x <- .SpatialDataImage(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @export
#' @rdname SpatialDataImage
setMethod("channels", "SpatialDataImage", \(x, ...) channels(meta(x)))

#' @export
#' @rdname SpatialDataImage
setMethod("channels", "ANY", \(x, ...) stop("only 'images' have channels"))

#' @importFrom S4Vectors isSequence
.get_multiscales_paths <- function(x) {
    ps <- list.files(x)
    ps <- suppressWarnings(as.numeric(sort(ps, decreasing=FALSE)))
    ps <- ps[!is.na(ps)]
    if (length(ps)) {
        qs <- seq(min(ps), max(ps))
        if (!isTRUE(all.equal(ps, qs)))
            stop("SpatialDataImage paths are ill-defined, should",
                " be an integer sequence, e.g., 0,1,...,n")
    } else {
      stop("SpatialDataImage path is empty")
    }
    return(ps)
}

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
#' @rdname SpatialDataImage
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
