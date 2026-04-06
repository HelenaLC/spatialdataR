#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param x \code{ImageArray}
#' @param data list of \code{\link[ZarrArray]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param i,j indices specifying elements to extract.
#' @param k scalar index specifying which scale to extract.
#' @param drop ignored.
#' @param ... option arguments passed to and from other methods.
#'
#' @return \code{ImageArray}
#'
#' @examples
#' library(SpatialData.data)
#' dir.create(td <- tempfile())
#' pa <- SpatialData.data:::.unzip_merfish_demo(td)
#' pa <- file.path(pa, "images", "rasterized")
#' (ia <- readImage(pa))
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @export
ImageArray <- function(data=list(), meta=Zattrs(), metadata=list(), ...) {
    x <- .ImageArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname ImageArray
#' @aliases channels
#' @export
setMethod("channels", "Zattrs", \(x, ...) unlist(x$omero$channels))

#' @rdname ImageArray
#' @aliases channels
#' @export
setMethod("channels", "ImageArray", \(x, ...) channels(meta(x)))

#' @rdname ImageArray
#' @export
setMethod("channels", "ANY", \(x, ...) stop("only 'images' have channels"))

#' @importFrom S4Vectors isSequence
.get_multiscales_dataset_paths <- function(md) {
    # validate 'multiscales'
    .validate_multiscales_dataset_path(md)
    # get & validate 'path's
    ds <- md$multiscales[[1]]$datasets
    ps <- vapply(ds, \(.) .$path, character(1))
    ps <- suppressWarnings(as.numeric(sort(ps, decreasing=FALSE)))
    if (length(ps)) {
        qs <- seq(min(ps), max(ps))
        if (!isTRUE(all.equal(ps, qs)))
            stop("ImageArray paths are ill-defined, should",
                " be an integer sequence, e.g., 0,1,...,n")
    }
    return(ps)
}

#' @noRd
.validate_multiscales_dataset_path <- function(md) {
    # validate 'multiscales' 
    ms <- md$multiscales
    if (!is.null(ms)) {
        # validate 'datasets' 
        ds <- ms[[1]]$datasets
        if (!is.null(ds)) {
            # validate 'paths'
            ok <- vapply(ds, \(.) !is.null(.$path), logical(1))
            if (!all(ok))
                stop("'ImageArray' paths are ill-defined,",
                    " no 'path' attribute under 'multiscale-datasets'")
        } else stop(
            "'ImageArray' paths are ill-defined,",
            " no 'datasets' attribute under 'multiscale'")
    } else stop( 
        "'ImageArray' paths are ill-defined,",
        " no 'multiscales' attribute under '.zattrs'")
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

#' @rdname ImageArray
#' @importFrom utils head tail
#' @exportMethod [
setMethod("[", "ImageArray", \(x, i, j, k, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    if (missing(k)) k <- TRUE else if (isFALSE(k)) k <- 0 else .check_jk(k, "k")
    ijk <- list(i, j, k)
    n <- length(data(x, NULL))
    d <- dim(data(x))
    x@data <- lapply(seq_len(n), \(.) {
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
