#' @name PointFrame
#' @title The `PointFrame` class
#'
#' @description
#' The \code{PointFrame} class stores \code{SpatialData} elements from its 
#' \code{"points"} layers. These are represented as \code{\link[arrow]{Table}}
#' (\code{data} slot) associated with .zattrs stored as \code{\link{Zattrs}} 
#' (\code{meta} slot); a list of \code{metadata} stores other arbitrary info.
#'  
#' Currently defined methods (here, \code{x} is a \code{PointFrame}):
#' \itemize{
#' \item \code{data/meta(x)} to access underlying \code{Table/Zattrs}
#' \item \code{names(x)} returns the underlying table's column names
#' \item \code{dim(x)} returns the dimensions of \code{data(x)}
#' \item \code{`$`,`[[`} directly access columns of \code{data(x)}
#' \item \code{filter,select} to subset rows/columns à la \code{dplyr}
#' \item \code{as.data.frame} to coerce \code{x} to a \code{data.frame}
#' }
#'
#' @param x \code{PointFrame}
#' @param data \code{arrow}-derived table for on-disk,
#'   \code{data.frame} for in-memory representation.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param i,j indices for subsetting (see \code{?base::Extract}).
#' @param drop ignored.
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{PointFrame}
#'
#' @examples
#' library(SpatialData.data)
#' dir.create(tf <- tempfile())
#' base <- SpatialData.data:::.unzip_merfish_demo(tf)
#' x <- file.path(base, "points", "single_molecule")
#' (p <- readPoint(x))
#' 
#' head(as.data.frame(data(p)))
#' (q <- dplyr::filter(p, cell_type == "VISp_wm"))
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @export
PointFrame <- function(data=data.frame(), meta=Zattrs(), metadata=list(), ...) {
    x <- .PointFrame(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname PointFrame
#' @export
setMethod("names", "PointFrame", \(x) {
    setdiff(names(data(x)), "__null_dask_index__") })

#' @rdname PointFrame
#' @export
setMethod("dim", "PointFrame", \(x) c(nrow(data(x)), length(names(x))))

#' @rdname PointFrame
#' @export
setMethod("length", "PointFrame", \(x) nrow(data(x)))

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod [[
setMethod("[[", "PointFrame", \(x, i, ...) {
    x <- select(data(x), !"__null_dask_index__")
    collect(select(x, all_of(i)))[[1]] })

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- \(x, pattern="") {
    setdiff(names(data(x)), "__null_dask_index__") }

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod $
setMethod("$", "PointFrame", \(x, name) do.call(`[[`, list(x, name)))
    # x <- select(data(x), !"__null_dask_index__")
    # collect(select(x, all_of(name)))[[1]] })

# sub ----

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "missing", "ANY"), 
    \(x, i, j, ...) x[seq_len(nrow(x)), j])

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "ANY", "missing"), 
    \(x, i, j, ...) x[i, seq_len(ncol(x))])

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "missing", "missing"), 
    \(x, i, j, ...) x[seq_len(nrow(x)), seq_len(ncol(x))])

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "ANY", "character"), \(x, i, j, ...) {
    stopifnot(all(j %in% names(x)))
    x[i, match(j, names(x))]
})

#' @rdname PointFrame
#' @importFrom dplyr mutate filter select
#' @export
setMethod("[", c("PointFrame", "numeric", "numeric"), \(x, i, j, ...) {
    .i <- `__null_dask_index__` <- NULL # R CMD check
    i <- seq_len(nrow(x))[i]
    x@data <- data(x) |>
        mutate(.i=1+`__null_dask_index__`) |>
        filter(.i %in% i) |>
        select(-.i)
    # make sure this is kept in any case
    ndi <- "__null_dask_index__"
    ndi <- match(ndi, names(x@data), nomatch=0)
    x@data <- x@data[, c(j, ndi)]
    return(x)
})

#' @rdname PointFrame
#' @importFrom BiocGenerics as.data.frame
#' @export
setMethod("as.data.frame", "PointFrame", \(x) as.data.frame(data(x))[names(x)])

setAs(
    from="PointFrame", to="data.frame",
    function(from) as.data.frame(from))

#' @importFrom dplyr filter
#' @export
filter.PointFrame <- \(.data, ...) { 
    .data@data <- filter(data(.data), ...)
    return(.data)
}

#' @importFrom dplyr select
#' @export
select.PointFrame <- \(.data, ...) { 
    .data@data <- select(data(.data), ...)
    return(.data)
}
