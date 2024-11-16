#' @name PointFrame
#' @title The `PointFrame` class
#'
#' @param x,.data \code{PointFrame}
#' @param data \code{arrow}-derived table for on-disk,
#'   \code{data.frame} for in-memory representation.
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param drop ignored.
#' @param i,j indices for subsetting (see \code{?base::Extract}).
#' @param name character string for extraction (see \code{?base::`$`}).
#' @param ... optional arguments passed to and from other methods.
#'
#' @return \code{PointFrame}
#'
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' x <- file.path(base, "points", "single_molecule")
#' (p <- readPoint(x))
#' 
#' head(as.data.frame(data(p)))
#' (q <- dplyr::filter(p, cell_type == "VISp_wm"))
#' plotPoint(q, c="x", s=0.2)
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
setMethod("dim", "PointFrame", \(x) dim(data(x)))

#' @rdname PointFrame
#' @export
setMethod("length", "PointFrame", \(x) nrow(data(x)))

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- \(x, pattern="") {
    setdiff(names(x@data), "__null_dask_index__") }

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod $
setMethod("$", "PointFrame", \(x, name) {
    collect(select(x@data, all_of(name)))[[1]] })

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod [[
setMethod("[[", "PointFrame", \(x, i, ...) {
    collect(select(x@data, all_of(i)))[[1]] })

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
#' @importFrom dplyr mutate filter select
#' @export
setMethod("[", c("PointFrame", "numeric", "numeric"), \(x, i, j, ...) {
    .i <- `__null_dask_index__` <- NULL # R CMD check
    i <- seq_len(nrow(x))[i]
    x@data <- data(x) |>
        mutate(.i=1+`__null_dask_index__`) |>
        filter(.i %in% i) |>
        select(-.i)
    x@data <- x@data[, j]
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
