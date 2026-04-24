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
#' zs <- get_demo_SDdata("merfish")
#' x <- file.path(zs, "points", "single_molecule")
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
    setdiff(colnames(data(x)), "__null_dask_index__") })

#' @rdname PointFrame
#' @export
setMethod("dim", "PointFrame", \(x) c(length(x), length(names(x))))

#' @rdname PointFrame
#' @export
#' @importFrom dplyr tally pull
setMethod("length", "PointFrame", \(x) {
    # suppress warning caused by the 'geometry' column being dropped
    # duckspatial::ddbs_drop_geometry() is an alternative, but fails if
    # 'geometry' is the only column
    suppressWarnings({
        data(x) |> tally() |> pull(n)
    })
})

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod [[
setMethod("[[", "PointFrame", \(x, i, ...) {
    x <- select(data(x), !"__null_dask_index__")
    collect(select(x, all_of(i)))[[1]] })

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- \(x, pattern="") {
    setdiff(colnames(data(x)), "__null_dask_index__") }

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod $
setMethod("$", "PointFrame", \(x, name) do.call(`[[`, list(x, name)))

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
