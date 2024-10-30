#' @name PointFrame
#' @title The `PointFrame` class
#'
#' @return \code{PointFrame}
#'
#' @examples
#' x <- file.path("extdata", "merfish.zarr")
#' x <- file.path(x, "points", "single_molecule")
#' x <- system.file(x, package="SpatialData")
#' (p <- readPoint(x))
#' 
#' head(as.data.frame(data(p)))
#' (q <- filter(p, cell_type == "VISp_wm"))
#' plotPoint(q, c="x", s=0.2)
#'
#' @importFrom S4Vectors metadata<-
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

#' @rdname PointFrame
#' @importFrom dplyr mutate filter select
#' @export
setMethod("[", c("PointFrame", "numeric"), \(x, i, ...) {
    .i <- `__null_dask_index__` <- NULL # R CMD check
    j <- seq_len(length(x))[i]
    x@data <- data(x) |>
        mutate(.i=1+`__null_dask_index__`) |>
        filter(.i %in% j) |>
        select(-.i)
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
filter.PointFrame <- \(x, ...) { x@data <- filter(data(x), ...); x }

#' @importFrom dplyr select
#' @export
select.PointFrame <- \(x, ...) { x@data <- select(data(x), ...); x }