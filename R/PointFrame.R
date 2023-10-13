#' @rdname PointFrame
#' @title The `PointFrame` class
#' @aliases
#' PointFrame PointFrame-class
#' $,PointFrame-method
#' dim,PointFrame-method
#' length,PointFrame-method
#' coord,PointFrame-method
#' coords,PointFrame-method
#'
#' @description
#' ...
#'
#' @param x An object of class \code{PointFrame}.
#' @param data An object of class \code{\link{arrow}[Table]}.
#' @param metadata A list
#' @param i,j Indices for subsetting (see \code{?base::Extract}).
#' @param name A character string specifying the column extract..
#' @param drop Ignored.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{PointFrame}
#'
#' @examples
#' path <- "extdata/blobs/points/blobs_points"
#' path <- system.file(path, package = "SpatialData")
#' (pf <- readPoints(path))
#'
#' @author Helena L. Crowell
#'
#' @importFrom S4Vectors metadata<-
#' @export
PointFrame <- function(data=NULL, metadata=list(), ...) {
    if (is.null(data)) data <- data.frame()
    pf <- .PointFrame(data=data, ...)
    metadata(pf) <- metadata
    return(pf)
}

# TODO: subsetting; don't want to always read
# everything but this is currently happening...
# should be doable w/ 'arrow' to have a "smarter"
# OI via queries & collecting only when necessary

#' @rdname PointFrame
#' @export
setMethod("names", "PointFrame", \(x) {
    setdiff(names(x@data), "__null_dask_index__") })

#' @rdname PointFrame
#' @export
setMethod("dim", "PointFrame", \(x) {
    c(length(x), length(names(x))) })

#' @rdname PointFrame
setMethod("length", "PointFrame", \(x) nrow(x@data))

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- \(x, pattern="")
    setdiff(names(x@data), "__null_dask_index__")

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
    x@data <- x@data |>
        mutate(.i=1+`__null_dask_index__`) |>
        filter(.i %in% j) |>
        select(-.i)
    return(x)
})

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "missing"), \(x, i, ...) return(x))

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "logical"), \(x, i, ...) {
    if (missing(i) || isTRUE(i)) return(x)
    stopifnot(length(i) == length(x))
    x[which(i)]
})

setAs(
    from="PointFrame", to="data.frame",
    function(from) as.data.frame(from@data)[names(from)])

as.data.frame.PointFrame <- \(x) as.data.frame(x@data)[names(x)]

#' @rdname PointFrame
#' @export
setMethod("as.data.frame", "PointFrame", \(x) as.data.frame.PointFrame(x))