# TODO: subsetting; don't want to always read
# everything but this is currently happening...
# should be doable w/ 'arrow' to have a "smarter" OI

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- function(x, pattern="") {
    setdiff(names(x@data), "__null_dask_index__")
}

#' @rdname PointFrame
#' @importFrom dplyr collect select all_of
#' @exportMethod $
setMethod("$", "PointFrame", function(x, name) {
    collect(select(x@data, all_of(name)))[[1]]
})

#' @rdname PointFrame
#' @importFrom dplyr collect select all_of
#' @exportMethod [[
setMethod("[[", "PointFrame", function(x, i, ...) {
    collect(select(x@data, all_of(i)))[[1]]
})

setAs(
    from="PointFrame", to="data.frame",
    function(from) as.data.frame(from@data)[names(from)])

#' @rdname PointFrame
#' @aliases alias
#' @export
setMethod("as.data.frame", "PointFrame", function(x) {
    as.data.frame(x@data)[names(x)]
})

#' @rdname PointFrame
setMethod("length", "PointFrame", function(x) nrow(x@data))

#' @rdname PointFrame
#' @export
setMethod("names", "PointFrame", function(x)
    setdiff(names(x@data), "__null_dask_index__"))

#' @rdname PointFrame
#' @export
setMethod("dim", "PointFrame", function(x)
    c(length(x), length(names(x))))
