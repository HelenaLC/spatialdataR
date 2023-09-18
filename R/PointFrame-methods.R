# TODO: subsetting; don't want to always read
# everything but this is currently happening...
# should be doable w/ 'arrow' to have a "smarter"
# OI via queries & collecting only when necessary

#' @importFrom utils .DollarNames
#' @export
.DollarNames.PointFrame <- function(x, pattern="") {
    setdiff(names(x@data), "__null_dask_index__")
}

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod $
setMethod("$", "PointFrame", function(x, name) {
    collect(select(x@data, all_of(name)))[[1]]
})

#' @rdname PointFrame
#' @importFrom dplyr select all_of collect
#' @exportMethod [[
setMethod("[[", "PointFrame", function(x, i, ...) {
    collect(select(x@data, all_of(i)))[[1]]
})

setAs(
    from="PointFrame", to="data.frame",
    function(from) as.data.frame(from@data)[names(from)])

#' @rdname PointFrame
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

#' @rdname PointFrame
#' @importFrom dplyr mutate filter select
#' @export
setMethod("[", c("PointFrame", "numeric"), function(x, i, ...) {
    j <- seq_len(length(x))[i]
    x@data <- x@data %>%
        mutate(.i=1+`__null_dask_index__`) %>%
        filter(.i %in% j) %>%
        select(-.i)
    return(x)
})

setMethod("[", c("PointFrame", "missing"), function(x, i, ...) return(x))

#' @rdname PointFrame
#' @export
setMethod("[", c("PointFrame", "logical"), function(x, i, ...) {
    if (missing(i) || isTRUE(i)) return(x)
    stopifnot("invalid length of logical index"=length(i) == length(x))
    x[which(i)]
})
