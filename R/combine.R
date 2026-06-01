#' @name combine
#' @title Combine two \code{SpatialData} objects
#' 
#' @param x,y \code{SpatialData} objects to combine.
#' @param ... ignored.
#' 
#' @returns
#' A \code{SpatialData} objects containing all elements 
#' from \code{x} and \code{y} with names made unique.
#' 
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="spatialdataR")
#' x <- readSpatialData(x)
#' 
#' y <- combine(x, x)
#' imageNames(y)
#' region(table(y, 1))
#' region(table(y, 2))
#' 
#' y <- combine(list(Alpha=x, x, Omega=x))
#' shapeNames(y)
#' 
#' @importFrom BiocGenerics combine
NULL

.combine <- \(xs, old, new) {
    for (i in seq_along(xs)) {
        x <- xs[[i]]
        # elements that might be referred to by tables (labels, shapes)
        old_nms <- unlist(colnames(x)[.ls])
        j <- match(old_nms, old[[i]])
        new_nms <- new[[i]][j]
        # rename elements
        for (l in .ls) {
            j <- match(names(x[[l]]), old[[i]])
            names(x[[l]]) <- new[[i]][j]
        }
        # sync tables
        x <- .sync_tables_sdattrs(x, old_nms, new_nms)
        # rename tables themselves
        j <- match(tableNames(x), old[[i]])
        tableNames(x) <- new[[i]][j]
        xs[[i]] <- x
    }
    names(ls) <- ls <- .LAYERS
    args <- lapply(ls, \(l) do.call(c, lapply(unname(xs), \(x) x[[l]])))
    do.call(SpatialData, args)
}

#' @export
#' @rdname combine
setMethod("combine", c("list", "missing"), \(x, y, ...) {
    # validate input
    ok <- all(vapply(x, \(.) is(., "SpatialData"), logical(1)))
    if (!ok) stop("'x' should be a list of 'SpatialData' objects")
    # get current element names
    old <- lapply(x, \(z) unlist(colnames(z)))
    # get list names; if missing, use empty strings
    if (is.null(nms <- names(x))) 
        nms <- character(length(x))
    # prepend list names to element names where available
    new <- lapply(seq_along(x), \(i) {
        if (nms[i] == "") return(old[[i]])
        paste(nms[i], old[[i]], sep=".")
    })
    # ensure global uniqueness
    new <- split(
        make.unique(unlist(new)), 
        rep(seq_along(new), lengths(new)))
    .combine(x, old, new)
})

#' @export
#' @rdname combine
setMethod("combine", 
    c("SpatialData", "SpatialData"), 
    \(x, y, ...) combine(list(x, y)))
        