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
NULL

#' @export
#' @rdname combine
#' @importFrom BiocGenerics combine
setMethod("combine", c("SpatialData", "SpatialData"), \(x, y, ...) {
    # ensure element names are unique across objects
    old <- list(unlist(colnames(x)), unlist(colnames(y)))
    idx <- rep.int(c(1, 2), lengths(old))
    new <- split(make.unique(unlist(old)), idx)
    for (i in c(1, 2)) {
        # get input element names
        z <- get(c("x", "y")[i])
        old_nms <- unlist(colnames(z)[.ls])
    
        # find new names for these elements
        j <- match(old_nms, old[[i]])
        new_nms <- new[[i]][j]
        
        # rename elements
        for (l in .ls) {
            j <- match(names(z[[l]]), old[[i]])
            names(z[[l]]) <- new[[i]][j]
        }
        # sync tables
        z <- .sync_tables_sdattrs(z, old_nms, new_nms)
        
        # rename tables themselves
        j <- match(tableNames(z), old[[i]])
        tableNames(z) <- new[[i]][j]
        
        assign(c("x", "y")[i], z)
    }
    SpatialData(
        images=c(x$images, y$images),
        labels=c(x$labels, y$labels),
        points=c(x$points, y$points),
        shapes=c(x$shapes, y$shapes),
        tables=c(x$tables, y$tables))
})
